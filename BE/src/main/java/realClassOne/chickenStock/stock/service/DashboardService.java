package realClassOne.chickenStock.stock.service;

import org.springframework.data.redis.core.RedisTemplate;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import com.fasterxml.jackson.databind.JsonNode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.dto.response.DashboardResponseDTO;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.entity.TradeHistory;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;
import realClassOne.chickenStock.stock.repository.PendingOrderRepository;
import realClassOne.chickenStock.stock.repository.TradeHistoryRepository;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class DashboardService {

    private final MemberRepository memberRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final PendingOrderRepository pendingOrderRepository;
    private final TradeHistoryRepository tradeHistoryRepository;
    private final KiwoomStockApiService kiwoomStockApiService;
    private final JwtTokenProvider jwtTokenProvider;

    private final RedisTemplate<String, DashboardResponseDTO> dashboardRedisTemplate;
    private static final long CACHE_TTL_SECONDS = 5; // 캐시 유효 시간: 5초
    private static final Long INITIAL_CAPITAL = 100_000_000L;

    /**
     * 대시보드 데이터를 캐시에서 조회
     */
    public DashboardResponseDTO getCachedDashboard(String cacheKey) {
        try {
            DashboardResponseDTO cached = dashboardRedisTemplate.opsForValue().get(cacheKey);
            if (cached != null) {
                return cached;
            }
        } catch (Exception e) {
            log.error("캐시에서 대시보드 데이터 조회 실패: {}, 오류: {}", cacheKey, e.getMessage());
            // 캐시 조회 실패 시 에러 발생시키지 않고 null 반환
        }
        return null;
    }

    /**
     * 대시보드 데이터를 캐시에 저장
     */
    public void cacheDashboard(String cacheKey, DashboardResponseDTO dashboard) {
        try {
            dashboardRedisTemplate.opsForValue().set(cacheKey, dashboard, CACHE_TTL_SECONDS, TimeUnit.SECONDS);
        } catch (Exception e) {
            log.error("대시보드 데이터 캐싱 실패: {}, 오류: {}", cacheKey, e.getMessage());
            // 캐싱 실패 시 에러 처리하지 않고 계속 진행
        }
    }

    /**
     * 인증 토큰 기반 캐시 키 생성
     */
    public String generateCacheKeyFromToken(String token) {
        return "dashboard:token:" + token;
    }

    /**
     * 회원 ID 기반 캐시 키 생성
     */
    public String generateCacheKeyFromMemberId(Long memberId) {
        return "dashboard:member:" + memberId;
    }

    /**
     * 토큰 기반 회원의 통합 대시보드 정보를 조회합니다.
     * 캐싱 기능을 적용하여 5초 동안 동일한 결과를 반환합니다.
     */
    @Transactional(readOnly = true)
    public DashboardResponseDTO getDashboard(String authorizationHeader) {
        // 토큰에서 회원 ID 추출
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // 캐시 키 생성
        String cacheKey = generateCacheKeyFromToken(token);

        // 캐시에서 조회
        DashboardResponseDTO cachedDashboard = getCachedDashboard(cacheKey);
        if (cachedDashboard != null) {
            return cachedDashboard;
        }

        // 캐시에 없는 경우 새로 조회
        DashboardResponseDTO dashboard = getDashboardByMemberId(memberId);

        // 결과를 캐시에 저장
        cacheDashboard(cacheKey, dashboard);

        return dashboard;
    }

    /**
     * 회원 ID로 대시보드 정보를 조회합니다.
     * 캐싱 기능을 적용하여 5초 동안 동일한 결과를 반환합니다.
     */
    @Transactional(readOnly = true)
    public DashboardResponseDTO getDashboardByMemberId(Long memberId) {
        // 캐시 키 생성
        String cacheKey = generateCacheKeyFromMemberId(memberId);

        // 캐시에서 조회
        DashboardResponseDTO cachedDashboard = getCachedDashboard(cacheKey);
        if (cachedDashboard != null) {
            return cachedDashboard;
        }

        // 회원 정보 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // 보유 종목 정보 조회 (active = true인 것만)
        List<HoldingPosition> holdings = holdingPositionRepository.findByMemberWithStockData(member)
                .stream()
                .filter(HoldingPosition::getActive)
                .collect(Collectors.toList());

        // 미체결 주문 정보 조회
        List<PendingOrder> pendingOrders = pendingOrderRepository.findByMemberAndStatus(
                member, PendingOrder.OrderStatus.PENDING);

        // 금일 거래 내역 조회
        LocalDateTime todayStart = LocalDate.now().atStartOfDay();
        List<TradeHistory> todayTrades = tradeHistoryRepository.findTodayTradesByMember(member, todayStart);

        // 보유 종목과 미체결 종목의 현재가 정보 일괄 조회 (키움증권 API)
        Map<String, JsonNode> stockPriceMap = getStockPrices(holdings, pendingOrders);

        // 대시보드 데이터 구성
        DashboardResponseDTO dashboard = buildDashboardResponse(member, holdings, pendingOrders, todayTrades, stockPriceMap);

        // 결과를 캐시에 저장
        cacheDashboard(cacheKey, dashboard);

        return dashboard;
    }

    /**
     * 보유 종목과 미체결 종목들의 현재가 정보를 일괄 조회합니다.
     * 관심종목정보요청(ka10095) API를 사용하여 한 번의 호출로 처리합니다.
     */
    private Map<String, JsonNode> getStockPrices(List<HoldingPosition> holdings, List<PendingOrder> pendingOrders) {
        // 종목 코드 수집 (보유 종목 + 미체결 종목)
        Set<String> allStockCodes = new HashSet<>();

        // 보유 종목 코드 추가
        holdings.forEach(position ->
                allStockCodes.add(position.getStockData().getShortCode()));

        // 미체결 종목 코드 추가
        pendingOrders.forEach(order ->
                allStockCodes.add(order.getStockData().getShortCode()));

        if (allStockCodes.isEmpty()) {
            log.info("조회할 종목이 없습니다.");
            return new HashMap<>();
        }

        // List로 변환
        List<String> stockCodeList = new ArrayList<>(allStockCodes);

        // 키움증권 REST API를 통해 일괄 조회 (최대 100개까지 가능)
        Map<String, JsonNode> stockDataMap = kiwoomStockApiService.getWatchListInfoMap(stockCodeList);


        // "_AL" 접미사를 제거한 맵 생성
        Map<String, JsonNode> cleanStockDataMap = new HashMap<>();
        for (Map.Entry<String, JsonNode> entry : stockDataMap.entrySet()) {
            String cleanCode = entry.getKey().replace("_AL", "");
            cleanStockDataMap.put(cleanCode, entry.getValue());
        }

        if (cleanStockDataMap.isEmpty()) {
            log.error("키움증권 REST API에서 종목 정보를 가져올 수 없습니다.");
            throw new CustomException(StockErrorCode.API_REQUEST_FAILED,
                    "종목 정보를 조회할 수 없습니다.");
        }


        return cleanStockDataMap;
    }

    private DashboardResponseDTO buildDashboardResponse(
            Member member,
            List<HoldingPosition> holdings,
            List<PendingOrder> pendingOrders,
            List<TradeHistory> todayTrades,
            Map<String, JsonNode> stockPriceMap) {

        // 보유 종목별 계산 및 DTO 생성
        List<DashboardResponseDTO.StockHoldingDTO> holdingDTOs = new ArrayList<>();
        Long totalInvestment = 0L;
        Long totalValuation = 0L;

        for (HoldingPosition position : holdings) {
            String stockCode = position.getStockData().getShortCode();
            JsonNode priceData = stockPriceMap.get(stockCode);

            // 현재가 추출
            Long currentPrice = extractCurrentPrice(priceData);

            // 현재가가 0이면 평균 매입가를 사용
            if (currentPrice == 0L) {
                currentPrice = position.getAveragePrice();
            }

            String priceChange = extractPriceChange(priceData);
            String changeRate = extractChangeRate(priceData);

            // 금액 계산
            Long investmentAmount = position.getAveragePrice() * position.getQuantity();
            Long valuationAmount = currentPrice * position.getQuantity();
            Long profitLoss = valuationAmount - investmentAmount;
            Double returnRate = investmentAmount > 0
                    ? (profitLoss.doubleValue() / investmentAmount.doubleValue()) * 100
                    : 0.0;

            totalInvestment += investmentAmount;
            totalValuation += valuationAmount;

            DashboardResponseDTO.StockHoldingDTO holdingDTO = DashboardResponseDTO.StockHoldingDTO.builder()
                    .stockCode(stockCode)
                    .stockName(position.getStockData().getShortName())
                    .quantity(position.getQuantity())
                    .averagePrice(position.getAveragePrice())
                    .currentPrice(currentPrice)
                    .valuationAmount(valuationAmount)
                    .profitLoss(profitLoss)
                    .returnRate(returnRate)
                    .priceChange(priceChange)
                    .changeRate(changeRate)
                    .build();

            holdingDTOs.add(holdingDTO);
        }

        // 미체결 금액 계산
        Long pendingBuyAmount = calculatePendingBuyAmount(pendingOrders);
        Long pendingSellAmount = calculatePendingSellAmount(pendingOrders, stockPriceMap);

        // 현금 = 순 현금성 자산 + 지정가 매수 대기중인 금액
        Long cash = member.getMemberMoney() + pendingBuyAmount;

        // 총 평가금액 = 주식 평가금액
        Long totalValuationAmount = totalValuation;

        // 총 자산 = 현금 + 총 평가금액
        Long totalAsset = cash + totalValuationAmount;

        // 초기 자본금(1억) 기준 총 손익 계산
        Long totalProfitLoss = totalAsset - INITIAL_CAPITAL;

        // 초기 자본금(1억) 기준 총 수익률 계산
        Double totalReturnRate = (totalProfitLoss.doubleValue() / INITIAL_CAPITAL.doubleValue()) * 100;

        // 당일 실현 손익 계산
        Long todayProfitLoss = calculateTodayRealizedProfitLoss(todayTrades);

        // 당일 매매 규모 계산 (당일 매도 주식의 총 매수 비용)
        Long todayTradeAmount = calculateTodayTradeAmount(todayTrades);

        // 당일 수익률 계산 (당일 실현 손익 / 당일 매매 규모)
        Double todayReturnRate = todayTradeAmount > 0
                ? (todayProfitLoss.doubleValue() / todayTradeAmount.doubleValue()) * 100
                : 0.0;

        java.time.format.DateTimeFormatter formatter = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        String formattedDateTime = LocalDateTime.now().format(formatter);

        return DashboardResponseDTO.builder()
                .memberMoney(member.getMemberMoney())  // 예수금 잔고 (순 현금성 자산)
                .cash(cash)                            // 현금 (순 현금성 자산 + 지정가 매수 대기중인 금액)
                .stockValuation(totalValuation)        // 보유 주식 평가금액
                .totalValuationAmount(totalValuationAmount)  // 총 평가금액 (= 보유 주식 평가금액)
                .pendingOrderAmount(pendingBuyAmount)  // 지정가 매수 대기중인 금액
                .pendingSellAmount(pendingSellAmount)  // 지정가 매도 대기중인 금액 (정보 제공용)
                .totalAsset(totalAsset)                // 총 자산
                .totalInvestment(totalInvestment)      // 총 투자금액
                .totalProfitLoss(totalProfitLoss)      // 총 손익
                .totalReturnRate(totalReturnRate)      // 총 수익률
                .todayProfitLoss(todayProfitLoss)      // 금일 수익
                .todayReturnRate(todayReturnRate)      // 금일 수익률
                .todayTradeAmount(todayTradeAmount)    // 금일 매매 규모
                .holdingStockCount(holdings.size())    // 보유 종목 수
                .holdings(holdingDTOs)                 // 보유 종목 상세 정보
                .updatedAt(formattedDateTime)          // 업데이트 시간
                .build();
    }

    /**
     * 당일 실현 손익을 계산합니다.
     */
    private Long calculateTodayRealizedProfitLoss(List<TradeHistory> todayTrades) {
        Long todayProfitLoss = 0L;

        for (TradeHistory trade : todayTrades) {
            if (trade.getTradeType() == TradeHistory.TradeType.SELL) {
                // 매도 거래의 실제 손익 = 매도 금액 - 매수 비용 - 수수료 - 세금
                // 매도 금액 (총 거래 금액)
                Long sellAmount = trade.getTotalPrice();

                // 이 매도 거래에 해당하는 평균 매입가를 기반으로 매수 비용 계산
                // 이는 홀딩 포지션의 평균 매입가로부터 직접 계산하는 것이 아니라
                // 해당 거래가 발생했을 때의 실제 매수 내역을 기반으로 계산해야 함
                Long buyAmount = calculateBuyAmountForSellTrade(trade);

                // 실현 손익 = 매도 금액 - 매수 비용 - 수수료 - 세금
                Long realizedProfit = sellAmount - buyAmount - trade.getFee() - trade.getTax();

                todayProfitLoss += realizedProfit;

                // 로깅 추가 (디버깅 용도)
                log.debug("매도 거래 ID: {}, 매도 금액: {}, 매수 비용: {}, 수수료: {}, 세금: {}, 실현 손익: {}",
                        trade.getTradeHistoryId(), sellAmount, buyAmount, trade.getFee(), trade.getTax(), realizedProfit);
            }
        }

        return todayProfitLoss;
    }

    /**
     * 매도 거래에 대한 실제 매수 비용을 계산합니다.
     * FIFO(선입선출) 방식을 사용하여 보다 정확한 매수 비용을 산출합니다.
     */
    private Long calculateBuyAmountForSellTrade(TradeHistory sellTrade) {
        Member member = sellTrade.getMember();
        StockData stockData = sellTrade.getStockData();
        LocalDateTime sellTime = sellTrade.getTradedAt();
        int sellQuantity = sellTrade.getQuantity();

        // 매도 거래 이전의 모든 관련 거래 내역을 시간 순으로 조회 (오래된 것부터)
        List<TradeHistory> previousTrades = tradeHistoryRepository.findByMemberAndStockDataAndTradedAtBeforeOrderByTradedAtAsc(
                member, stockData, sellTime);

        if (previousTrades.isEmpty()) {
            log.warn("매도 거래({})에 대한 이전 매수 내역이 없습니다.", sellTrade.getTradeHistoryId());
            return 0L;
        }

        // FIFO 방식으로 매수 비용 계산
        long totalBuyAmount = 0L;
        int remainingSellQuantity = sellQuantity;

        // 첫 매수부터 순서대로 처리 (FIFO)
        for (TradeHistory history : previousTrades) {
            if (remainingSellQuantity <= 0) {
                break; // 모든 매도 수량에 대한 매수 비용을 계산했으면 종료
            }

            if (history.getTradeType() == TradeHistory.TradeType.BUY) {
                // 이 매수 거래에서 사용 가능한 수량 계산
                int availableBuyQuantity = history.getQuantity();

                // 이전 매도 거래로 이미 매도된 수량 차감 (같은 매수 건에 대해)
                for (TradeHistory prevSell : previousTrades) {
                    if (prevSell.getTradeType() == TradeHistory.TradeType.SELL
                            && prevSell.getTradedAt().isAfter(history.getTradedAt())
                            && prevSell.getTradedAt().isBefore(sellTime)) {
                        availableBuyQuantity -= calculateBuyQuantityUsedForSellTrade(prevSell, history, previousTrades);
                    }
                }

                if (availableBuyQuantity <= 0) {
                    continue; // 이 매수 거래의 모든 수량이 이미 매도되었으면 다음 거래로
                }

                // 현재 매도에 적용할 수량 계산
                int quantityToUse = Math.min(availableBuyQuantity, remainingSellQuantity);

                // 해당 수량에 대한 매수 비용 계산 및 추가
                long buyAmountPerShare = history.getUnitPrice();
                totalBuyAmount += buyAmountPerShare * quantityToUse;

                // 남은 매도 수량 업데이트
                remainingSellQuantity -= quantityToUse;

                log.debug("매수 거래 ID: {}, 단가: {}, 사용 수량: {}, 누적 매수 비용: {}, 남은 매도 수량: {}",
                        history.getTradeHistoryId(), buyAmountPerShare, quantityToUse, totalBuyAmount, remainingSellQuantity);
            }
        }

        // 모든 수량에 대한 매수 비용을 찾지 못한 경우 (비정상 케이스)
        if (remainingSellQuantity > 0) {
            log.warn("매도 수량({})에 대한 충분한 매수 내역을 찾지 못했습니다. 부족한 수량: {}",
                    sellQuantity, remainingSellQuantity);

            // 대안: 평균 매입가를 사용하여 나머지 수량에 대한 비용 추정
            HoldingPosition position = holdingPositionRepository.findByMemberAndStockDataAndActiveTrue(member, stockData)
                    .orElse(null);
            if (position != null) {
                totalBuyAmount += position.getAveragePrice() * remainingSellQuantity;
                log.debug("부족한 수량에 대해 현재 평균 매입가({})를 사용하여 비용 추정", position.getAveragePrice());
            }
        }

        return totalBuyAmount;
    }

    /**
     * 특정 매도 거래에서 특정 매수 거래의 수량이 얼마나 사용되었는지 계산합니다.
     * FIFO 방식을 고려하여 정확한 수량을 계산합니다.
     */
    private int calculateBuyQuantityUsedForSellTrade(TradeHistory sellTrade, TradeHistory buyTrade, List<TradeHistory> allTrades) {
        // 이 매수 거래 이전의 모든 매수 거래들의 총 수량
        int totalBuyQuantityBeforeThis = 0;
        for (TradeHistory trade : allTrades) {
            if (trade.getTradeType() == TradeHistory.TradeType.BUY
                    && trade.getTradedAt().isBefore(buyTrade.getTradedAt())) {
                totalBuyQuantityBeforeThis += trade.getQuantity();
            }
        }

        // 이 매수 거래 이전의 매수 거래들이 처리한 총 매도 수량
        int totalSellQuantityProcessedByPreviousBuys = 0;

        // 이 매수 거래 자체의 수량
        int thisBuyQuantity = buyTrade.getQuantity();

        // 매도 거래의 총 수량이 이 매수 거래 이전의 모든 매수 수량보다 크면,
        // 이 매수 거래에서 일부 수량이 사용된 것
        if (sellTrade.getQuantity() > totalBuyQuantityBeforeThis) {
            // 이 매수 거래에서 사용된 최대 가능 수량은 매도 수량 - 이전 매수 거래들의 총 수량
            int maxPossibleQuantityFromThisBuy = sellTrade.getQuantity() - totalBuyQuantityBeforeThis;
            // 실제 사용된 수량은 이 매수 거래의 수량과 최대 가능 수량 중 작은 값
            return Math.min(thisBuyQuantity, maxPossibleQuantityFromThisBuy);
        }

        // 이 매수 거래 이전의 매수 거래들로 충분히 처리 가능하면 0 반환
        return 0;
    }


    /**
     * JsonNode에서 현재가를 추출합니다.
     * 키움증권 관심종목정보요청(ka10095) API 응답 형식에 맞게 파싱합니다.
     */
    private Long extractCurrentPrice(JsonNode priceData) {
        if (priceData == null) {
            log.warn("가격 데이터가 null입니다.");
            return 0L;
        }

        try {
            // 키움증권 REST API 응답 형식
            if (priceData.has("cur_prc")) {
                String priceStr = priceData.get("cur_prc").asText();
                // 부호와 쉼표를 제거하고 숫자만 추출
                priceStr = priceStr.replaceAll("[^0-9]", "");

                if (priceStr.isEmpty()) {
                    return 0L;
                }

                return Long.parseLong(priceStr);
            } else {
                log.error("현재가(cur_prc) 필드를 찾을 수 없습니다. 사용 가능한 필드: {}",
                        priceData.fieldNames());
                return 0L;
            }
        } catch (Exception e) {
            log.error("현재가 파싱 오류: {}", e.getMessage(), e);
            return 0L;
        }
    }

    /**
     * JsonNode에서 전일대비를 추출합니다.
     */
    private String extractPriceChange(JsonNode priceData) {
        if (priceData == null) {
            return "0";
        }

        if (priceData.has("pred_pre")) {
            return priceData.get("pred_pre").asText();
        }
        return "0";
    }

    /**
     * JsonNode에서 등락률을 추출합니다.
     */
    private String extractChangeRate(JsonNode priceData) {
        if (priceData == null) {
            return "0.00";
        }

        if (priceData.has("flu_rt")) {
            return priceData.get("flu_rt").asText();
        }
        return "0.00";
    }

    /**
     * 금일 거래에 대한 손익을 계산합니다.
     */
    private Long calculateTodayProfitLoss(String stockCode, List<TradeHistory> todayTrades, Long currentPrice) {
        Long todayProfitLoss = 0L;

        for (TradeHistory trade : todayTrades) {
            if (trade.getStockData().getShortCode().equals(stockCode)) {
                if (trade.getTradeType() == TradeHistory.TradeType.SELL) {
                    // 매도한 경우: 매도가와 현재가의 차이
                    Long sellPrice = trade.getUnitPrice();
                    Long priceDiff = sellPrice - currentPrice;
                    todayProfitLoss += priceDiff * trade.getQuantity();
                }
                // 매수한 경우는 금일 손익에 포함하지 않음 (당일 실현 손익만 계산)
            }
        }

        return todayProfitLoss;
    }

    /**
     * 미체결 매수 주문의 총 금액을 계산합니다.
     */
    private Long calculatePendingBuyAmount(List<PendingOrder> pendingOrders) {
        return pendingOrders.stream()
                .filter(order -> order.getOrderType() == TradeHistory.TradeType.BUY)
                .mapToLong(order -> order.getTargetPrice() * order.getQuantity())
                .sum();
    }

    /**
     * 미체결 매도 주문의 총 평가금액을 계산합니다.
     */
    private Long calculatePendingSellAmount(List<PendingOrder> pendingOrders, Map<String, JsonNode> stockPriceMap) {
        Long totalAmount = 0L;

        List<PendingOrder> sellOrders = pendingOrders.stream()
                .filter(order -> order.getOrderType() == TradeHistory.TradeType.SELL)
                .collect(Collectors.toList());

        for (PendingOrder order : sellOrders) {
            String stockCode = order.getStockData().getShortCode();
            JsonNode priceData = stockPriceMap.get(stockCode);
            Long currentPrice = extractCurrentPrice(priceData);
            totalAmount += currentPrice * order.getQuantity();
        }

        return totalAmount;
    }

    /**
     * 당일 매매 규모(매도 주식의 매수 비용 합계)를 계산합니다.
     */
    private Long calculateTodayTradeAmount(List<TradeHistory> todayTrades) {
        Long todayTradeAmount = 0L;

        for (TradeHistory trade : todayTrades) {
            if (trade.getTradeType() == TradeHistory.TradeType.SELL) {
                // 이 매도 거래에 대한 매수 비용 계산
                Long buyAmount = calculateBuyAmountForSellTrade(trade);
                todayTradeAmount += buyAmount;
            }
        }

        return todayTradeAmount;
    }
}