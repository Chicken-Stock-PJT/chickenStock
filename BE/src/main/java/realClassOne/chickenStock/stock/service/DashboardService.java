package realClassOne.chickenStock.stock.service;

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
    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final JwtTokenProvider jwtTokenProvider;

    /**
     * 회원의 통합 대시보드 정보를 조회합니다.
     * 한 번의 키움증권 API 호출로 모든 종목 정보를 가져옵니다.
     */
    @Transactional(readOnly = true)
    public DashboardResponseDTO getDashboard(String authorizationHeader) {
        // 1. 회원 정보 조회
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // 2. 보유 종목 정보 조회 (active = true인 것만)
        List<HoldingPosition> holdings = holdingPositionRepository.findByMemberWithStockData(member)
                .stream()
                .filter(HoldingPosition::getActive)
                .collect(Collectors.toList());

        // 3. 미체결 주문 정보 조회
        List<PendingOrder> pendingOrders = pendingOrderRepository.findByMemberAndStatus(
                member, PendingOrder.OrderStatus.PENDING);

        // 4. 금일 거래 내역 조회
        LocalDateTime todayStart = LocalDate.now().atStartOfDay();
        List<TradeHistory> todayTrades = tradeHistoryRepository.findTodayTradesByMember(member, todayStart);

        // 5. 보유 종목과 미체결 종목의 현재가 정보 일괄 조회 (키움증권 API)
        Map<String, JsonNode> stockPriceMap = getStockPrices(holdings, pendingOrders);

        // 6. 대시보드 데이터 구성
        return buildDashboardResponse(member, holdings, pendingOrders, todayTrades, stockPriceMap);
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
            return new HashMap<>();
        }

        // List로 변환
        List<String> stockCodeList = new ArrayList<>(allStockCodes);

        // 키움증권 API를 통해 일괄 조회 (최대 100개까지 가능)
        Map<String, JsonNode> stockDataMap = kiwoomStockApiService.getWatchListInfoMap(stockCodeList);

        // API 호출 실패 시 웹소켓 데이터로 보충
        for (String stockCode : stockCodeList) {
            if (!stockDataMap.containsKey(stockCode) || stockDataMap.get(stockCode) == null) {
                JsonNode websocketData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);
                if (websocketData != null) {
                    stockDataMap.put(stockCode, websocketData);
                }
            }
        }

        return stockDataMap;
    }

    /**
     * 대시보드 응답 DTO를 구성합니다.
     */
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
        Long todayProfitLoss = 0L;

        for (HoldingPosition position : holdings) {
            String stockCode = position.getStockData().getShortCode();
            JsonNode priceData = stockPriceMap.get(stockCode);

            // 현재가 추출
            Long currentPrice = extractCurrentPrice(priceData);
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

            // 금일 손익 계산 (금일 거래가 있는 경우)
            todayProfitLoss += calculateTodayProfitLoss(stockCode, todayTrades, currentPrice);

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
        Long pendingOrderAmount = pendingBuyAmount + pendingSellAmount;

        // 총 자산 계산
        Long totalAsset = member.getMemberMoney() + totalValuation + pendingOrderAmount;

        // 총 손익 및 수익률 계산
        Long totalProfitLoss = totalValuation - totalInvestment;
        Double totalReturnRate = totalInvestment > 0
                ? (totalProfitLoss.doubleValue() / totalInvestment.doubleValue()) * 100
                : 0.0;

        // 금일 수익률 계산
        Double todayReturnRate = totalValuation > 0
                ? (todayProfitLoss.doubleValue() / totalValuation.doubleValue()) * 100
                : 0.0;

        return DashboardResponseDTO.builder()
                .memberMoney(member.getMemberMoney())
                .stockValuation(totalValuation)
                .pendingOrderAmount(pendingOrderAmount)
                .totalAsset(totalAsset)
                .totalInvestment(totalInvestment)
                .totalProfitLoss(totalProfitLoss)
                .totalReturnRate(totalReturnRate)
                .todayProfitLoss(todayProfitLoss)
                .todayReturnRate(todayReturnRate)
                .holdingStockCount(holdings.size())
                .holdings(holdingDTOs)
                .updatedAt(LocalDateTime.now())
                .build();
    }

    /**
     * JsonNode에서 현재가를 추출합니다.
     */
    private Long extractCurrentPrice(JsonNode priceData) {
        if (priceData == null) {
            return 0L;
        }

        try {
            String priceStr;
            if (priceData.has("cur_prc")) { // API 응답
                priceStr = priceData.get("cur_prc").asText();
            } else if (priceData.has("10")) { // 웹소켓 응답
                priceStr = priceData.get("10").asText();
            } else {
                return 0L;
            }

            return Long.parseLong(priceStr.replaceAll("[^0-9-]", ""));
        } catch (NumberFormatException e) {
            log.warn("현재가 파싱 오류", e);
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
}