package realClassOne.chickenStock.stock.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.notification.service.NotificationService;
import realClassOne.chickenStock.stock.dto.common.ChartDataDTO;
import realClassOne.chickenStock.stock.dto.request.ChartRequestDTO;
import realClassOne.chickenStock.stock.dto.response.ChartResponseDTO;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.entity.TradeHistory;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;
import realClassOne.chickenStock.stock.repository.PendingOrderRepository;
import realClassOne.chickenStock.stock.repository.StockDataRepository;
import realClassOne.chickenStock.stock.repository.TradeHistoryRepository;
import realClassOne.chickenStock.stock.websocket.handler.PortfolioWebSocketHandler;
import realClassOne.chickenStock.stock.websocket.handler.StockWebSocketHandler;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Service
@Slf4j
@RequiredArgsConstructor
public class LimitOrderExecutionService {

    private final StockChartService stockChartService;
    private final PendingOrderRepository pendingOrderRepository;
    private final TradeHistoryRepository tradeHistoryRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final MemberRepository memberRepository;
    private final StockDataRepository stockDataRepository;
    private final ObjectMapper objectMapper;
    private final RedisTemplate<String, Object> redisTemplate;
    private final PortfolioWebSocketHandler portfolioWebSocketHandler;
    private final FeeTaxService feeTaxService;
    private final StockWebSocketHandler stockWebSocketHandler;
    private final NotificationService notificationService;

    // API 호출 제한 관리를 위한 스케줄러
    private final ScheduledExecutorService apiScheduler = Executors.newScheduledThreadPool(1);

    // 현재 시간 슬롯의 API 호출 횟수 추적
    private final AtomicInteger currentSecondCalls = new AtomicInteger(0);
    private volatile long currentSecond = System.currentTimeMillis() / 1000;

    // 1초에 3회 제한
    private static final int MAX_CALLS_PER_SECOND = 3;

    // 차트 데이터 캐시 (종목코드 -> 차트 데이터)
    private final Map<String, CandleData> chartCache = new ConcurrentHashMap<>();

    // 캔들 데이터 클래스
    private static class CandleData {
        String stockCode;
        Long high;
        Long low;
        String timestamp;
        LocalDateTime fetchedAt;

        CandleData(String stockCode, Long high, Long low, String timestamp) {
            this.stockCode = stockCode;
            this.high = high;
            this.low = low;
            this.timestamp = timestamp;
            this.fetchedAt = LocalDateTime.now();
        }

        boolean isValid() {
            // 2분 이내의 데이터만 유효한 것으로 간주
            return LocalDateTime.now().isBefore(fetchedAt.plusMinutes(2));
        }
    }

    /**
     * 1분마다 실행되는 지정가 주문 체결 스케줄러
     * 직전 1분간의 캔들 데이터를 기반으로 지정가 주문을 체결합니다.
     */
    @Scheduled(cron = "0 * * * * *") // 매분 0초에 실행
    public void executePendingLimitOrders() {
        try {
            log.info("지정가 주문 체결 스케줄러 시작 - {}", LocalDateTime.now());

            // 1. 현재 PENDING 상태인 지정가 주문들을 가져옴
            List<PendingOrder> pendingOrders = pendingOrderRepository.findByStatusOrderByCreatedAtAsc(
                    PendingOrder.OrderStatus.PENDING);

            if (pendingOrders.isEmpty()) {
                log.debug("처리할 지정가 주문이 없습니다.");
                return;
            }

            log.info("처리 대상 지정가 주문 수: {}", pendingOrders.size());

            // 2. 종목코드별로 주문을 그룹화
            Map<String, List<PendingOrder>> ordersByStock = pendingOrders.stream()
                    .collect(Collectors.groupingBy(order -> order.getStockData().getShortCode()));

            // 3. 각 종목별로 최적화된 API 호출
            processOrdersWithRateLimit(ordersByStock);

        } catch (Exception e) {
            log.error("지정가 주문 체결 스케줄러 오류", e);
        }
    }

    /**
     * API 호출 제한을 고려하여 주문을 처리합니다.
     */
    private void processOrdersWithRateLimit(Map<String, List<PendingOrder>> ordersByStock) {
        List<String> stockCodes = new ArrayList<>(ordersByStock.keySet());
        int totalStocks = stockCodes.size();

        log.info("처리할 종목 수: {}, 주문 수: {}", totalStocks,
                ordersByStock.values().stream().mapToInt(List::size).sum());

        // 종목 코드를 배치로 나누어 처리
        List<CompletableFuture<Void>> futures = new ArrayList<>();

        for (String stockCode : stockCodes) {
            CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
                try {
                    // API 호출 전 1초당 3회 제한 확인
                    waitForApiLimit();

                    // 캐시 확인
                    CandleData cachedData = chartCache.get(stockCode);
                    if (cachedData != null && cachedData.isValid()) {
                        log.debug("캐시된 차트 데이터 사용: {}", stockCode);
                        processOrdersForStock(stockCode, ordersByStock.get(stockCode), cachedData);
                    } else {
                        // 차트 데이터 조회
                        CandleData candleData = fetchLatestCandleData(stockCode);
                        if (candleData != null) {
                            chartCache.put(stockCode, candleData);
                            processOrdersForStock(stockCode, ordersByStock.get(stockCode), candleData);
                        }
                    }
                } catch (Exception e) {
                    log.error("종목 {} 처리 중 오류", stockCode, e);
                }
            }, apiScheduler);

            futures.add(future);
        }

        // 모든 작업 완료 대기 (최대 50초)
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                    .get(50, TimeUnit.SECONDS);
        } catch (Exception e) {
            log.error("주문 처리 중 타임아웃 또는 오류", e);
        }

        log.info("지정가 주문 처리 완료");
    }

    /**
     * API 호출 제한을 위한 대기 메서드
     */
    private void waitForApiLimit() throws InterruptedException {
        while (true) {
            long now = System.currentTimeMillis() / 1000;

            // 새로운 초가 시작되면 카운터 리셋
            if (now > currentSecond) {
                currentSecond = now;
                currentSecondCalls.set(0);
            }

            // 현재 초에 여유가 있으면 호출
            if (currentSecondCalls.get() < MAX_CALLS_PER_SECOND) {
                currentSecondCalls.incrementAndGet();
                break;
            }

            // 다음 초까지 대기
            Thread.sleep(100);
        }
    }

    /**
     * 특정 종목의 최신 1분봉 데이터를 가져옵니다.
     */
    private CandleData fetchLatestCandleData(String stockCode) {
        try {
            // 직전 1분 데이터를 가져오기 위한 설정
            String currentTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd"));

            ChartRequestDTO request = ChartRequestDTO.builder()
                    .chartType("MINUTE")
                    .stockCode(stockCode)
                    .baseDate(currentTime)
                    .timeInterval("1")  // 1분봉
                    .modifiedPriceType("1")
                    .contYn("N")
                    .build();

            ChartResponseDTO response = stockChartService.getStockChart(request);

            if (response.getCode() == 0 && response.getChartData() != null && !response.getChartData().isEmpty()) {
                // 가장 최근 완성된 봉 데이터 (직전 1분)
                ChartDataDTO latestCandle = response.getChartData().get(0);

                // 가격 데이터 파싱
                Long high = parsePriceValue(latestCandle.getHighPrice());
                Long low = parsePriceValue(latestCandle.getLowPrice());
                String timestamp = latestCandle.getDate();

                log.info("종목 {} 1분봉 데이터 조회 성공 - 고가: {}, 저가: {}, 시간: {}",
                        stockCode, high, low, timestamp);

                return new CandleData(stockCode, high, low, timestamp);
            }

            log.warn("종목 {} 1분봉 데이터 조회 실패 또는 데이터 없음", stockCode);
            return null;

        } catch (Exception e) {
            log.error("종목 {} 차트 데이터 조회 중 오류", stockCode, e);
            return null;
        }
    }

    /**
     * 가격 문자열을 Long 값으로 파싱합니다.
     */
    private Long parsePriceValue(String priceStr) {
        if (priceStr == null || priceStr.isEmpty()) {
            return 0L;
        }

        // 부호 및 쉼표 제거
        String cleanedPrice = priceStr.replace(",", "")
                .replace("+", "")
                .replace("-", "")
                .trim();

        try {
            return Long.parseLong(cleanedPrice);
        } catch (NumberFormatException e) {
            log.warn("가격 파싱 실패: {}", priceStr);
            return 0L;
        }
    }

    /**
     * 특정 종목의 지정가 주문들을 처리합니다.
     */
    @Transactional
    public void processOrdersForStock(String stockCode, List<PendingOrder> orders, CandleData candleData) {
        try {
            log.info("종목 {} 지정가 주문 처리 시작 - 주문 수: {}, 고가: {}, 저가: {}",
                    stockCode, orders.size(), candleData.high, candleData.low);

            int executedCount = 0;

            for (PendingOrder order : orders) {
                if (shouldExecuteOrderEnhanced(order, candleData)) {
                    Long executionPrice = determineExecutionPrice(order, candleData);
                    executeLimitOrder(order, executionPrice);
                    executedCount++;
                }
            }

            log.info("종목 {} 지정가 주문 처리 완료 - 체결: {}/{}", stockCode, executedCount, orders.size());

        } catch (Exception e) {
            log.error("종목 {} 주문 처리 중 오류", stockCode, e);
        }
    }

    /**
     * 확장된 주문 체결 조건 판단
     * 분봉 범위를 벗어난 주문도 체결 가능
     */
    private boolean shouldExecuteOrderEnhanced(PendingOrder order, CandleData candleData) {
        Long targetPrice = order.getTargetPrice();

        if (order.getOrderType() == TradeHistory.TradeType.BUY) {
            // 매수 주문: 지정가가 캔들 저가 이상이면 체결
            boolean shouldExecute = targetPrice >= candleData.low;

            if (shouldExecute) {
                log.info("매수 주문 체결 조건 충족 - ID: {}, 지정가: {}, 캔들 범위: [{} ~ {}]",
                        order.getOrderId(), targetPrice, candleData.low, candleData.high);
            }

            return shouldExecute;
        } else {
            // 매도 주문: 지정가가 캔들 고가 이하면 체결
            boolean shouldExecute = targetPrice <= candleData.high;

            if (shouldExecute) {
                log.info("매도 주문 체결 조건 충족 - ID: {}, 지정가: {}, 캔들 범위: [{} ~ {}]",
                        order.getOrderId(), targetPrice, candleData.low, candleData.high);
            }

            return shouldExecute;
        }
    }

    /**
     * 체결가 결정 로직
     * 분봉 범위를 벗어난 지정가의 경우 분봉 고가/저가로 체결
     */
    private Long determineExecutionPrice(PendingOrder order, CandleData candleData) {
        Long targetPrice = order.getTargetPrice();

        if (order.getOrderType() == TradeHistory.TradeType.BUY) {
            // 매수: 지정가가 분봉 고가보다 높으면 고가로 체결
            if (targetPrice > candleData.high) {
                log.info("매수 체결가 조정: 지정가 {} -> 분봉 고가 {}", targetPrice, candleData.high);
                return candleData.high;
            }
            // 지정가가 분봉 저가보다 낮으면 저가로 체결
            else if (targetPrice < candleData.low) {
                log.info("매수 체결가 조정: 지정가 {} -> 분봉 저가 {}", targetPrice, candleData.low);
                return candleData.low;
            }
            // 분봉 범위 내면 지정가로 체결
            return targetPrice;
        } else {
            // 매도: 지정가가 분봉 저가보다 낮으면 저가로 체결
            if (targetPrice < candleData.low) {
                log.info("매도 체결가 조정: 지정가 {} -> 분봉 저가 {}", targetPrice, candleData.low);
                return candleData.low;
            }
            // 지정가가 분봉 고가보다 높으면 고가로 체결
            else if (targetPrice > candleData.high) {
                log.info("매도 체결가 조정: 지정가 {} -> 분봉 고가 {}", targetPrice, candleData.high);
                return candleData.high;
            }
            // 분봉 범위 내면 지정가로 체결
            return targetPrice;
        }
    }

    /**
     * 지정가 주문을 실제로 체결합니다.
     */
    @Transactional
    protected void executeLimitOrder(PendingOrder order, Long executionPrice) {
        try {
            log.info("지정가 주문 체결 시작 - ID: {}, 종목: {}, 타입: {}, 수량: {}, 체결가: {} (원래 지정가: {})",
                    order.getOrderId(), order.getStockData().getShortCode(),
                    order.getOrderType(), order.getQuantity(), executionPrice, order.getTargetPrice());

            Member member = memberRepository.findById(order.getMember().getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            StockData stockData = order.getStockData();
            Integer quantity = order.getQuantity();

            if (order.getOrderType() == TradeHistory.TradeType.BUY) {
                processBuyOrder(order, member, stockData, quantity, executionPrice);
            } else {
                processSellOrder(order, member, stockData, quantity, executionPrice);
            }

            // 주문 상태를 완료로 변경
            order.complete();
            pendingOrderRepository.save(order);

            // 포트폴리오 실시간 업데이트 알림
            portfolioWebSocketHandler.sendFullPortfolioUpdate(member.getMemberId());

            // 체결 정보 웹소켓 전송
            try {
                String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("HHmmss"));
                stockWebSocketHandler.broadcastTradeExecution(
                        stockData.getShortCode(),
                        order.getOrderType() == TradeHistory.TradeType.BUY ? "BUY" : "SELL",
                        quantity,
                        executionPrice,
                        executionPrice * quantity,
                        timestamp
                );
            } catch (Exception e) {
                log.warn("체결 정보 웹소켓 전송 실패: {}", e.getMessage());
            }

            // FCM 알림 전송
            try {
                log.info("FCM 체결 알림 전송 시작");
                notificationService.createTradeFCMNotification(
                        member.getMemberId(),
                        stockData.getShortName(),
                        order.getOrderType() == TradeHistory.TradeType.BUY ? "BUY" : "SELL",
                        quantity,
                        executionPrice
                );
                log.info("FCM 체결 알림 전송 완료");
            } catch (Exception e) {
                log.error("FCM 체결 알림 전송 중 오류 발생", e);
            }

            try {
                log.info("체결 알림 생성 시작: 회원ID={}, 종목={}, 타입={}, 수량={}, 가격={}",
                        member.getMemberId(), stockData.getShortName(),
                        order.getOrderType() == TradeHistory.TradeType.BUY ? "BUY" : "SELL",
                        quantity, executionPrice);

                notificationService.createTradeNotification(
                        member.getMemberId(),
                        stockData.getShortName(),
                        order.getOrderType() == TradeHistory.TradeType.BUY ? "BUY" : "SELL",
                        quantity,
                        executionPrice
                );

                log.info("체결 알림 생성 완료");
            } catch (Exception e) {
                log.error("체결 알림 생성 중 오류 발생", e);
            }

        } catch (Exception e) {
            log.error("지정가 주문 체결 실패 - ID: {}", order.getOrderId(), e);
            order.fail();
            pendingOrderRepository.save(order);
            throw e;
        }
    }

    /**
     * 매수 주문 처리
     */
    private void processBuyOrder(PendingOrder order, Member member, StockData stockData,
                                 Integer quantity, Long executionPrice) {
        // 실제 체결 금액 계산
        Long totalPrice = executionPrice * quantity;

        // 실제 체결가 기준으로 수수료 재계산
        Long actualFee = feeTaxService.calculateBuyFee(totalPrice);

        // 예약된 금액과 실제 필요 금액 차이 계산
        Long reservedAmount = order.getTargetPrice() * quantity + order.getReservedFee();
        Long actualAmount = totalPrice + actualFee;
        Long difference = reservedAmount - actualAmount;

        // 차액이 있으면 환불 처리
        if (difference > 0) {
            // 환불 처리
            Long newBalance = member.getMemberMoney() + difference;
            member.updateMemberMoney(newBalance);
            memberRepository.save(member);
            log.info("매수 체결가 차이로 인한 환불: {} 원", difference);
        } else if (difference < 0) {
            // 추가 차감이 필요한 경우 (실제로는 거의 발생하지 않음)
            Long additionalAmount = Math.abs(difference);
            if (member.getMemberMoney() < additionalAmount) {
                throw new CustomException(StockErrorCode.INSUFFICIENT_FUNDS,
                        "체결 시 추가 수수료로 인한 잔액 부족");
            }
            Long newBalance = member.getMemberMoney() - additionalAmount;
            member.updateMemberMoney(newBalance);
            memberRepository.save(member);
            log.info("매수 체결가 차이로 인한 추가 차감: {} 원", additionalAmount);
        }

        // 거래 내역 생성
        TradeHistory tradeHistory = TradeHistory.of(
                member, stockData, TradeHistory.TradeType.BUY,
                quantity, executionPrice, totalPrice,
                actualFee, 0L, LocalDateTime.now()
        );
        tradeHistoryRepository.save(tradeHistory);

        // 보유 포지션 업데이트
        updateHoldingPosition(member, stockData, quantity, executionPrice, true);
    }

    /**
     * 매도 주문 처리
     */
    private void processSellOrder(PendingOrder order, Member member, StockData stockData,
                                  Integer quantity, Long executionPrice) {
        // 총 거래 금액
        Long totalPrice = executionPrice * quantity;

        // 보유 포지션 확인
        HoldingPosition position = holdingPositionRepository
                .findByMemberAndStockDataAndActiveTrue(member, stockData)
                .orElseThrow(() -> new CustomException(StockErrorCode.NO_HOLDING_POSITION,
                        "보유하지 않은 종목입니다: " + stockData.getShortCode()));

        if (position.getQuantity() < quantity) {
            throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK_QUANTITY,
                    "보유 수량 부족: 보유 " + position.getQuantity() + ", 요청 " + quantity);
        }

        // 실제 체결가 기준으로 수수료와 세금 재계산
        Long actualFee = feeTaxService.calculateSellFee(totalPrice);
        Long actualTax = feeTaxService.calculateSellTax(totalPrice);

        // 거래 내역 생성
        TradeHistory tradeHistory = TradeHistory.of(
                member, stockData, TradeHistory.TradeType.SELL,
                quantity, executionPrice, totalPrice,
                actualFee, actualTax, LocalDateTime.now()
        );
        tradeHistoryRepository.save(tradeHistory);

        // 회원 잔액 증가 (실제 계산된 수수료와 세금 차감)
        Long netAmount = totalPrice - actualFee - actualTax;
        Long newBalance = member.getMemberMoney() + netAmount;
        member.updateMemberMoney(newBalance);
        memberRepository.save(member);

        // 보유 포지션 업데이트
        updateHoldingPosition(member, stockData, quantity, executionPrice, false);
    }

    /**
     * 보유 포지션을 업데이트합니다.
     */
    private void updateHoldingPosition(Member member, StockData stockData,
                                       Integer quantity, Long price, boolean isBuy) {
        Optional<HoldingPosition> existingPosition = holdingPositionRepository
                .findByMemberAndStockDataAndActiveTrue(member, stockData);

        HoldingPosition position;

        if (!existingPosition.isPresent()) {
            if (isBuy) {
                // 신규 포지션 생성 (매수)
                position = HoldingPosition.of(member, stockData, quantity, price, 0L, 0.0);
                holdingPositionRepository.save(position);
            }
            // 매도인데 포지션이 없는 경우는 이미 예외 처리됨
        } else {
            position = existingPosition.get();

            if (isBuy) {
                // 매수: 평균 단가 재계산
                Long totalCost = (position.getAveragePrice() * position.getQuantity()) + (price * quantity);
                Integer newQuantity = position.getQuantity() + quantity;
                Long newAveragePrice = totalCost / newQuantity;

                position.updatePosition(newQuantity, newAveragePrice, 0L, 0.0);
            } else {
                // 매도: 수량 차감
                Integer newQuantity = position.getQuantity() - quantity;

                if (newQuantity == 0) {
                    position.updatePosition(0, position.getAveragePrice(), 0L, 0.0); // 수량을 0으로 설정
                    position.deactivate();
                } else {
                    position.updatePosition(newQuantity, position.getAveragePrice(), 0L, 0.0);
                }
            }

            holdingPositionRepository.save(position);
        }
    }

    /**
     * 캐시를 정리합니다 (5분마다 실행)
     */
    @Scheduled(fixedRate = 300000)
    public void cleanupCache() {
        chartCache.entrySet().removeIf(entry -> !entry.getValue().isValid());
        log.debug("차트 캐시 정리 완료. 남은 항목 수: {}", chartCache.size());
    }
}