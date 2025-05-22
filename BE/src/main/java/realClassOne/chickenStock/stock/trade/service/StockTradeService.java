package realClassOne.chickenStock.stock.trade.service;

import com.fasterxml.jackson.databind.JsonNode;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronizationAdapter;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.InvestmentSummary;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.notification.service.NotificationService;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.dto.common.PendingOrderDTO;
import realClassOne.chickenStock.stock.dto.request.TradeRequestDTO;
import realClassOne.chickenStock.stock.dto.response.InitializeMoneyResponseDTO;
import realClassOne.chickenStock.stock.dto.response.StockInfoResponseDTO;
import realClassOne.chickenStock.stock.dto.response.TradeResponseDTO;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.entity.TradeHistory;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;
import realClassOne.chickenStock.stock.repository.PendingOrderRepository;
import realClassOne.chickenStock.stock.repository.StockDataRepository;
import realClassOne.chickenStock.stock.repository.TradeHistoryRepository;
import realClassOne.chickenStock.stock.service.KiwoomStockApiService;
import realClassOne.chickenStock.stock.service.StockSubscriptionService;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;
import realClassOne.chickenStock.stock.websocket.handler.PortfolioWebSocketHandler;
import realClassOne.chickenStock.stock.websocket.handler.StockWebSocketHandler;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class StockTradeService implements KiwoomWebSocketClient.StockDataListener {

    @PersistenceContext
    private EntityManager entityManager;

    private final MemberRepository memberRepository;
    private final StockDataRepository stockDataRepository;
    private final TradeHistoryRepository tradeHistoryRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final PendingOrderRepository pendingOrderRepository;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final StockSubscriptionService stockSubscriptionService;
    private final JwtTokenProvider jwtTokenProvider;
    private final PortfolioWebSocketHandler portfolioWebSocketHandler;
    private final StockWebSocketHandler stockWebSocketHandler;
    private final FeeTaxService feeTaxService;
    private final NotificationService notificationService;
    private final TradingSecurityService tradingSecurityService;
    private final DistributedLockService distributedLockService;
    private final KiwoomApiCircuitBreaker circuitBreaker;
    private final StockPriceCacheService stockPriceCacheService;
    private final TradeEventService tradeEventService;

    // 동시성 제어를 위한 락 추가
    private final Map<String, ReentrantLock> stockLocks = new ConcurrentHashMap<>();
    private final Map<Long, ReentrantLock> memberLocks = new ConcurrentHashMap<>();

    // 트레이드 성공 여부를 추적하기 위한 메트릭
    private final AtomicInteger successfulTrades = new AtomicInteger(0);
    private final AtomicInteger failedTrades = new AtomicInteger(0);

    private final KiwoomStockApiService kiwoomStockApiService;

    // 특정 종목에 대한 락 획득
    private ReentrantLock getStockLock(String stockCode) {
        return stockLocks.computeIfAbsent(stockCode, k -> new ReentrantLock());
    }

    // 특정 회원에 대한 락 획득 (자금 관련 동시성 제어)
    private ReentrantLock getMemberLock(Long memberId) {
        return memberLocks.computeIfAbsent(memberId, k -> new ReentrantLock());
    }

//    // 매수 메서드
//    @Transactional(isolation = Isolation.READ_COMMITTED)
//    public TradeResponseDTO buyStock(String authorizationHeader, TradeRequestDTO request) {
//        try {
//            // 분산락 획득 시도
//            String lockName = "trade:" + request.getStockCode();
//            String ownerId = "thread-" + Thread.currentThread().getId() + "-buyStock";
//            boolean lockAcquired = distributedLockService.tryLock(lockName, ownerId, 5, 10);
//
//            if (!lockAcquired) {
//                failedTrades.incrementAndGet();
//                throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
//                        "현재 해당 종목에 대한 거래가 많습니다. 잠시 후 다시 시도해주세요.");
//            }
//
//            try {
//                return buyStockInternal(authorizationHeader, request);
//            } finally {
//                distributedLockService.unlock(lockName, ownerId);
//            }
//        } catch (CustomException e) {
//            throw e;
//        } catch (Exception e) {
//            log.error("매수 처리 중 오류 발생", e);
//            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED);
//        }
//    }

    @Transactional(isolation = Isolation.READ_COMMITTED)
    public TradeResponseDTO processLimitBuyOrder(Member member, StockData stockData,
                                                  Integer quantity, Long price) {
        // 수수료 계산
        Long totalPrice = price * quantity;
        Long fee = calculateFee(totalPrice);

        // 잔액 확인
        if (member.getMemberMoney() < totalPrice + fee) {
            throw new CustomException(StockErrorCode.INSUFFICIENT_FUNDS);
        }

        // 지정가 주문 생성
        PendingOrder pendingOrder = PendingOrder.createBuyOrder(
                member, stockData, quantity, price, fee
        );
        pendingOrderRepository.save(pendingOrder);

        // 잔액에서 예약금 차감 (주문 금액 + 수수료)
        Long newBalance = member.getMemberMoney() - totalPrice - fee;
        member.updateMemberMoney(newBalance);
        memberRepository.save(member);

        // 해당 종목 실시간 가격 구독 - 지정가 주문 목적으로 구독
        String pendingOrderPurpose = "PENDING_ORDER_" + pendingOrder.getOrderId();
        boolean subscriptionSuccess = kiwoomWebSocketClient.subscribeStockWithPurpose(
                stockData.getShortCode(),
                pendingOrderPurpose
        );

        if (!subscriptionSuccess) {
//            log.warn("지정가 주문에 대한 종목 구독 실패: {} (주문ID: {})",
//                    stockData.getShortCode(), pendingOrder.getOrderId());
        }

        // 즉시 체결 확인 (비동기로 처리) - 트랜잭션 완료 후 실행되도록 변경
        // 중요: TransactionSynchronizationManager를 사용하여 현재 트랜잭션 완료 후 실행
        TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronizationAdapter() {
            @Override
            public void afterCommit() {
                // 트랜잭션이 완료된 후에 실행
                CompletableFuture.runAsync(() -> {
                    try {
                        // 약간의 지연을 주어 DB에 반영될 시간을 확보
                        Thread.sleep(500);

                        // 최신 상태의 주문 객체를 다시 조회하여 사용
                        PendingOrder freshOrder = pendingOrderRepository.findById(pendingOrder.getOrderId())
                                .orElse(null);

                        if (freshOrder != null && freshOrder.getStatus() == PendingOrder.OrderStatus.PENDING) {
                            checkAndExecutePendingOrder(freshOrder);
                        }
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    } catch (Exception e) {
                    }
                });
            }
        });

        // 응답 생성
        return TradeResponseDTO.builder()
                .orderId(pendingOrder.getOrderId())
                .stockCode(stockData.getShortCode())
                .stockName(stockData.getShortName())
                .tradeType("BUY")
                .quantity(quantity)
                .unitPrice(price)
                .totalPrice(totalPrice)
                .fee(fee)
                .status("PENDING")
                .message("지정가 매수 주문이 등록되었습니다.")
                .build();
    }

    // 오버로딩 - 내부 사용
    @Transactional(isolation = Isolation.READ_COMMITTED)
    public TradeResponseDTO buyStockWithLock(TradeRequestDTO request, Member member) {
        validateTradeRequest(request);

        StockData stock = stockDataRepository.findByShortCode(request.getStockCode())
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND, "해당 종목을 찾을 수 없습니다: " + request.getStockCode()));

        // 비정상 거래 패턴 체크
        boolean isAbnormal = tradingSecurityService.checkAbnormalTradingPattern(
                member.getMemberId(), request.getStockCode(), "BUY");

        // 제한된 회원인지 체크
        boolean isRestricted = tradingSecurityService.isRestricted(
                member.getMemberId());

        if (isAbnormal || isRestricted) {
            failedTrades.incrementAndGet();
            throw new CustomException(StockErrorCode.TRADE_RESTRICTED,
                    "비정상적인 거래 패턴이 감지되어 제한되었습니다.");
        }

        // 분산락 획득 시도 - 재시도 로직 적용
        String lockName = "trade:" + request.getStockCode();
        String ownerId = "thread-" + Thread.currentThread().getId() + "-member-" + member.getMemberId();
        boolean lockAcquired = distributedLockService.tryLockWithRetry(lockName, ownerId, 3); // 최대 3번 재시도

        if (!lockAcquired) {
            failedTrades.incrementAndGet();
            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
                    "현재 해당 종목에 대한 거래가 많습니다. 잠시 후 다시 시도해주세요.");
        }

        try {
            // 데드락 방지를 위해 항상 작은 ID의 락부터 획득
            Long memberId = member.getMemberId();
            String stockCode = request.getStockCode();

            ReentrantLock lock1, lock2;
            // 숫자가 아닌 문자 제거 후 비교 (종목 코드에 문자가 포함될 수 있음)
            String numericStockCode = stockCode.replaceAll("[^0-9]", "");
            boolean useMemberLockFirst = memberId < (numericStockCode.isEmpty() ? 0 : Long.parseLong(numericStockCode));

            if (useMemberLockFirst) {
                lock1 = getMemberLock(memberId);
                lock2 = getStockLock(stockCode);
//                log.debug("락 획득 순서: 회원({}) -> 종목({})", memberId, stockCode);
            } else {
                lock1 = getStockLock(stockCode);
                lock2 = getMemberLock(memberId);
//                log.debug("락 획득 순서: 종목({}) -> 회원({})", stockCode, memberId);
            }

            try {
                lock1.lock();
                try {
                    lock2.lock();
                    try {
                        Long currentPrice;
                        boolean temporarySubscription = false;

                        if (Boolean.TRUE.equals(request.getMarketOrder())) {
                            temporarySubscription = !kiwoomWebSocketClient.isSubscribed(request.getStockCode());

                            // 서킷 브레이커 패턴 적용
                            currentPrice = circuitBreaker.executeCall(
                                    () -> getCurrentStockPriceWithRetry(request.getStockCode()),
                                    null
                            );

                            if (currentPrice == null) {
                                failedTrades.incrementAndGet();
                                throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE);
                            }
                        } else {
                            return createErrorResponse("내부 로직 오류: 시장가 주문이 아닌 요청이 들어왔습니다.");
                        }

                        // 총 구매 금액 계산
                        Long totalAmount = currentPrice * request.getQuantity();

                        // 수수료 계산
                        Long fee = feeTaxService.calculateBuyFee(totalAmount);

                        // 총 구매 금액에 수수료 추가
                        Long totalAmountWithFee = totalAmount + fee;

                        member = memberRepository.findById(member.getMemberId())
                                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

                        if (member.getMemberMoney() < totalAmountWithFee) {
                            failedTrades.incrementAndGet();
                            throw new CustomException(StockErrorCode.INSUFFICIENT_BALANCE);
                        }

                        // 매수 처리 (수수료 포함)
                        member.subtractMemberMoney(totalAmountWithFee);
                        memberRepository.save(member);

                        // 수수료를 전체 통계에 추가
                        feeTaxService.addBuyFee(fee);

                        // 거래 내역 생성 (수수료, 세금 포함)
                        TradeHistory tradeHistory = TradeHistory.of(
                                member,
                                stock,
                                TradeHistory.TradeType.BUY,
                                request.getQuantity(),
                                currentPrice,
                                totalAmount,
                                fee,    // 수수료 추가
                                0L,     // 매수에는 세금 없음
                                LocalDateTime.now()
                        );
                        tradeHistoryRepository.save(tradeHistory);

                        // 비활성화된 기존 포지션 확인 (같은 종목 재매수 확인)
                        Optional<HoldingPosition> existingPosition = holdingPositionRepository
                                .findByMemberAndStockData(member, stock);

                        if (existingPosition.isPresent()) {
                            HoldingPosition position = existingPosition.get();

                            if (!position.getActive()) {
                                // 비활성화된 포지션 재활성화
                                position.activate();
                                position.updatePosition(
                                        request.getQuantity(),
                                        currentPrice,
                                        0L,  // 초기 수익
                                        0.0  // 초기 수익률
                                );
                                holdingPositionRepository.save(position);
                                holdingPositionRepository.flush();

                            } else {
                                // 기존 활성 포지션 업데이트
                                int newQuantity = position.getQuantity() + request.getQuantity();
                                long newAvgPrice = ((position.getAveragePrice() * position.getQuantity()) + (currentPrice * request.getQuantity())) / newQuantity;
                                Long latestPrice = getCurrentStockPriceWithRetry(stock.getShortCode());
                                if (latestPrice == null) latestPrice = currentPrice;

                                long currentProfit = (latestPrice - newAvgPrice) * newQuantity;
                                double returnRate = ((double) latestPrice / newAvgPrice - 1.0) * 100.0;

                                position.updatePosition(newQuantity, newAvgPrice, currentProfit, returnRate);
                                holdingPositionRepository.save(position);
                                holdingPositionRepository.flush();
                            }
                        } else {
                            // 새 포지션 생성
                            HoldingPosition newPosition = HoldingPosition.of(
                                    member,
                                    stock,
                                    request.getQuantity(),
                                    currentPrice,
                                    0L, // 초기 수익
                                    0.0 // 초기 수익률
                            );
                            holdingPositionRepository.save(newPosition);
                            holdingPositionRepository.flush();
                            member.addHoldingPosition(newPosition);
                        }

                        // 투자 요약 업데이트
                        updateInvestmentSummary(member);

                        // 임시 구독 해제
                        if (temporarySubscription) {
                            unsubscribeStockAfterTrade(request.getStockCode());
                        }

                        // 가격 캐시 무효화 (추가된 부분)
                        stockPriceCacheService.invalidateCache(stock.getShortCode());

                        successfulTrades.incrementAndGet();

                        // 여기에 체결 정보 WebSocket 전송 코드 추가
                        try {
                            // 시간 포맷 변환 (HHmmss 형식으로)
                            String timestamp = tradeHistory.getTradedAt().format(java.time.format.DateTimeFormatter.ofPattern("HHmmss"));
                            stockWebSocketHandler.broadcastTradeExecution(
                                    stock.getShortCode(),
                                    "BUY",
                                    request.getQuantity(),
                                    currentPrice,
                                    totalAmount,
                                    timestamp
                            );
                        } catch (Exception e) {
//                            log.warn("시장가 매수 체결 정보 WebSocket 전송 실패: {}", e.getMessage());
                            // 웹소켓 전송 실패는 거래 자체에 영향을 주지 않으므로 예외 전파하지 않음
                        }

                        return TradeResponseDTO.fromTradeHistory(tradeHistory);
                    } catch (Exception e) {
//                        log.error("매수 주문 처리 중 오류 발생", e);
                        failedTrades.incrementAndGet();
                        throw e;
                    } finally {
                        lock2.unlock();
                    }
                } finally {
                    lock1.unlock();
                }
            } catch (Exception e) {
//                log.error("락 획득 중 오류 발생", e);
                throw e;
            }
        } finally {
            distributedLockService.unlock(lockName, ownerId);
        }
    }

//    @Transactional(isolation = Isolation.READ_COMMITTED)
//    public TradeResponseDTO sellStock(String authorizationHeader, TradeRequestDTO request) {
//        try {
//            // 토큰에서 회원 정보 추출
//            String token = jwtTokenProvider.resolveToken(authorizationHeader);
//            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
//            Member member = memberRepository.findById(memberId)
//                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));
//
//            // 비정상 거래 패턴 체크
//            boolean isAbnormal = tradingSecurityService.checkAbnormalTradingPattern(
//                    member.getMemberId(), request.getStockCode(), "SELL");
//
//            // 제한된 회원인지 체크
//            boolean isRestricted = tradingSecurityService.isRestricted(
//                    member.getMemberId());
//
//            if (isAbnormal || isRestricted) {
//                failedTrades.incrementAndGet();
//                throw new CustomException(StockErrorCode.TRADE_RESTRICTED,
//                        "비정상적인 거래 패턴이 감지되어 제한되었습니다.");
//            }
//
//            // 종목 정보 조회
//            StockData stockData = stockDataRepository.findByShortCode(request.getStockCode())
//                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));
//
//            Integer quantity = request.getQuantity();
//            if (quantity == null || quantity <= 0) {
//                throw new CustomException(StockErrorCode.INVALID_QUANTITY);
//            }
//
//            // 분산락 획득 시도
//            String lockName = "trade:" + request.getStockCode();
//            String ownerId = "thread-" + Thread.currentThread().getId() + "-member-" + member.getMemberId();
//            boolean lockAcquired = distributedLockService.tryLock(lockName, ownerId, 5, 10);
//
//            if (!lockAcquired) {
//                failedTrades.incrementAndGet();
//                throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
//                        "현재 해당 종목에 대한 거래가 많습니다. 잠시 후 다시 시도해주세요.");
//            }
//
//            try {
//                // 보유 포지션 확인
//                List<HoldingPosition> positions = holdingPositionRepository
//                        .findAllByMemberAndStockDataAndActiveTrue(member, stockData);
//                if (positions.isEmpty()) {
//                    throw new CustomException(StockErrorCode.NO_HOLDING_POSITION);
//                }
//                HoldingPosition position = positions.get(0);
//
//                // 현재 대기 중인 매도 주문 수량 조회
//                int pendingSellQuantity = pendingOrderRepository.getTotalPendingSellQuantityForMemberAndStock(
//                        member.getMemberId(), stockData.getShortCode());
//
//                // 실제 매도 가능 수량 계산 (보유 수량 - 대기 중인 매도 주문 수량)
//                int availableQuantity = position.getQuantity() - pendingSellQuantity;
//
//                if (availableQuantity < quantity) {
//                    throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK_QUANTITY,
//                            String.format("매도 가능 수량이 부족합니다. 보유: %d주, 대기 중인 매도 주문: %d주, 가능: %d주, 요청: %d주",
//                                    position.getQuantity(), pendingSellQuantity, availableQuantity, quantity));
//                }
//
//                // 시장가 주문인 경우
//                if (Boolean.TRUE.equals(request.getMarketOrder())) {
//                    return processMarketSellOrder(member, stockData, quantity, position);
//                }
//
//                // 지정가 주문인 경우
//                if (request.getPrice() == null || request.getPrice() <= 0) {
//                    throw new CustomException(StockErrorCode.INVALID_PRICE);
//                }
//
//                return processLimitSellOrder(member, stockData, quantity, request.getPrice());
//            } finally {
//                distributedLockService.unlock(lockName, ownerId);
//            }
//
//        } catch (CustomException e) {
//            throw e;
//        } catch (Exception e) {
//            log.error("매도 처리 중 오류 발생", e);
//            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED);
//        }
//    }

    /**
     * 지정가 매도 주문 처리
     */
    @Transactional(isolation = Isolation.READ_COMMITTED)
    public TradeResponseDTO processLimitSellOrder(Member member, StockData stockData,
                                                  Integer quantity, Long price) {
        // 보유 포지션 확인
        List<HoldingPosition> positions = holdingPositionRepository
                .findAllByMemberAndStockDataAndActiveTrue(member, stockData);
        if (positions.isEmpty()) {
            throw new CustomException(StockErrorCode.NO_HOLDING_POSITION);
        }
        HoldingPosition position = positions.get(0);

        // 현재 대기 중인 매도 주문 수량 조회
        int pendingSellQuantity = pendingOrderRepository.getTotalPendingSellQuantityForMemberAndStock(
                member.getMemberId(), stockData.getShortCode());

        // 실제 매도 가능 수량 계산 (보유 수량 - 대기 중인 매도 주문 수량)
        int availableQuantity = position.getQuantity() - pendingSellQuantity;

        if (availableQuantity < quantity) {
            throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK_QUANTITY,
                    String.format("매도 가능 수량이 부족합니다. 보유: %d주, 대기 중인 매도 주문: %d주, 가능: %d주, 요청: %d주",
                            position.getQuantity(), pendingSellQuantity, availableQuantity, quantity));
        }

        // 수수료 및 세금 계산
        Long totalPrice = price * quantity;
        Long fee = calculateFee(totalPrice);
        Long tax = calculateTax(totalPrice);

        // 지정가 주문 생성
        PendingOrder pendingOrder = PendingOrder.createSellOrder(
                member, stockData, quantity, price, fee, tax
        );
        pendingOrderRepository.save(pendingOrder);

        // 해당 종목 실시간 가격 구독 - 지정가 주문 목적으로 구독
        String pendingOrderPurpose = "PENDING_ORDER_" + pendingOrder.getOrderId();
        boolean subscriptionSuccess = kiwoomWebSocketClient.subscribeStockWithPurpose(
                stockData.getShortCode(),
                pendingOrderPurpose
        );

        if (!subscriptionSuccess) {
//            log.warn("지정가 주문에 대한 종목 구독 실패: {} (주문ID: {})",
//                    stockData.getShortCode(), pendingOrder.getOrderId());
        }

        // 응답 생성
        return TradeResponseDTO.builder()
                .orderId(pendingOrder.getOrderId())
                .stockCode(stockData.getShortCode())
                .stockName(stockData.getShortName())
                .tradeType("SELL")
                .quantity(quantity)
                .unitPrice(price)
                .totalPrice(totalPrice)
                .fee(fee)
                .tax(tax)
                .status("PENDING")
                .message("지정가 매도 주문이 등록되었습니다.")
                .build();
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public boolean deactivateHoldingPosition(Long positionId) {
        try {
            // 논리적 삭제
            int updatedRows = holdingPositionRepository.updateActiveStatus(positionId, false);
            return updatedRows > 0;
        } catch (Exception e) {
//            log.error("포지션 논리적 삭제 중 오류 발생: 포지션ID={}", positionId, e);
            return false;
        }
    }

    // 오버로딩 - 내부 사용
    @Transactional(isolation = Isolation.READ_COMMITTED)
    public TradeResponseDTO sellStockWithLock(TradeRequestDTO request, Member member) {
        validateTradeRequest(request);

        // 비정상 거래 패턴 체크
        boolean isAbnormal = tradingSecurityService.checkAbnormalTradingPattern(
                member.getMemberId(), request.getStockCode(), "SELL");

        // 제한된 회원인지 체크
        boolean isRestricted = tradingSecurityService.isRestricted(
                member.getMemberId());

        if (isAbnormal || isRestricted) {
            failedTrades.incrementAndGet();
            throw new CustomException(StockErrorCode.TRADE_RESTRICTED,
                    "비정상적인 거래 패턴이 감지되어 제한되었습니다.");
        }

        // 분산락 획득 시도 - 재시도 로직 적용
        String lockName = "trade:" + request.getStockCode();
        String ownerId = "thread-" + Thread.currentThread().getId() + "-member-" + member.getMemberId();
        boolean lockAcquired = distributedLockService.tryLockWithRetry(lockName, ownerId, 3); // 최대 3번 재시도

        if (!lockAcquired) {
            failedTrades.incrementAndGet();
            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
                    "현재 해당 종목에 대한 거래가 많습니다. 잠시 후 다시 시도해주세요.");
        }

        try {
            StockData stock = stockDataRepository.findByShortCode(request.getStockCode())
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 데드락 방지를 위해 항상 작은 ID의 락부터 획득
            Long memberId = member.getMemberId();
            String stockCode = request.getStockCode();

            ReentrantLock lock1, lock2;
            // 숫자가 아닌 문자 제거 후 비교 (종목 코드에 문자가 포함될 수 있음)
            String numericStockCode = stockCode.replaceAll("[^0-9]", "");
            boolean useMemberLockFirst = memberId < (numericStockCode.isEmpty() ? 0 : Long.parseLong(numericStockCode));

            if (useMemberLockFirst) {
                lock1 = getMemberLock(memberId);
                lock2 = getStockLock(stockCode);
//                log.debug("락 획득 순서: 회원({}) -> 종목({})", memberId, stockCode);
            } else {
                lock1 = getStockLock(stockCode);
                lock2 = getMemberLock(memberId);
//                log.debug("락 획득 순서: 종목({}) -> 회원({})", stockCode, memberId);
            }

            try {
                lock1.lock();
                try {
                    lock2.lock();
                    try {
                        // 매번 최신 상태의 회원 정보와 포지션 조회
                        member = memberRepository.findById(member.getMemberId())
                                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

                        // 보유 수량 확인 (active=true인 포지션만)
                        HoldingPosition position = holdingPositionRepository.findByMemberAndStockDataAndActiveTrue(member, stock)
                                .orElseThrow(() -> new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "해당 종목을 보유하고 있지 않습니다"));

                        // 현재 대기 중인 매도 주문 수량 조회
                        int pendingSellQuantity = pendingOrderRepository.getTotalPendingSellQuantityForMemberAndStock(
                                member.getMemberId(), stock.getShortCode());

                        // 실제 매도 가능 수량 계산 (보유 수량 - 대기 중인 매도 주문 수량)
                        int availableQuantity = position.getQuantity() - pendingSellQuantity;

                        if (availableQuantity < request.getQuantity()) {
                            failedTrades.incrementAndGet();
                            throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK,
                                    String.format("매도 가능 수량이 부족합니다. 보유: %d주, 대기 중인 매도 주문: %d주, 가능: %d주, 요청: %d주",
                                            position.getQuantity(), pendingSellQuantity, availableQuantity, request.getQuantity()));
                        }

                        // 시장가 주문인 경우 실시간 가격 조회
                        Long currentPrice;
                        boolean temporarySubscription = false;

                        if (Boolean.TRUE.equals(request.getMarketOrder())) {
                            temporarySubscription = !kiwoomWebSocketClient.isSubscribed(request.getStockCode());

                            // 서킷 브레이커 패턴 적용
                            currentPrice = circuitBreaker.executeCall(
                                    () -> getCurrentStockPriceWithRetry(request.getStockCode()),
                                    null
                            );

                            if (currentPrice == null) {
                                failedTrades.incrementAndGet();
                                throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE);
                            }
                        } else {
                            return createErrorResponse("내부 로직 오류: 시장가 주문이 아닌 요청이 들어왔습니다.");
                        }

                        // 포지션 업데이트를 위해 먼저 홀딩 포지션 처리 (즉시 반영)
                        int newQuantity = position.getQuantity() - request.getQuantity();

                        // 매도 수량 만큼 즉시 차감
                        if (newQuantity == 0) {
                            // 포지션 비활성화 (논리적 삭제) + 수량도 0으로 업데이트
                            position.updatePosition(0, position.getAveragePrice(), 0L, 0.0); // 수량을 0으로 설정
                            position.deactivate();
                            holdingPositionRepository.save(position);
                            holdingPositionRepository.flush();

                        } else {
                            // 보유량 감소만 처리 (평균단가는 변경하지 않음)
                            Long latestPrice = getCurrentStockPriceWithRetry(stock.getShortCode());
                            if (latestPrice == null) latestPrice = currentPrice; // 가격 조회 실패 시 거래 가격 사용

                            long currentProfit = (latestPrice - position.getAveragePrice()) * newQuantity;
                            double returnRate = ((double) latestPrice / position.getAveragePrice() - 1.0) * 100.0;

                            position.updatePosition(newQuantity, position.getAveragePrice(), currentProfit, returnRate);
                            holdingPositionRepository.save(position);
                            holdingPositionRepository.flush(); // 즉시 DB에 반영
                        }

                        // 총 판매 금액 계산
                        Long totalAmount = currentPrice * request.getQuantity();

                        // 수수료 및 세금 계산
                        Long fee = feeTaxService.calculateSellFee(totalAmount);
                        Long tax = feeTaxService.calculateSellTax(totalAmount);

                        // 순 매도 금액 (수수료와 세금 제외)
                        Long netAmount = totalAmount - fee - tax;

                        // 매도 처리 (순 금액만 입금)
                        member.addMemberMoney(netAmount);
                        memberRepository.save(member);

                        // 수수료와 세금을 전체 통계에 추가
                        feeTaxService.addSellFee(fee);
                        feeTaxService.addSellTax(tax);

                        // 거래 내역 생성 (수수료, 세금 포함)
                        TradeHistory tradeHistory = TradeHistory.of(
                                member,
                                stock,
                                TradeHistory.TradeType.SELL,
                                request.getQuantity(),
                                currentPrice,
                                totalAmount,
                                fee,    // 매도 수수료
                                tax,    // 매도 세금
                                LocalDateTime.now()
                        );
                        tradeHistoryRepository.save(tradeHistory);

                        // 투자 요약 업데이트
                        updateInvestmentSummary(member);

                        // 임시 구독 해제
                        if (temporarySubscription) {
                            unsubscribeStockAfterTrade(request.getStockCode());
                        }

                        // 가격 캐시 무효화 (추가된 부분)
                        stockPriceCacheService.invalidateCache(stock.getShortCode());

                        successfulTrades.incrementAndGet();

                        // 추가: 트랜잭션을 즉시 플러시하여 DB에 반영
                        holdingPositionRepository.flush();
                        memberRepository.flush();

                        // 여기에 체결 정보 WebSocket 전송 코드 추가
                        try {
                            // 시간 포맷 변환 (HHmmss 형식으로)
                            String timestamp = tradeHistory.getTradedAt().format(java.time.format.DateTimeFormatter.ofPattern("HHmmss"));
                            stockWebSocketHandler.broadcastTradeExecution(
                                    stock.getShortCode(),
                                    "SELL",
                                    request.getQuantity(),
                                    currentPrice,
                                    totalAmount,
                                    timestamp
                            );
                        } catch (Exception e) {
//                            log.warn("시장가 매도 체결 정보 WebSocket 전송 실패: {}", e.getMessage());
                            // 웹소켓 전송 실패는 거래 자체에 영향을 주지 않으므로 예외 전파하지 않음
                        }

                        return TradeResponseDTO.fromTradeHistory(tradeHistory);
                    } catch (Exception e) {
//                        log.error("매도 주문 처리 중 오류 발생", e);
                        failedTrades.incrementAndGet();
                        throw e;
                    } finally {
                        lock2.unlock();
                    }
                } finally {
                    lock1.unlock();
                }
            } catch (Exception e) {
//                log.error("락 획득 중 오류 발생", e);
                throw e;
            }
        } finally {
            distributedLockService.unlock(lockName, ownerId);
        }
    }

    // 입력값 검증 메서드
    private void validateTradeRequest(TradeRequestDTO request) {
        if (request == null) {
            throw new CustomException(StockErrorCode.INVALID_REQUEST, "요청 정보가 없습니다");
        }

        if (request.getStockCode() == null || request.getStockCode().trim().isEmpty()) {
            throw new CustomException(StockErrorCode.INVALID_REQUEST, "종목 코드가 필요합니다");
        }

        if (request.getQuantity() == null || request.getQuantity() <= 0) {
            throw new CustomException(StockErrorCode.INVALID_REQUEST, "유효한 수량이 필요합니다");
        }

        if (Boolean.FALSE.equals(request.getMarketOrder()) && (request.getPrice() == null || request.getPrice() <= 0)) {
            throw new CustomException(StockErrorCode.INVALID_REQUEST, "지정가 주문에는 가격이 필요합니다");
        }
    }

    public Long getCurrentStockPriceWithRetry(String stockCode) {
        int maxRetries = 3;
        int retryCount = 0;
        long retryDelayMs = 200;

        // 1. 먼저 캐시에서 확인
        Long cachedPrice = stockPriceCacheService.getFromCacheOnly(stockCode);
        if (cachedPrice != null) {

            return cachedPrice;
        }

        boolean temporarySubscription = false;
        String purpose = "PRICE_CHECK_" + System.currentTimeMillis();

        try {
            // 2. 웹소켓에서 확인 (구독 중인 경우)
            if (kiwoomWebSocketClient.isSubscribed(stockCode)) {
                Long websocketPrice = kiwoomWebSocketClient.getLatestPrice(stockCode);
                if (websocketPrice != null) {
                    stockPriceCacheService.updatePriceAndNotify(stockCode, websocketPrice);

                    return websocketPrice;
                }
            }

            // 3. 임시 구독 시도
//            log.debug("종목 {} 가격 조회를 위한 임시 구독 시작", stockCode);
            temporarySubscription = kiwoomWebSocketClient.subscribeStockWithPurpose(stockCode, purpose);

            if (temporarySubscription) {
                // 데이터 수신 대기
                Thread.sleep(500);

                // 가격 확인
                Long websocketPrice = kiwoomWebSocketClient.getLatestPrice(stockCode);
                if (websocketPrice != null) {
                    stockPriceCacheService.updatePriceAndNotify(stockCode, websocketPrice);
//                    log.debug("임시 구독 후 가격 조회 성공: {}, 가격: {}", stockCode, websocketPrice);
                    return websocketPrice;
                }
            }

            // 4. REST API로 가격 조회 (최대 3회 재시도)
            while (retryCount < maxRetries) {
                try {
                    Long apiPrice = getCurrentPriceUsingRestApi(stockCode);
                    if (apiPrice != null) {
                        stockPriceCacheService.updatePriceAndNotify(stockCode, apiPrice);

                        return apiPrice;
                    }
                } catch (Exception e) {
//                    log.error("REST API 가격 조회 실패 (시도 {}/{}): {}",
//                            retryCount + 1, maxRetries, e.getMessage());
                }

                retryCount++;
                if (retryCount < maxRetries) {
                    Thread.sleep(retryDelayMs);
                    retryDelayMs *= 2; // 지수 백오프
                }
            }

            // 모든 시도 실패
//            log.error("종목 {} 가격 조회 모든 경로 실패", stockCode);
            throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE,
                    "현재 종목 가격 정보를 가져올 수 없습니다. 잠시 후 다시 시도해 주세요.");

        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
//            log.error("가격 조회 중 인터럽트 발생: {}", stockCode);
            throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE,
                    "가격 조회 처리가 중단되었습니다.");
        } catch (CustomException ce) {
            throw ce;
        } catch (Exception e) {
//            log.error("종목 {} 가격 조회 중 예외 발생: {}", stockCode, e.getMessage(), e);
            throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE,
                    "시스템 오류로 가격 정보를 가져올 수 없습니다.");
        } finally {
            // 임시 구독 해제
            if (temporarySubscription) {
                try {
//                    log.debug("임시 구독 해제: {}", stockCode);
                    kiwoomWebSocketClient.unsubscribeStockForPurpose(stockCode, purpose);
                } catch (Exception e) {
//                    log.warn("임시 구독 해제 중 오류: {}", e.getMessage());
                }
            }
        }
    }

    // REST API를 이용한 현재가 조회 메서드
    private Long getCurrentPriceUsingRestApi(String stockCode) {
        try {

            // 1. 먼저 getStockInfo 메서드 시도
            try {
                StockInfoResponseDTO stockInfo = kiwoomStockApiService.getStockInfo(stockCode);
                if (stockInfo != null && stockInfo.getCurrentPrice() != null) {
                    String currentPriceStr = stockInfo.getCurrentPrice()
                            .replace(",", "")
                            .replace("+", "")
                            .replace("-", "")
                            .trim();

                    try {
                        Long currentPrice = Long.parseLong(currentPriceStr);
//                        log.info("종목 {} REST API (getStockInfo) 현재가 조회 성공: {}원", stockCode, currentPrice);
                        return currentPrice;
                    } catch (NumberFormatException e) {
//                        log.warn("현재가 문자열 변환 실패: {}", currentPriceStr);
                    }
                }
            } catch (Exception e) {
//                log.warn("getStockInfo로 현재가 조회 실패, getStockBasicInfo 시도: {}", e.getMessage());
            }

            // 2. getStockBasicInfo 메서드 시도
            JsonNode stockBasicInfo = kiwoomStockApiService.getStockBasicInfo(stockCode);
            if (stockBasicInfo != null && stockBasicInfo.has("cur_prc")) {
                String currentPriceStr = stockBasicInfo.get("cur_prc").asText()
                        .replace(",", "")
                        .replace("+", "")
                        .replace("-", "")
                        .trim();

                try {
                    Long currentPrice = Long.parseLong(currentPriceStr);
//                    log.info("종목 {} REST API (getStockBasicInfo) 현재가 조회 성공: {}원", stockCode, currentPrice);
                    return currentPrice;
                } catch (NumberFormatException e) {
//                    log.warn("현재가 문자열 변환 실패: {}", currentPriceStr);
                    return null;
                }
            }

//            log.error("종목 {} REST API 응답에서 현재가를 찾을 수 없음", stockCode);
            return null;
        } catch (Exception e) {
//            log.error("종목 {} REST API를 통한 현재가 조회 중 오류 발생", stockCode, e);
            return null;
        }
    }

    private boolean hasPendingOrderForStock(String stockCode) {
        return pendingOrderRepository.existsActivePendingOrderForStock(stockCode);
    }

    // 거래 후 구독 해제 메서드
    private void unsubscribeStockAfterTrade(String stockCode) {
        try {
            // 먼저 구독 상태 확인
            if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
//                log.debug("종목 {}은 이미 구독 해제되어 있음", stockCode);
                return;
            }

            // 해당 종목이 다른 곳에서 필요한지 확인
            boolean isNeededElsewhere = isStockNeededElsewhere(stockCode);

            if (!isNeededElsewhere) {
//                log.info("거래 완료 후 불필요한 종목 구독 해제: {}", stockCode);
                kiwoomWebSocketClient.unsubscribeStock(stockCode);
            } else {
//                log.info("종목 {}은 다른 곳에서 사용 중이므로 구독 유지", stockCode);
            }
        } catch (Exception e) {
//            log.warn("종목 {} 구독 해제 중 오류 발생", stockCode, e);
        }
    }

    private boolean isStockNeededElsewhere(String stockCode) {
        // 1. 해당 종목의 지정가 주문이 있는지 확인
        boolean hasPendingOrders = pendingOrderRepository.existsByStockDataShortCodeAndStatus(
                stockCode, PendingOrder.OrderStatus.PENDING);

        // 2. 회원들의 포트폴리오에 해당 종목이 있는지 확인
        boolean inPortfolios = holdingPositionRepository.existsByStockDataShortCode(stockCode);

        // 3. 웹소켓 클라이언트 목적 기반 구독 확인
        boolean hasWebSocketSubscribers = kiwoomWebSocketClient.hasSubscriptionPurpose(stockCode, "WEBSOCKET_CLIENT");

        // 4. 지정가 주문 목적 구독 확인
        boolean hasPendingOrderPurpose = kiwoomWebSocketClient.hasSubscriptionPurpose(stockCode, "PENDING_ORDER");

        // 5. 포트폴리오 목적 구독 확인
        boolean hasPortfolioPurpose = kiwoomWebSocketClient.hasSubscriptionPurpose(stockCode, "PORTFOLIO");

        boolean result = hasPendingOrders || inPortfolios || hasWebSocketSubscribers ||
                hasPendingOrderPurpose || hasPortfolioPurpose;

//        log.debug("종목 {} 필요 여부 확인: 지정가 주문={}, 포트폴리오={}, 결과={}",
//                stockCode, hasPendingOrders, inPortfolios, result);

        return result;
    }

    @Transactional
    public InitializeMoneyResponseDTO initializeMemberMoney(String authorization) {
        String token = jwtTokenProvider.resolveToken(authorization);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // 회원 락 획득
        ReentrantLock memberLock = getMemberLock(memberId);
        memberLock.lock();
        try {
            // 회원 정보 먼저 불러와서 초기화 여부 확인
            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 이미 초기화를 한 경우 예외 발생
            if (!List.of(1L, 2L, 3L, 4L).contains(member.getMemberId())) { // BOT일 경우 초기화 무제한
                if (member.isMoneyInitialized()) {
                    throw new CustomException(MemberErrorCode.MONEY_ALREADY_INITIALIZED);
                }
            }

            // 1. 모든 엔티티를 JPQL을 사용하여 직접 삭제 (순서 중요: 외래 키 제약조건 고려)

            // 1.1 보류 중인 주문(PendingOrder) 삭제
            int deletedPendingOrders = entityManager.createQuery(
                            "DELETE FROM PendingOrder p WHERE p.member.id = :memberId")
                    .setParameter("memberId", memberId)
                    .executeUpdate();

            // 1.2 거래 내역(TradeHistory) 삭제
            int deletedTradeHistories = entityManager.createQuery(
                            "DELETE FROM TradeHistory t WHERE t.member.id = :memberId")
                    .setParameter("memberId", memberId)
                    .executeUpdate();

            // 1.3 보유 포지션(HoldingPosition) 삭제
            int deletedPositions = entityManager.createQuery(
                            "DELETE FROM HoldingPosition h WHERE h.member.id = :memberId")
                    .setParameter("memberId", memberId)
                    .executeUpdate();

            // 1.4 투자 요약(InvestmentSummary) 삭제
            int deletedSummaries = entityManager.createQuery(
                            "DELETE FROM InvestmentSummary i WHERE i.member.id = :memberId")
                    .setParameter("memberId", memberId)
                    .executeUpdate();

            // 변경사항을 DB에 반영
            entityManager.flush();

            // 중요: 영속성 컨텍스트 초기화
            entityManager.clear();

            // 2. 새로운 상태로 회원 정보 다시 로드
            member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 3. 회원 자금 초기화 (1억원)
            member.updateMemberMoney(100_000_000L);
            member.markMoneyInitialized(); // 초기화 여부 true로 변경

            // 4. 새로운 투자 요약 생성
            InvestmentSummary summary = InvestmentSummary.of(
                    member,
                    0L,  // 총 투자금
                    0L,  // 총 평가액
                    0L,  // 총 손익
                    0.0  // 수익률
            );

            // 5. 저장 및 반영
            memberRepository.save(member);

            // 6. 변경사항 즉시 적용을 위한 플러시
            memberRepository.flush();

            return new InitializeMoneyResponseDTO(
                    "success",
                    "회원 기본금이 1억원으로 초기화되었으며, 모든 주식 관련 데이터가 삭제되었습니다.",
                    memberId
            );
        } finally {
            memberLock.unlock();
        }
    }

    /**
     * 수수료 계산 메서드들 추가
     */
    private Long calculateFee(Long totalPrice) {
        // feeTaxService.calculateBuyFee()로 변경 가능
        return feeTaxService.calculateBuyFee(totalPrice);
    }

    private Long calculateTax(Long totalPrice) {
        // feeTaxService.calculateSellTax()로 변경 가능
        return feeTaxService.calculateSellTax(totalPrice);
    }

    @Transactional(isolation = Isolation.READ_COMMITTED)
    public TradeResponseDTO processMarketBuyOrder(Member member, StockData stockData, Integer quantity) {
        // 분산락 획득 시도
        String lockName = "trade:" + stockData.getShortCode();
        String ownerId = "thread-" + Thread.currentThread().getId() + "-member-" + member.getMemberId();
        boolean lockAcquired = false;

        try {
            // 락 획득 시도 (최대 3회 재시도)
            lockAcquired = distributedLockService.tryLockWithRetry(lockName, ownerId, 3);

            if (!lockAcquired) {
                failedTrades.incrementAndGet();
                throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
                        "현재 해당 종목에 대한 거래가 많습니다. 잠시 후 다시 시도해주세요.");
            }

            // 현재가 조회
            Long currentPrice = circuitBreaker.executeCall(
                    () -> getCurrentStockPriceWithRetry(stockData.getShortCode()),
                    null
            );

            if (currentPrice == null) {
                throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE,
                        "현재 종목 가격을 조회할 수 없습니다. 잠시 후 다시 시도해주세요.");
            }

            // 총 구매 금액 계산
            Long totalAmount = currentPrice * quantity;

            // 수수료 계산
            Long fee = feeTaxService.calculateBuyFee(totalAmount);

            // 총 구매 금액에 수수료 추가
            Long totalAmountWithFee = totalAmount + fee;

            // 최신 회원 정보 조회 (잔액 확인 시점에 최신 데이터로)
            member = memberRepository.findById(member.getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 잔액 확인 - 중요!
            if (member.getMemberMoney() < totalAmountWithFee) {
                failedTrades.incrementAndGet();
                throw new CustomException(StockErrorCode.INSUFFICIENT_BALANCE,
                        String.format("잔액이 부족합니다. 필요 금액: %d원 (거래금액: %d원, 수수료: %d원), 보유 금액: %d원",
                                totalAmountWithFee, totalAmount, fee, member.getMemberMoney()));
            }

            // 매수 처리 (수수료 포함)
            member.subtractMemberMoney(totalAmountWithFee);
            memberRepository.save(member);

            // 수수료를 전체 통계에 추가 (별도 트랜잭션)
            try {
                feeTaxService.addBuyFee(fee);
            } catch (Exception e) {
//                log.warn("수수료 기록 중 오류 발생 (거래 처리는 계속 진행): {}", e.getMessage());
            }

            // 거래 내역 생성
            TradeHistory tradeHistory = TradeHistory.of(
                    member,
                    stockData,
                    TradeHistory.TradeType.BUY,
                    quantity,
                    currentPrice,
                    totalAmount,
                    fee,    // 수수료 추가
                    0L,     // 매수에는 세금 없음
                    LocalDateTime.now()
            );
            tradeHistoryRepository.save(tradeHistory);

            // 포지션 업데이트
            updateHoldingPositionForBuy(member, stockData, quantity, currentPrice);

            // 투자 요약 업데이트
            try {
                updateInvestmentSummary(member);
            } catch (Exception e) {
                log.warn("투자 요약 업데이트 중 오류 (거래 처리는 완료됨): {}", e.getMessage());
            }

            // 가격 캐시 무효화
            stockPriceCacheService.invalidateCache(stockData.getShortCode());

            // 체결 정보 WebSocket 전송
            try {
                String timestamp = tradeHistory.getTradedAt().format(DateTimeFormatter.ofPattern("HHmmss"));
                stockWebSocketHandler.broadcastTradeExecution(
                        stockData.getShortCode(),
                        "BUY",
                        quantity,
                        currentPrice,
                        totalAmount,
                        timestamp
                );
            } catch (Exception e) {
//                log.warn("체결 정보 WebSocket 전송 실패 (거래는 정상 처리됨): {}", e.getMessage());
            }

            successfulTrades.incrementAndGet();
            return TradeResponseDTO.fromTradeHistory(tradeHistory);

        } catch (Exception e) {
//            log.error("시장가 매수 처리 중 오류: {}", e.getMessage(), e);
            failedTrades.incrementAndGet();

            if (e instanceof CustomException) {
                throw e;
            } else {
                throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
                        "거래 처리 중 오류가 발생했습니다: " + e.getMessage());
            }
        } finally {
            // 락 해제 (획득한 경우에만)
            if (lockAcquired) {
                try {
                    distributedLockService.unlock(lockName, ownerId);
                } catch (Exception e) {
//                    log.warn("락 해제 중 오류: {}", e.getMessage());
                }
            }
        }
    }

    @Transactional(isolation = Isolation.READ_COMMITTED)
    public TradeResponseDTO processMarketSellOrder(Member member, StockData stockData, Integer quantity, HoldingPosition position) {
        // 분산락 획득 시도
        String lockName = "trade:" + stockData.getShortCode();
        String ownerId = "thread-" + Thread.currentThread().getId() + "-member-" + member.getMemberId();
        boolean lockAcquired = false;

        try {
            // 락 획득 시도 (최대 3회 재시도)
            lockAcquired = distributedLockService.tryLockWithRetry(lockName, ownerId, 3);

            if (!lockAcquired) {
                failedTrades.incrementAndGet();
                throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
                        "현재 해당 종목에 대한 거래가 많습니다. 잠시 후 다시 시도해주세요.");
            }

            // 현재가 조회
            Long currentPrice = circuitBreaker.executeCall(
                    () -> getCurrentStockPriceWithRetry(stockData.getShortCode()),
                    null
            );

            if (currentPrice == null) {
                throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE,
                        "현재 종목 가격을 조회할 수 없습니다. 잠시 후 다시 시도해주세요.");
            }

            // 보유 포지션 재확인 (최신 데이터)
            position = holdingPositionRepository.findByMemberAndStockDataAndActiveTrue(member, stockData)
                    .orElseThrow(() -> new CustomException(StockErrorCode.NO_HOLDING_POSITION,
                            "보유하지 않은 종목입니다: " + stockData.getShortCode()));

            // 현재 대기 중인 매도 주문 수량 조회
            int pendingSellQuantity = pendingOrderRepository.getTotalPendingSellQuantityForMemberAndStock(
                    member.getMemberId(), stockData.getShortCode());

            // 실제 매도 가능 수량 계산
            int availableQuantity = position.getQuantity() - pendingSellQuantity;

            if (availableQuantity < quantity) {
                throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK_QUANTITY,
                        String.format("매도 가능 수량이 부족합니다. 보유: %d주, 대기 중인 매도 주문: %d주, 가능: %d주, 요청: %d주",
                                position.getQuantity(), pendingSellQuantity, availableQuantity, quantity));
            }

            // 총 판매 금액 계산
            Long totalAmount = currentPrice * quantity;

            // 수수료 및 세금 계산
            Long fee = feeTaxService.calculateSellFee(totalAmount);
            Long tax = feeTaxService.calculateSellTax(totalAmount);

            // 순 매도 금액 (수수료와 세금 제외)
            Long netAmount = totalAmount - fee - tax;

            // 포지션 업데이트
            int newQuantity = position.getQuantity() - quantity;

            // 매도 수량 만큼 즉시 차감
            if (newQuantity == 0) {
                // 포지션 비활성화 (논리적 삭제) + 수량도 0으로 업데이트
                position.updatePosition(0, position.getAveragePrice(), 0L, 0.0);
                position.deactivate();
            } else {
                // 보유량 감소만 처리 (평균단가는 변경하지 않음)
                long currentProfit = (currentPrice - position.getAveragePrice()) * newQuantity;
                double returnRate = ((double) currentPrice / position.getAveragePrice() - 1.0) * 100.0;
                position.updatePosition(newQuantity, position.getAveragePrice(), currentProfit, returnRate);
            }

            holdingPositionRepository.save(position);
            holdingPositionRepository.flush(); // 즉시 DB에 반영

            // 매도 처리 (순 금액만 입금)
            member.addMemberMoney(netAmount);
            memberRepository.save(member);

            // 수수료와 세금을 전체 통계에 추가 (별도 트랜잭션)
            try {
                feeTaxService.addSellFee(fee);
                feeTaxService.addSellTax(tax);
            } catch (Exception e) {
//                log.warn("수수료/세금 기록 중 오류 발생 (거래 처리는 계속 진행): {}", e.getMessage());
            }

            // 거래 내역 생성
            TradeHistory tradeHistory = TradeHistory.of(
                    member,
                    stockData,
                    TradeHistory.TradeType.SELL,
                    quantity,
                    currentPrice,
                    totalAmount,
                    fee,
                    tax,
                    LocalDateTime.now()
            );
            tradeHistoryRepository.save(tradeHistory);

            // 투자 요약 업데이트
            try {
                updateInvestmentSummary(member);
            } catch (Exception e) {
//                log.warn("투자 요약 업데이트 중 오류 (거래 처리는 완료됨): {}", e.getMessage());
            }

            // 가격 캐시 무효화
            stockPriceCacheService.invalidateCache(stockData.getShortCode());

            // 체결 정보 WebSocket 전송
            try {
                String timestamp = tradeHistory.getTradedAt().format(DateTimeFormatter.ofPattern("HHmmss"));
                stockWebSocketHandler.broadcastTradeExecution(
                        stockData.getShortCode(),
                        "SELL",
                        quantity,
                        currentPrice,
                        totalAmount,
                        timestamp
                );
            } catch (Exception e) {
                log.warn("체결 정보 WebSocket 전송 실패 (거래는 정상 처리됨): {}", e.getMessage());
            }

            successfulTrades.incrementAndGet();
            return TradeResponseDTO.fromTradeHistory(tradeHistory);

        } catch (Exception e) {
            log.error("시장가 매도 처리 중 오류: {}", e.getMessage(), e);
            failedTrades.incrementAndGet();

            if (e instanceof CustomException) {
                throw e;
            } else {
                throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
                        "거래 처리 중 오류가 발생했습니다: " + e.getMessage());
            }
        } finally {
            // 락 해제 (획득한 경우에만)
            if (lockAcquired) {
                try {
                    distributedLockService.unlock(lockName, ownerId);
                } catch (Exception e) {
                    log.warn("락 해제 중 오류: {}", e.getMessage());
                }
            }
        }
    }

    // 주문 실패 시 금액 환불 처리 메서드
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void refundMoneyOnOrderFailure(Member member, Long amount) {
        try {
            // 최신 회원 정보 다시 조회
            Member freshMember = memberRepository.findById(member.getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 금액 환불
            freshMember.addMemberMoney(amount);
            memberRepository.save(freshMember);
//            log.info("주문 실패로 인한 금액 환불: 회원ID={}, 금액={}원", member.getMemberId(), amount);
        } catch (Exception e) {
//            log.error("주문 실패 후 금액 환불 처리 중 오류 발생", e);
        }
    }

    @Transactional
    public TradeResponseDTO createPendingSellOrder(Member member, StockData stock, TradeRequestDTO request) {
        // 종목 락 획득
        ReentrantLock stockLock = getStockLock(stock.getShortCode());
        stockLock.lock();
        try {
            // 최신 상태로 회원 및 포지션 정보 조회
            member = memberRepository.findById(member.getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 보유 수량 확인 (active=true인 포지션만)
            HoldingPosition position = holdingPositionRepository.findByMemberAndStockDataAndActiveTrue(member, stock)
                    .orElseThrow(() -> new CustomException(StockErrorCode.INSUFFICIENT_STOCK,
                            "해당 종목을 보유하고 있지 않습니다"));

            // 현재 대기 중인 매도 주문 수량 조회
            int pendingSellQuantity = pendingOrderRepository.getTotalPendingSellQuantityForMemberAndStock(
                    member.getMemberId(), stock.getShortCode());

            // 실제 매도 가능 수량 계산 (보유 수량 - 대기 중인 매도 주문 수량)
            int availableQuantity = position.getQuantity() - pendingSellQuantity;

            if (availableQuantity < request.getQuantity()) {
                failedTrades.incrementAndGet();
                throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK,
                        String.format("매도 가능 수량이 부족합니다. 보유: %d주, 대기 중: %d주, 가능: %d주, 요청: %d주",
                                position.getQuantity(), pendingSellQuantity, availableQuantity, request.getQuantity()));
            }


            // 현재 가격 확인하여 매도 조건 즉시 충족 여부 체크
            boolean temporarySubscription = !kiwoomWebSocketClient.isSubscribed(request.getStockCode());
            Long currentPrice = getCurrentStockPriceWithRetry(request.getStockCode());

            // 현재가가 지정가보다 높거나 같으면 즉시 체결
            if (currentPrice != null && currentPrice >= request.getPrice()) {
//                log.info("지정가({}원)가 현재가({}원)보다 낮거나 같아 즉시 체결합니다.", request.getPrice(), currentPrice);

                // 시장가와 동일한 sellStock 메서드 사용
                TradeRequestDTO marketRequest = new TradeRequestDTO();
                marketRequest.setStockCode(request.getStockCode());
                marketRequest.setQuantity(request.getQuantity());
                marketRequest.setMarketOrder(true);

                // 임시 구독이었다면 구독 해제
                if (temporarySubscription) {
                    unsubscribeStockAfterTrade(request.getStockCode());
                }

                return sellStockWithLock(marketRequest, member);
            }

            PendingOrder pendingOrder = null;
            try {
                // 예상 수수료 및 세금 계산
                Long expectedTotalAmount = request.getPrice() * request.getQuantity();
                Long expectedFee = feeTaxService.calculateSellFee(expectedTotalAmount);
                Long expectedTax = feeTaxService.calculateSellTax(expectedTotalAmount);

                // 지정가 주문 생성
                pendingOrder = PendingOrder.createSellOrder(
                        member,
                        stock,
                        request.getQuantity(),
                        request.getPrice(),
                        expectedFee,  // 예상 수수료
                        expectedTax   // 예상 세금
                );
                pendingOrderRepository.save(pendingOrder);

                // 해당 종목 실시간 가격 구독 - 지정가 주문 목적으로 구독
                String pendingOrderPurpose = "PENDING_ORDER_" + pendingOrder.getOrderId();
                boolean subscriptionSuccess = kiwoomWebSocketClient.subscribeStockWithPurpose(
                        request.getStockCode(),
                        pendingOrderPurpose
                );

                if (!subscriptionSuccess) {
//                    log.warn("지정가 주문에 대한 종목 구독 실패: {} (주문ID: {})",
//                            request.getStockCode(), pendingOrder.getOrderId());
                }

                TradeResponseDTO response = new TradeResponseDTO();
                response.setOrderId(pendingOrder.getOrderId());
                response.setStockCode(stock.getShortCode());
                response.setStockName(stock.getShortName());
                response.setTradeType("SELL");
                response.setQuantity(request.getQuantity());
                response.setUnitPrice(request.getPrice());
                response.setTotalPrice(expectedTotalAmount);
                response.setFee(expectedFee);
                response.setTax(expectedTax);
                response.setTradedAt(LocalDateTime.now());
                response.setStatus("PENDING");
                response.setMessage(String.format("지정가 매도 주문이 접수되었습니다. 예상 수수료: %d원, 예상 세금: %d원",
                        expectedFee, expectedTax));

                return response;
            } catch (Exception e) {
                log.error("지정가 매도 주문 처리 중 오류 발생", e);

                // 주문 실패 시 처리
                if (pendingOrder != null) {
                    pendingOrder.fail();
                    pendingOrderRepository.save(pendingOrder);

                    // 실패한 주문의 구독도 해제
                    String pendingOrderPurpose = "PENDING_ORDER_" + pendingOrder.getOrderId();
                    kiwoomWebSocketClient.unsubscribeStockForPurpose(request.getStockCode(), pendingOrderPurpose);
                }

                // 임시 구독이었고 주문 실패 시 구독 해제
                if (temporarySubscription) {
                    unsubscribeStockAfterTrade(request.getStockCode());
                }

                failedTrades.incrementAndGet();
                TradeResponseDTO errorResponse = new TradeResponseDTO();
                errorResponse.setStatus("ERROR");
                errorResponse.setMessage("지정가 매도 주문 처리 중 오류 발생: " + e.getMessage());
                return errorResponse;
            }
        } finally {
            stockLock.unlock();
        }
    }

    // 종목 구독 메서드
    private boolean subscribeStockIfNeeded(String stockCode) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
//            log.error("유효하지 않은 종목 코드로 구독 시도: {}", stockCode);
            return false;
        }

        stockCode = stockCode.trim();

        if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
            try {
//                log.info("종목 {} 실시간 데이터 구독 시작", stockCode);
                stockSubscriptionService.registerStockForSubscription(stockCode);
                return true;
            } catch (Exception e) {
//                log.error("종목 {} 구독 중 예외 발생", stockCode, e);
                return false;
            }
        }
        return true; // 이미 구독 중인 경우 true 반환
    }

    private Long getCurrentStockPrice(String stockCode) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
//            log.error("유효하지 않은 종목 코드: {}", stockCode);
            return null;
        }

        stockCode = stockCode.trim();

        try {
            // latestPriceDataCache에서 조회 - 내부적으로 두 형태 모두 시도함
            JsonNode priceData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);

            if (priceData != null && priceData.has("10")) {
                String currentPriceStr = priceData.get("10").asText()
                        .replace(",", "")
                        .replace("+", "")
                        .replace("-", "")
                        .trim();
                try {
                    return Long.parseLong(currentPriceStr);
                } catch (NumberFormatException e) {
//                    log.error("종목 {} 가격 변환 오류: {}", stockCode, currentPriceStr, e);
                    return null;
                }
            } else {
//                log.warn("종목 {} 실시간 가격 데이터가 없음", stockCode);
                return null;
            }
        } catch (Exception e) {
//            log.error("종목 {} 가격 조회 중 오류 발생", stockCode, e);
            return null;
        }
    }

    // 포지션 업데이트 메서드
    private void updateHoldingPosition(Member member, StockData stock, Integer quantity, Long price, TradeHistory.TradeType type) {
        try {
            Optional<HoldingPosition> optionalPosition = holdingPositionRepository.findByMemberAndStockData(member, stock);

            if (type == TradeHistory.TradeType.BUY) {
                if (optionalPosition.isPresent()) {
                    // 기존 포지션 업데이트
                    HoldingPosition position = optionalPosition.get();
                    int newQuantity = position.getQuantity() + quantity;
                    long newAvgPrice = ((position.getAveragePrice() * position.getQuantity()) + (price * quantity)) / newQuantity;
                    Long currentPrice = getCurrentStockPriceWithRetry(stock.getShortCode());
                    if (currentPrice == null) currentPrice = price; // 가격 조회 실패 시 거래 가격 사용

                    long currentProfit = (currentPrice - newAvgPrice) * newQuantity;
                    double returnRate = ((double) currentPrice / newAvgPrice - 1.0) * 100.0;

                    position.updatePosition(newQuantity, newAvgPrice, currentProfit, returnRate);
                    holdingPositionRepository.save(position);
                } else {
                    // 새 포지션 생성
                    HoldingPosition newPosition = HoldingPosition.of(
                            member,
                            stock,
                            quantity,
                            price,
                            0L, // 초기 수익
                            0.0 // 초기 수익률
                    );
                    holdingPositionRepository.save(newPosition);
                    member.addHoldingPosition(newPosition);
                }
            } else if (type == TradeHistory.TradeType.SELL) {
                // 매도는 반드시 기존 포지션이 있어야 함
                HoldingPosition position = optionalPosition.orElseThrow(
                        () -> new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "해당 종목을 보유하고 있지 않습니다"));

                int newQuantity = position.getQuantity() - quantity;
                if (newQuantity == 0) {
                    // 보유량이 0이 되면 포지션 삭제
                    holdingPositionRepository.delete(position);
                } else {
                    // 보유량 감소만 처리 (평균단가는 변경하지 않음)
                    Long currentPrice = getCurrentStockPriceWithRetry(stock.getShortCode());
                    if (currentPrice == null) currentPrice = price; // 가격 조회 실패 시 거래 가격 사용

                    long currentProfit = (currentPrice - position.getAveragePrice()) * newQuantity;
                    double returnRate = ((double) currentPrice / position.getAveragePrice() - 1.0) * 100.0;

                    position.updatePosition(newQuantity, position.getAveragePrice(), currentProfit, returnRate);
                    holdingPositionRepository.save(position);
                }
            }
        } catch (Exception e) {
            log.error("포지션 업데이트 중 오류 발생", e);
            throw new CustomException(StockErrorCode.POSITION_UPDATE_FAILED, "포지션 업데이트 실패: " + e.getMessage());
        }
    }

    @Transactional
    public void updateInvestmentSummary(Member member) {
        try {
            Member freshMember = memberRepository.findById(member.getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            InvestmentSummary summary = freshMember.getInvestmentSummary();

            // active=true인 포지션만 조회
            List<HoldingPosition> positions = holdingPositionRepository.findByMemberAndActiveTrue(freshMember);

            long totalInvestment = 0L;
            long totalValuation = 0L;

            for (HoldingPosition position : positions) {
                String stockCode = position.getStockData().getShortCode();
                long investedAmount = position.getAveragePrice() * position.getQuantity();

                // 현재가 조회 시도
                Long currentPrice = null;
                try {
                    // 먼저 캐시에서 조회
                    currentPrice = stockPriceCacheService.getFromCacheOnly(stockCode);

                    // 캐시에 없으면 웹소켓에서 조회
                    if (currentPrice == null && kiwoomWebSocketClient.isSubscribed(stockCode)) {
                        currentPrice = kiwoomWebSocketClient.getLatestPrice(stockCode);
                    }

                    // 없으면 평균 단가 사용
                    if (currentPrice == null) {
                        currentPrice = position.getAveragePrice();
                    }
                } catch (Exception e) {
//                    log.warn("투자 요약 업데이트 중 종목 {} 가격 조회 실패, 평균단가 사용: {}",
//                            stockCode, e.getMessage());
                    currentPrice = position.getAveragePrice();
                }

                long currentValue = currentPrice * position.getQuantity();
                totalInvestment += investedAmount;
                totalValuation += currentValue;
            }

            long totalProfitLoss = totalValuation - totalInvestment;
            double returnRate = totalInvestment > 0 ?
                    ((double) totalValuation / totalInvestment - 1.0) * 100.0 : 0.0;

            if (summary == null) {
                summary = InvestmentSummary.of(freshMember, totalInvestment, totalValuation, totalProfitLoss, returnRate);
                freshMember.setInvestmentSummary(summary);
            } else {
                summary.updateValues(totalInvestment, totalValuation, totalProfitLoss, returnRate);
            }

            // 변경 사항 저장
            memberRepository.save(freshMember);

        } catch (Exception e) {
            log.error("투자 요약 업데이트 중 오류 발생", e);
            // 투자 요약 업데이트 실패는 거래 자체에 영향을 주지 않도록 예외 전파하지 않음
        }
    }

    @Override
    @Transactional
    public void onStockPriceUpdate(String stockCode, JsonNode data) {
        try {
            if (data != null && data.has("10")) {
                // 주문이 있는지 먼저 확인하여 불필요한 처리 방지
                if (!hasPendingOrderForStock(stockCode)) {
                    return; // 주문이 없으면 바로 리턴
                }

                String priceStr = data.get("10").asText().replace(",", "").replace("+", "").replace("-", "").trim();
                try {
                    Long currentPrice = Long.parseLong(priceStr);
                    checkAndExecutePendingOrdersInNewTransaction(stockCode, currentPrice);
                } catch (NumberFormatException e) {
                    log.error("가격 변환 오류: {}", priceStr, e);
                }
            }
        } catch (Exception e) {
            log.error("지정가 주문 체결 확인 중 오류 발생", e);
        }
    }

    // 별도의 트랜잭션에서 실행하는 메서드
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void checkAndExecutePendingOrdersInNewTransaction(String stockCode, Long currentPrice) {
        ReentrantLock lock = getStockLock(stockCode);
        boolean lockAcquired = false;

        try {
            // 락 획득 시도 (최대 500ms만 대기)
            lockAcquired = lock.tryLock(500, TimeUnit.MILLISECONDS);
            if (!lockAcquired) {
//                log.warn("종목 {} 락 획득 실패로 처리 스킵", stockCode);
                return;
            }

            // 매수 주문 중 현재가가 지정가 이하인 주문 체결
            List<PendingOrder> buyOrders = pendingOrderRepository.findExecutableOrdersWithRelations(
                    stockCode, TradeHistory.TradeType.BUY, currentPrice);

            // 매도 주문 중 현재가가 지정가 이상인 주문 체결
            List<PendingOrder> sellOrders = pendingOrderRepository.findExecutableOrdersWithRelations(
                    stockCode, TradeHistory.TradeType.SELL, currentPrice);

            // 전체 주문 수 확인
            int totalOrders = buyOrders.size() + sellOrders.size();
            if (totalOrders == 0) {
                return; // 처리할 주문이 없으면 바로 리턴
            }

//            log.info("종목 {} 지정가 주문 체결 처리: 매수={}, 매도={}",
//                    stockCode, buyOrders.size(), sellOrders.size());

            // 매수 주문 처리
            for (PendingOrder order : buyOrders) {
                executeBuyOrderWithFreshData(order, currentPrice);
            }

            // 매도 주문 처리
            for (PendingOrder order : sellOrders) {
                executeSellOrderWithFreshData(order, currentPrice);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
//            log.error("주문 처리 중 인터럽트 발생", e);
        } catch (Exception e) {
            log.error("주문 처리 중 오류 발생: {}", e.getMessage(), e);
        } finally {
            if (lockAcquired) {
                lock.unlock();
            }
        }
    }

    // 매수 주문 처리 메서드
    @Transactional
    private void executeBuyOrderWithFreshData(PendingOrder order, Long currentPrice) {
        if (order.getStatus() != PendingOrder.OrderStatus.PENDING) {
            return;
        }

        try {
            Member member = order.getMember();
            StockData stock = order.getStockData();
            String stockCode = stock.getShortCode();

            // 주문 상태를 먼저 처리 중으로 변경
            order.processing();
            pendingOrderRepository.save(order);

            // 실제 거래 금액 계산
            long actualTotalAmount = currentPrice * order.getQuantity();

            // 새로운 수수료 계산 (실제 체결가 기준)
            Long actualFee = feeTaxService.calculateBuyFee(actualTotalAmount);

            // 원래 예약된 금액과 수수료
            long reservedTotalAmount = order.getTargetPrice() * order.getQuantity();
            Long reservedFee = order.getReservedFee() != null ? order.getReservedFee() : 0L;
            long totalReserved = reservedTotalAmount + reservedFee;

            // 실제 필요한 금액
            long actualTotalWithFee = actualTotalAmount + actualFee;

            // 환불액 계산 (예약액 - 실제 필요액)
            long refundAmount = totalReserved - actualTotalWithFee;

            if (refundAmount > 0) {
                // 차액 환불
                member.addMemberMoney(refundAmount);
                memberRepository.save(member);
//                log.info("체결가 차이로 인한 환불: 주문ID={}, 환불액={}원", order.getOrderId(), refundAmount);
            } else if (refundAmount < 0) {
                // 매우 드문 경우지만, 수수료가 예상보다 많을 경우
                long additionalAmount = Math.abs(refundAmount);

                // 잔액 확인
                if (member.getMemberMoney() < additionalAmount) {
//                    log.error("체결 시 추가 수수료 부족: 주문ID={}, 부족액={}원", order.getOrderId(), additionalAmount);
                    order.fail();
                    pendingOrderRepository.save(order);
                    failedTrades.incrementAndGet();

                    // 전체 예약금 환불
                    member.addMemberMoney(totalReserved);
                    memberRepository.save(member);
                    return;
                }

                // 추가 차감
                member.subtractMemberMoney(additionalAmount);
                memberRepository.save(member);
//                log.info("체결가 차이로 인한 추가 차감: 주문ID={}, 추가액={}원", order.getOrderId(), additionalAmount);
            }

            // 수수료를 전체 통계에 추가
            feeTaxService.addBuyFee(actualFee);

            // 거래 내역 생성
            TradeHistory tradeHistory = TradeHistory.of(
                    member,
                    stock,
                    TradeHistory.TradeType.BUY,
                    order.getQuantity(),
                    currentPrice,
                    actualTotalAmount,
                    actualFee,
                    0L,
                    LocalDateTime.now()
            );
            tradeHistoryRepository.save(tradeHistory);

            // 포지션 업데이트 및 투자 요약 업데이트
            updateHoldingPosition(member, stock, order.getQuantity(), currentPrice, TradeHistory.TradeType.BUY);
            updateInvestmentSummary(member);

            // 주문 상태 업데이트
            order.complete();
            pendingOrderRepository.save(order);

            // 체결된 주문의 구독 목적 해제
            String pendingOrderPurpose = "PENDING_ORDER_" + order.getOrderId();
            kiwoomWebSocketClient.unsubscribeStockForPurpose(stockCode, pendingOrderPurpose);

            // 구독 여부 확인 및 포트폴리오 업데이트
            if (!isStockNeededElsewhere(stockCode)) {
                unsubscribeStockAfterTrade(stockCode);
            }

            try {
                portfolioWebSocketHandler.sendFullPortfolioUpdate(member.getMemberId());
            } catch (Exception e) {
                log.warn("포트폴리오 업데이트 알림 실패: {}", e.getMessage());
            }

            // 체결 정보 WebSocket 전송
            try {
                String timestamp = tradeHistory.getTradedAt().format(java.time.format.DateTimeFormatter.ofPattern("HHmmss"));
                stockWebSocketHandler.broadcastTradeExecution(
                        stockCode,
                        "BUY",
                        order.getQuantity(),
                        currentPrice,
                        actualTotalAmount,
                        timestamp
                );
//                log.info("지정가 매수 체결 정보 WebSocket 전송 성공: 종목={}, 가격={}, 수량={}",
//                        stockCode, currentPrice, order.getQuantity());
            } catch (Exception e) {
                log.warn("지정가 매수 체결 정보 WebSocket 전송 실패: {}", e.getMessage());
            }

            successfulTrades.incrementAndGet();

            // 여기에 알림 전송 코드 추가
            try {
                notificationService.createTradeNotification(
                        member.getMemberId(),
                        stock.getShortName(),
                        "BUY",
                        order.getQuantity(),
                        currentPrice
                );
                log.info("지정가 매수 체결 알림 전송 성공: 회원ID={}, 종목={}", member.getMemberId(), stock.getShortName());
            } catch (Exception e) {
                log.warn("지정가 매수 체결 알림 전송 실패: {}", e.getMessage());
                // 알림 실패는 거래 자체에 영향을 주지 않으므로 예외 전파하지 않음
            }
        } catch (Exception e) {
            handleOrderExecutionFailure(order, "매수", e);
        }
    }

    // 매도 주문 처리 메서드
    @Transactional
    private void executeSellOrderWithFreshData(PendingOrder order, Long currentPrice) {
        if (order.getStatus() != PendingOrder.OrderStatus.PENDING) {
            return;
        }

        try {
            Member member = order.getMember();
            StockData stock = order.getStockData();
            String stockCode = stock.getShortCode();

            // 주문 상태를 먼저 처리 중으로 변경
            order.processing();
            pendingOrderRepository.save(order);

            // 보유 수량 확인
            HoldingPosition position = holdingPositionRepository.findByMemberAndStockData(member, stock)
                    .orElseThrow(() -> new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "해당 종목을 보유하고 있지 않습니다"));

            if (position.getQuantity() < order.getQuantity()) {
                order.fail();
                pendingOrderRepository.save(order);
                failedTrades.incrementAndGet();
                throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "보유 수량이 부족합니다");
            }

            // 거래 내역 생성
            long totalAmount = currentPrice * order.getQuantity();

            // 수수료 및 세금 계산
            Long fee = feeTaxService.calculateSellFee(totalAmount);
            Long tax = feeTaxService.calculateSellTax(totalAmount);

            // 순 매도 금액 (수수료와 세금 제외)
            Long netAmount = totalAmount - fee - tax;

            // 매도 처리 (순 금액만 입금)
            member.addMemberMoney(netAmount);
            memberRepository.save(member);

            // 수수료와 세금을 전체 통계에 추가
            feeTaxService.addSellFee(fee);
            feeTaxService.addSellTax(tax);

            TradeHistory tradeHistory = TradeHistory.of(
                    member,
                    stock,
                    TradeHistory.TradeType.SELL,
                    order.getQuantity(),
                    currentPrice,
                    totalAmount,
                    fee,    // 매도 수수료
                    tax,    // 매도 세금
                    LocalDateTime.now()
            );
            tradeHistoryRepository.save(tradeHistory);

            // 포지션 업데이트 및 투자 요약 업데이트
            updateHoldingPosition(member, stock, order.getQuantity(), currentPrice, TradeHistory.TradeType.SELL);
            updateInvestmentSummary(member);

            // 주문 상태 업데이트
            order.complete();
            pendingOrderRepository.save(order);

            // 체결된 주문의 구독 목적 해제
            String pendingOrderPurpose = "PENDING_ORDER_" + order.getOrderId();
            kiwoomWebSocketClient.unsubscribeStockForPurpose(stockCode, pendingOrderPurpose);

            // 구독 여부 확인 및 포트폴리오 업데이트
            if (!isStockNeededElsewhere(stockCode)) {
                unsubscribeStockAfterTrade(stockCode);
            }

            try {
                portfolioWebSocketHandler.sendFullPortfolioUpdate(member.getMemberId());
            } catch (Exception e) {
                log.warn("포트폴리오 업데이트 알림 실패: {}", e.getMessage());
            }

            // 추가: 체결 정보 WebSocket 전송
            try {
                // 시간 포맷 변환 (HHmmss 형식으로)
                String timestamp = tradeHistory.getTradedAt().format(java.time.format.DateTimeFormatter.ofPattern("HHmmss"));
                stockWebSocketHandler.broadcastTradeExecution(
                        stockCode,
                        "SELL",
                        order.getQuantity(),
                        currentPrice,
                        totalAmount,
                        timestamp
                );
//                log.info("지정가 매도 체결 정보 WebSocket 전송 성공: 종목={}, 가격={}, 수량={}",
//                        stockCode, currentPrice, order.getQuantity());
            } catch (Exception e) {
                log.warn("지정가 매도 체결 정보 WebSocket 전송 실패: {}", e.getMessage());
                // 웹소켓 전송 실패는 거래 자체에 영향을 주지 않으므로 예외 전파하지 않음
            }

            successfulTrades.incrementAndGet();

            // 여기에 알림 전송 코드 추가
            try {
                notificationService.createTradeNotification(
                        member.getMemberId(),
                        stock.getShortName(),
                        "SELL",
                        order.getQuantity(),
                        currentPrice
                );
//                log.info("지정가 매도 체결 알림 전송 성공: 회원ID={}, 종목={}", member.getMemberId(), stock.getShortName());
            } catch (Exception e) {
                log.warn("지정가 매도 체결 알림 전송 실패: {}", e.getMessage());
                // 알림 실패는 거래 자체에 영향을 주지 않으므로 예외 전파하지 않음
            }
        } catch (Exception e) {
            handleOrderExecutionFailure(order, "매도", e);
        }
    }

    // 주문 실패 처리 공통 메서드
    private void handleOrderExecutionFailure(PendingOrder order, String orderType, Exception e) {
        log.error("지정가 {} 주문 체결 중 오류 발생: {}", orderType, e.getMessage(), e);

        try {
            // 오류 발생 시 주문 상태 실패로 변경
            order.fail();
            pendingOrderRepository.save(order);

            // 안전하게 stockCode 추출
            String stockCode = "unknown";
            try {
                stockCode = order.getStockData().getShortCode();
            } catch (Exception ex) {
                log.warn("실패 처리 중 종목 코드 조회 실패", ex);
            }

            // 매수 주문이 실패하면 전체 금액 환불 처리
            if (order.getOrderType() == TradeHistory.TradeType.BUY) {
                try {
                    refundMoneyOnOrderFailure(order.getMember(), order.getTargetPrice() * order.getQuantity());
                } catch (Exception ex) {
                    log.error("주문 실패 후 환불 처리 중 오류", ex);
                }
            }

            // 다른 지정가 주문이 없고, 포트폴리오에도 없으면 구독 해제
            if (!isStockNeededElsewhere(stockCode)) {
                unsubscribeStockAfterTrade(stockCode);
            }

            failedTrades.incrementAndGet();
        } catch (Exception ex) {
            log.error("주문 실패 처리 중 추가 오류 발생", ex);
        }
    }

    @Override
    public void onStockBidAskUpdate(String stockCode, JsonNode data) {
        // 호가 업데이트는 별도 처리 필요 없음
    }

    private TradeResponseDTO createErrorResponse(String message) {
        TradeResponseDTO response = new TradeResponseDTO();
        response.setStatus("ERROR");
        response.setMessage(message);
        return response;
    }

    /**
     * 회원의 미체결 주문 목록 조회
     */
    @Transactional(isolation = Isolation.READ_COMMITTED, readOnly = true)
    public List<PendingOrderDTO> getPendingOrdersByMember(String authorizationHeader) {
        try {
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            List<PendingOrder> pendingOrders = pendingOrderRepository
                    .findByMemberAndStatusOrderByCreatedAtDesc(member, PendingOrder.OrderStatus.PENDING);

            return pendingOrders.stream()
                    .map(PendingOrderDTO::fromEntity)
                    .collect(Collectors.toList());

        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("미체결 주문 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.ORDER_INQUIRY_FAILED);
        }
    }

    /**
     * 지정가 주문 취소 처리
     */
    @Transactional
    public boolean cancelPendingOrder(String authorizationHeader, Long orderId) {
        try {
            // 토큰에서 회원 정보 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            // 주문 조회
            PendingOrder order = pendingOrderRepository.findById(orderId)
                    .orElseThrow(() -> new CustomException(StockErrorCode.ORDER_NOT_FOUND));

            // 본인 주문인지 확인
            if (!order.getMember().getMemberId().equals(memberId)) {
                throw new CustomException(StockErrorCode.UNAUTHORIZED_ORDER_ACCESS);
            }

            // 취소 가능한 상태인지 확인
            if (!order.isCancelable()) {
                throw new CustomException(StockErrorCode.ORDER_NOT_CANCELABLE);
            }

            // 매수 주문 취소 시 예약금 환불
            if (order.getOrderType() == TradeHistory.TradeType.BUY) {
                Long refundAmount = order.getTargetPrice() * order.getQuantity() + order.getReservedFee();
                Long newBalance = order.getMember().getMemberMoney() + refundAmount;
                order.getMember().updateMemberMoney(newBalance);
                memberRepository.save(order.getMember());

                log.info("매수 주문 취소로 인한 예약금 환불: {} 원", refundAmount);
            }

            // 주문 상태를 취소로 변경
            order.cancel();
            pendingOrderRepository.save(order);

            log.info("지정가 주문 취소 완료 - 주문 ID: {}, 회원 ID: {}", orderId, memberId);

            return true;

        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("주문 취소 처리 중 오류 발생", e);
            return false;
        }
    }

    // 개별 주문을 별도 트랜잭션으로 처리
    @Transactional
    public void processOrderWithTransaction(PendingOrder order, Long currentPrice) {
        try {
            // 최신 상태 확인을 위해 주문 재조회
            PendingOrder freshOrder = pendingOrderRepository.findById(order.getOrderId())
                    .orElse(null);

            if (freshOrder == null || freshOrder.getStatus() != PendingOrder.OrderStatus.PENDING) {
                return;
            }

            if (freshOrder.getOrderType() == TradeHistory.TradeType.BUY) {
                // 매수 주문: 지정가가 현재가보다 크거나 같으면 체결
                if (freshOrder.getTargetPrice() >= currentPrice) {
//                    log.info("매수 주문 체결 조건 충족: 주문ID={}, 지정가={}, 현재가={}",
//                            freshOrder.getOrderId(), freshOrder.getTargetPrice(), currentPrice);
                    executeBuyOrderWithFreshData(freshOrder, currentPrice);
                }
            } else {
                // 매도 주문: 지정가가 현재가보다 작거나 같으면 체결
                if (freshOrder.getTargetPrice() <= currentPrice) {
//                    log.info("매도 주문 체결 조건 충족: 주문ID={}, 지정가={}, 현재가={}",
//                            freshOrder.getOrderId(), freshOrder.getTargetPrice(), currentPrice);
                    executeSellOrderWithFreshData(freshOrder, currentPrice);
                }
            }
        } catch (Exception e) {
            log.error("주문 처리 중 오류 발생: orderId={}, error={}",
                    order.getOrderId(), e.getMessage());
        }
    }

    // 트레이드 성공/실패 메트릭 반환
    public Map<String, Integer> getTradeMetrics() {
        Map<String, Integer> metrics = new HashMap<>();
        metrics.put("successfulTrades", successfulTrades.get());
        metrics.put("failedTrades", failedTrades.get());
        return metrics;
    }

    // 시스템 상태 모니터링 메서드
    public boolean checkSystemHealth() {
        // 키움 웹소켓 연결 상태 확인
        boolean socketConnected = kiwoomWebSocketClient.isConnected();

        // 주요 종목 가격 데이터 확인 (KOSPI 지수)
        Long kospiPrice = getCurrentStockPrice("KS11");

        // 시스템이 정상이면 true 반환
        return socketConnected && kospiPrice != null;
    }

    /**
     * 지정가 주문에 대한 즉시 체결 확인 요청
     */
    public void checkAndExecutePendingOrder(PendingOrder order) {
        try {
            // 최신 상태 확인을 위해 주문 재조회
            PendingOrder freshOrder = pendingOrderRepository.findById(order.getOrderId())
                    .orElse(null);

            if (freshOrder == null || freshOrder.getStatus() != PendingOrder.OrderStatus.PENDING) {
                log.debug("주문 ID {}는 이미 처리되었거나 존재하지 않습니다.", order.getOrderId());
                return;
            }

            // 캐시 또는 웹소켓에서만 가격 확인 (API 호출 없음)
            Long currentPrice = null;
            String stockCode = freshOrder.getStockData().getShortCode();
            String purpose = "PENDING_ORDER_" + freshOrder.getOrderId();
            boolean temporarySubscription = false;

            // 1. 캐시에서 가격 확인
            currentPrice = stockPriceCacheService.getFromCacheOnly(stockCode);

            // 2. 캐시에 없고 웹소켓 구독 중이면 웹소켓에서 확인
            if (currentPrice == null) {
                if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
                    // 임시 구독 시작
//                    log.info("지정가 주문 체결 확인을 위한 임시 구독 시작: {}", stockCode);
                    temporarySubscription = kiwoomWebSocketClient.subscribeStockWithPurpose(stockCode, purpose);

                    if (temporarySubscription) {
                        // 데이터 수신 대기
                        try {
                            Thread.sleep(500);
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }

                // 가격 조회 시도
                currentPrice = kiwoomWebSocketClient.getLatestPrice(stockCode);

                // 임시 구독했다면 즉시 해제
                if (temporarySubscription) {
                    try {
//                        log.info("지정가 주문 체결 확인 후 임시 구독 해제: {}", stockCode);
                        kiwoomWebSocketClient.unsubscribeStockForPurpose(stockCode, purpose);
                    } catch (Exception e) {
                        log.warn("임시 구독 해제 중 오류 발생: {}", e.getMessage());
                    }
                }
            }

            // 현재가를 얻었으면 체결 조건 확인
            if (currentPrice != null) {
                boolean shouldExecute = false;
                if (freshOrder.getOrderType() == TradeHistory.TradeType.BUY) {
                    // 매수는 지정가 >= 현재가일 때 체결
                    shouldExecute = freshOrder.getTargetPrice() >= currentPrice;
                } else {
                    // 매도는 지정가 <= 현재가일 때 체결
                    shouldExecute = freshOrder.getTargetPrice() <= currentPrice;
                }

                if (shouldExecute) {
//                    log.info("지정가 주문 즉시 체결 조건 충족: ID={}, 타입={}, 지정가={}, 현재가={}",
//                            freshOrder.getOrderId(), freshOrder.getOrderType(), freshOrder.getTargetPrice(), currentPrice);

                    // 이벤트 발행 - 주문 체결 요청
                    tradeEventService.requestOrderExecution(freshOrder, currentPrice);
                } else {
                    log.debug("지정가 주문 즉시 체결 불가: ID={}, 타입={}, 지정가={}, 현재가={}",
                            freshOrder.getOrderId(), freshOrder.getOrderType(), freshOrder.getTargetPrice(), currentPrice);
                }
            } else {
                // 현재가를 얻지 못했지만 오류는 아님 - 스케줄러가 나중에 처리할 것임
                log.debug("지정가 주문 ID={}의 현재가를 얻지 못했습니다. 스케줄러가 나중에 처리할 것입니다.", freshOrder.getOrderId());
            }
        } catch (Exception e) {
            log.error("즉시 체결 확인 중 오류 발생: {}", e.getMessage(), e);
        }
    }

    @Transactional(isolation = Isolation.READ_COMMITTED)
    public TradeResponseDTO buyStock(String authorizationHeader, TradeRequestDTO request) {
        try {
            // 토큰에서 회원 정보 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 종목 정보 조회
            StockData stockData = stockDataRepository.findByShortCode(request.getStockCode())
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            Integer quantity = request.getQuantity();
            if (quantity == null || quantity <= 0) {
                throw new CustomException(StockErrorCode.INVALID_QUANTITY);
            }

            // 시장가 주문인 경우
            if (Boolean.TRUE.equals(request.getMarketOrder())) {
                // 락 획득 시도하지 않고 바로 처리
                return processMarketBuyOrder(member, stockData, quantity);
            }

            // 지정가 주문인 경우
            if (request.getPrice() == null || request.getPrice() <= 0) {
                throw new CustomException(StockErrorCode.INVALID_PRICE);
            }

            return processLimitBuyOrder(member, stockData, quantity, request.getPrice());
        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("매수 처리 중 오류 발생", e);
            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED);
        }
    }

    @Transactional(isolation = Isolation.READ_COMMITTED)
    public TradeResponseDTO sellStock(String authorizationHeader, TradeRequestDTO request) {
        try {
            // 토큰에서 회원 정보 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 종목 정보 조회
            StockData stockData = stockDataRepository.findByShortCode(request.getStockCode())
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 보유 포지션 확인
            List<HoldingPosition> positions = holdingPositionRepository
                    .findAllByMemberAndStockDataAndActiveTrue(member, stockData);
            if (positions.isEmpty()) {
                throw new CustomException(StockErrorCode.NO_HOLDING_POSITION);
            }
            HoldingPosition position = positions.get(0);

            // 현재 대기 중인 매도 주문 수량 조회
            int pendingSellQuantity = pendingOrderRepository.getTotalPendingSellQuantityForMemberAndStock(
                    member.getMemberId(), stockData.getShortCode());

            // 실제 매도 가능 수량 계산 (보유 수량 - 대기 중인 매도 주문 수량)
            int availableQuantity = position.getQuantity() - pendingSellQuantity;

            if (availableQuantity < request.getQuantity()) {
                throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK_QUANTITY,
                        String.format("매도 가능 수량이 부족합니다. 보유: %d주, 대기 중인 매도 주문: %d주, 가능: %d주, 요청: %d주",
                                position.getQuantity(), pendingSellQuantity, availableQuantity, request.getQuantity()));
            }

            // 시장가 주문인 경우 - 락 획득 시도하지 않고 바로 처리
            if (Boolean.TRUE.equals(request.getMarketOrder())) {
                return processMarketSellOrder(member, stockData, request.getQuantity(), position);
            }

            // 지정가 주문인 경우
            if (request.getPrice() == null || request.getPrice() <= 0) {
                throw new CustomException(StockErrorCode.INVALID_PRICE);
            }

            return processLimitSellOrder(member, stockData, request.getQuantity(), request.getPrice());
        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("매도 처리 중 오류 발생", e);
            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED);
        }
    }

    // StockTradeService 클래스에 추가할 메서드들

    /**
     * 직접 가격을 입력받아 매수 처리 (캐시된 가격 사용 시)
     */
    @Transactional(isolation = Isolation.READ_COMMITTED)
    public TradeResponseDTO executeBuyWithPrice(Member member, TradeRequestDTO request, Long currentPrice) {
        try {
            validateTradeRequest(request);

            StockData stock = stockDataRepository.findByShortCode(request.getStockCode())
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 데드락 방지를 위해 항상 작은 ID의 락부터 획득
            Long memberId = member.getMemberId();
            String stockCode = request.getStockCode();

            ReentrantLock lock1, lock2;
            String numericStockCode = stockCode.replaceAll("[^0-9]", "");
            boolean useMemberLockFirst = memberId < (numericStockCode.isEmpty() ? 0 : Long.parseLong(numericStockCode));

            if (useMemberLockFirst) {
                lock1 = getMemberLock(memberId);
                lock2 = getStockLock(stockCode);
            } else {
                lock1 = getStockLock(stockCode);
                lock2 = getMemberLock(memberId);
            }

            try {
                lock1.lock();
                try {
                    lock2.lock();
                    try {
                        // 총 구매 금액 계산
                        Long totalAmount = currentPrice * request.getQuantity();

                        // 수수료 계산
                        Long fee = feeTaxService.calculateBuyFee(totalAmount);

                        // 총 구매 금액에 수수료 추가
                        Long totalAmountWithFee = totalAmount + fee;

                        // 최신 회원 정보 조회
                        member = memberRepository.findById(member.getMemberId())
                                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

                        // 잔액 확인
                        if (member.getMemberMoney() < totalAmountWithFee) {
                            failedTrades.incrementAndGet();
                            throw new CustomException(StockErrorCode.INSUFFICIENT_BALANCE);
                        }

                        // 매수 처리 (수수료 포함)
                        member.subtractMemberMoney(totalAmountWithFee);
                        memberRepository.save(member);

                        // 수수료를 전체 통계에 추가
                        feeTaxService.addBuyFee(fee);

                        // 거래 내역 생성
                        TradeHistory tradeHistory = TradeHistory.of(
                                member,
                                stock,
                                TradeHistory.TradeType.BUY,
                                request.getQuantity(),
                                currentPrice,
                                totalAmount,
                                fee,
                                0L,
                                LocalDateTime.now()
                        );
                        tradeHistoryRepository.save(tradeHistory);

                        // 비활성화된 기존 포지션 확인
                        updateHoldingPositionForBuy(member, stock, request.getQuantity(), currentPrice);

                        // 투자 요약 업데이트
                        updateInvestmentSummary(member);

                        // 가격 캐시 무효화
                        stockPriceCacheService.invalidateCache(stock.getShortCode());

                        successfulTrades.incrementAndGet();

                        // 체결 정보 WebSocket 전송
                        try {
                            String timestamp = tradeHistory.getTradedAt().format(DateTimeFormatter.ofPattern("HHmmss"));
                            stockWebSocketHandler.broadcastTradeExecution(
                                    stock.getShortCode(),
                                    "BUY",
                                    request.getQuantity(),
                                    currentPrice,
                                    totalAmount,
                                    timestamp
                            );
                        } catch (Exception e) {
                            log.warn("체결 정보 WebSocket 전송 실패: {}", e.getMessage());
                        }

                        return TradeResponseDTO.fromTradeHistory(tradeHistory);
                    } catch (Exception e) {
                        log.error("매수 주문 처리 중 오류 발생", e);
                        failedTrades.incrementAndGet();
                        throw e;
                    } finally {
                        lock2.unlock();
                    }
                } finally {
                    lock1.unlock();
                }
            } catch (Exception e) {
                log.error("락 획득 중 오류 발생", e);
                throw e;
            }
        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("매수 처리 중 오류 발생", e);
            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED);
        }
    }

    /**
     * 직접 가격을 입력받아 매도 처리 (캐시된 가격 사용 시)
     */
    @Transactional(isolation = Isolation.READ_COMMITTED)
    public TradeResponseDTO executeSellWithPrice(Member member, TradeRequestDTO request, Long currentPrice) {
        try {
            validateTradeRequest(request);

            StockData stock = stockDataRepository.findByShortCode(request.getStockCode())
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 데드락 방지를 위해 항상 작은 ID의 락부터 획득
            Long memberId = member.getMemberId();
            String stockCode = request.getStockCode();

            ReentrantLock lock1, lock2;
            String numericStockCode = stockCode.replaceAll("[^0-9]", "");
            boolean useMemberLockFirst = memberId < (numericStockCode.isEmpty() ? 0 : Long.parseLong(numericStockCode));

            if (useMemberLockFirst) {
                lock1 = getMemberLock(memberId);
                lock2 = getStockLock(stockCode);
            } else {
                lock1 = getStockLock(stockCode);
                lock2 = getMemberLock(memberId);
            }

            try {
                lock1.lock();
                try {
                    lock2.lock();
                    try {
                        // 최신 회원 정보 조회
                        member = memberRepository.findById(member.getMemberId())
                                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

                        // 보유 수량 확인
                        HoldingPosition position = holdingPositionRepository.findByMemberAndStockDataAndActiveTrue(member, stock)
                                .orElseThrow(() -> new CustomException(StockErrorCode.INSUFFICIENT_STOCK));

                        // 현재 대기 중인 매도 주문 수량 조회
                        int pendingSellQuantity = pendingOrderRepository.getTotalPendingSellQuantityForMemberAndStock(
                                member.getMemberId(), stock.getShortCode());

                        // 실제 매도 가능 수량 계산
                        int availableQuantity = position.getQuantity() - pendingSellQuantity;

                        if (availableQuantity < request.getQuantity()) {
                            failedTrades.incrementAndGet();
                            throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK_QUANTITY);
                        }

                        // 포지션 업데이트
                        updateHoldingPositionForSell(position, request.getQuantity(), currentPrice);

                        // 총 판매 금액 계산
                        Long totalAmount = currentPrice * request.getQuantity();

                        // 수수료 및 세금 계산
                        Long fee = feeTaxService.calculateSellFee(totalAmount);
                        Long tax = feeTaxService.calculateSellTax(totalAmount);

                        // 순 매도 금액
                        Long netAmount = totalAmount - fee - tax;

                        // 매도 처리 (순 금액만 입금)
                        member.addMemberMoney(netAmount);
                        memberRepository.save(member);

                        // 수수료와 세금을 전체 통계에 추가
                        feeTaxService.addSellFee(fee);
                        feeTaxService.addSellTax(tax);

                        // 거래 내역 생성
                        TradeHistory tradeHistory = TradeHistory.of(
                                member,
                                stock,
                                TradeHistory.TradeType.SELL,
                                request.getQuantity(),
                                currentPrice,
                                totalAmount,
                                fee,
                                tax,
                                LocalDateTime.now()
                        );
                        tradeHistoryRepository.save(tradeHistory);

                        // 투자 요약 업데이트
                        updateInvestmentSummary(member);

                        // 가격 캐시 무효화
                        stockPriceCacheService.invalidateCache(stock.getShortCode());

                        successfulTrades.incrementAndGet();

                        // 체결 정보 WebSocket 전송
                        try {
                            String timestamp = tradeHistory.getTradedAt().format(DateTimeFormatter.ofPattern("HHmmss"));
                            stockWebSocketHandler.broadcastTradeExecution(
                                    stock.getShortCode(),
                                    "SELL",
                                    request.getQuantity(),
                                    currentPrice,
                                    totalAmount,
                                    timestamp
                            );
                        } catch (Exception e) {
                            log.warn("체결 정보 WebSocket 전송 실패: {}", e.getMessage());
                        }

                        return TradeResponseDTO.fromTradeHistory(tradeHistory);
                    } catch (Exception e) {
                        log.error("매도 주문 처리 중 오류 발생", e);
                        failedTrades.incrementAndGet();
                        throw e;
                    } finally {
                        lock2.unlock();
                    }
                } finally {
                    lock1.unlock();
                }
            } catch (Exception e) {
                log.error("락 획득 중 오류 발생", e);
                throw e;
            }
        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("매도 처리 중 오류 발생", e);
            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED);
        }
    }

    // StockData 가져오기 헬퍼 메서드
    public StockData getStockData(String stockCode) {
        return stockDataRepository.findByShortCode(stockCode)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));
    }

    private void updateHoldingPositionForBuy(Member member, StockData stock, int quantity, Long price) {
        Optional<HoldingPosition> existingPosition = holdingPositionRepository
                .findByMemberAndStockData(member, stock);

        if (existingPosition.isPresent()) {
            HoldingPosition position = existingPosition.get();

            if (!position.getActive()) {
                // 비활성화된 포지션 재활성화
                position.activate();
                position.updatePosition(
                        quantity,
                        price,
                        0L,
                        0.0
                );
            } else {
                // 기존 활성 포지션 업데이트 (평균 매수가 계산)
                int newQuantity = position.getQuantity() + quantity;
                long newAvgPrice = ((position.getAveragePrice() * position.getQuantity()) + (price * quantity)) / newQuantity;

                long currentProfit = (price - newAvgPrice) * newQuantity;
                double returnRate = ((double) price / newAvgPrice - 1.0) * 100.0;

                position.updatePosition(newQuantity, newAvgPrice, currentProfit, returnRate);
            }
            holdingPositionRepository.save(position);
            holdingPositionRepository.flush(); // 즉시 DB 반영
        } else {
            // 새 포지션 생성
            HoldingPosition newPosition = HoldingPosition.of(
                    member,
                    stock,
                    quantity,
                    price,
                    0L,
                    0.0
            );
            holdingPositionRepository.save(newPosition);
            holdingPositionRepository.flush(); // 즉시 DB 반영
            member.addHoldingPosition(newPosition);
        }
    }

    // 매도용 포지션 업데이트 헬퍼 메서드
    private void updateHoldingPositionForSell(HoldingPosition position, int quantity, Long price) {
        int newQuantity = position.getQuantity() - quantity;

        if (newQuantity == 0) {
            // 포지션 비활성화
            position.updatePosition(0, position.getAveragePrice(), 0L, 0.0);
            position.deactivate();
        } else {
            // 보유량 감소만 처리
            long currentProfit = (price - position.getAveragePrice()) * newQuantity;
            double returnRate = ((double) price / position.getAveragePrice() - 1.0) * 100.0;

            position.updatePosition(newQuantity, position.getAveragePrice(), currentProfit, returnRate);
        }

        holdingPositionRepository.save(position);
        holdingPositionRepository.flush();
    }


    public int getAvailableSellQuantity(String authorizationHeader, String shortCode) {
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        StockData stockData = stockDataRepository.findByShortCode(shortCode)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        // 보유 수량 조회
        int holdingQty = holdingPositionRepository
                .findByMemberAndStockData(member, stockData)
                .map(HoldingPosition::getQuantity)
                .orElse(0);

        // 팬딩 매도 주문 수량 합산
        List<PendingOrder> pendingOrders =
                pendingOrderRepository.findByMemberAndStockDataAndOrderTypeAndStatus(
                        member,
                        stockData,
                        TradeHistory.TradeType.SELL,
                        PendingOrder.OrderStatus.PENDING
                );
        int pendingSellQty = pendingOrders.stream()
                .mapToInt(PendingOrder::getQuantity)
                .sum();

        return Math.max(0, holdingQty - pendingSellQty);
    }
}