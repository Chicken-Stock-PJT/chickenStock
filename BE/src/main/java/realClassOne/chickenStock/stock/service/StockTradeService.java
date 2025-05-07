package realClassOne.chickenStock.stock.service;

import com.fasterxml.jackson.databind.JsonNode;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.InvestmentSummary;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.dto.common.PendingOrderDTO;
import realClassOne.chickenStock.stock.dto.request.TradeRequestDTO;
import realClassOne.chickenStock.stock.dto.response.InitializeMoneyResponseDTO;
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
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;
import realClassOne.chickenStock.stock.websocket.handler.PortfolioWebSocketHandler;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class StockTradeService implements KiwoomWebSocketClient.StockDataListener {

    private final MemberRepository memberRepository;
    private final StockDataRepository stockDataRepository;
    private final TradeHistoryRepository tradeHistoryRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final PendingOrderRepository pendingOrderRepository;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final StockSubscriptionService stockSubscriptionService;
    private final JwtTokenProvider jwtTokenProvider;
    private final PortfolioWebSocketHandler portfolioWebSocketHandler;

    // 동시성 제어를 위한 락 추가
    private final Map<String, ReentrantLock> stockLocks = new ConcurrentHashMap<>();
    // 회원 ID 기반 락 추가 (자금 관련 동시성 제어)
    private final Map<Long, ReentrantLock> memberLocks = new ConcurrentHashMap<>();

    // 트레이드 성공 여부를 추적하기 위한 메트릭
    private final AtomicInteger successfulTrades = new AtomicInteger(0);
    private final AtomicInteger failedTrades = new AtomicInteger(0);

    @PostConstruct
    public void init() {
        try {
            kiwoomWebSocketClient.addListener(this);
            log.info("StockTradeService 초기화 완료 - 실시간 가격 리스너 등록");
        } catch (Exception e) {
            log.error("StockTradeService 초기화 실패", e);
            // 초기화 실패 시 서비스 재시작 로직 추가
            throw new RuntimeException("StockTradeService 초기화 실패", e);
        }
    }

    // 특정 종목에 대한 락 획득
    private ReentrantLock getStockLock(String stockCode) {
        return stockLocks.computeIfAbsent(stockCode, k -> new ReentrantLock());
    }

    // 특정 회원에 대한 락 획득 (자금 관련 동시성 제어)
    private ReentrantLock getMemberLock(Long memberId) {
        return memberLocks.computeIfAbsent(memberId, k -> new ReentrantLock());
    }

    // 매수 메서드
    @Transactional
    public TradeResponseDTO buyStock(String authorizationHeader, TradeRequestDTO request) {
        // 입력값 검증
        validateTradeRequest(request);

        String token = jwtTokenProvider.resolveToken(authorizationHeader);

        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        StockData stock = stockDataRepository.findByShortCode(request.getStockCode())
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND, "해당 종목을 찾을 수 없습니다: " + request.getStockCode()));

        // 동시성 제어를 위한 락 획득
        ReentrantLock stockLock = getStockLock(request.getStockCode());
        ReentrantLock memberLock = getMemberLock(memberId);

        // 데드락 방지를 위해 항상 동일한 순서로 락 획득
        memberLock.lock();
        try {
            stockLock.lock();
            try {
                // 시장가 주문인 경우 실시간 가격 조회
                Long currentPrice;
                boolean temporarySubscription = false;

                if (Boolean.TRUE.equals(request.getMarketOrder())) {
                    // 임시 구독 필요 여부 확인
                    temporarySubscription = !kiwoomWebSocketClient.isSubscribed(request.getStockCode());

                    // 현재가 조회
                    currentPrice = getCurrentStockPriceWithRetry(request.getStockCode());
                    if (currentPrice == null) {
                        failedTrades.incrementAndGet();
                        throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE);
                    }
                } else {
                    // 지정가 주문 처리
                    return createPendingBuyOrder(member, stock, request);
                }

                // 총 구매 금액 계산
                Long totalAmount = currentPrice * request.getQuantity();

                // 잔액 확인 - 재조회하여 최신 데이터로 확인
                member = memberRepository.findById(memberId)
                        .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

                if (member.getMemberMoney() < totalAmount) {
                    failedTrades.incrementAndGet();
                    throw new CustomException(StockErrorCode.INSUFFICIENT_BALANCE);
                }

                // 매수 처리
                member.subtractMemberMoney(totalAmount);
                memberRepository.save(member);

                // 거래 내역 생성
                TradeHistory tradeHistory = TradeHistory.of(
                        member,
                        stock,
                        TradeHistory.TradeType.BUY,
                        request.getQuantity(),
                        currentPrice,
                        totalAmount,
                        LocalDateTime.now()
                );
                tradeHistoryRepository.save(tradeHistory);

                // 포지션 업데이트
                updateHoldingPosition(member, stock, request.getQuantity(), currentPrice, TradeHistory.TradeType.BUY);

                // 투자 요약 업데이트
                updateInvestmentSummary(member);

                // 임시 구독이었다면 구독 해제
                if (temporarySubscription) {
                    unsubscribeStockAfterTrade(request.getStockCode());
                }

                successfulTrades.incrementAndGet();
                return TradeResponseDTO.fromTradeHistory(tradeHistory);
            } catch (Exception e) {
                log.error("매수 주문 처리 중 오류 발생", e);
                failedTrades.incrementAndGet();
                throw e;
            } finally {
                stockLock.unlock();
            }
        } finally {
            memberLock.unlock();
        }
    }

    // 오버로딩
    @Transactional
    public TradeResponseDTO buyStock(TradeRequestDTO request, Member member) {
        // 입력값 검증
        validateTradeRequest(request);

        StockData stock = stockDataRepository.findByShortCode(request.getStockCode())
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND, "해당 종목을 찾을 수 없습니다: " + request.getStockCode()));

        // 동시성 제어를 위한 락 획득
        ReentrantLock stockLock = getStockLock(request.getStockCode());
        ReentrantLock memberLock = getMemberLock(member.getMemberId());

        // 데드락 방지를 위해 항상 동일한 순서로 락 획득
        memberLock.lock();
        try {
            stockLock.lock();
            try {
                // 시장가 주문인 경우 실시간 가격 조회
                Long currentPrice;
                boolean temporarySubscription = false;

                if (Boolean.TRUE.equals(request.getMarketOrder())) {
                    // 임시 구독 필요 여부 확인
                    temporarySubscription = !kiwoomWebSocketClient.isSubscribed(request.getStockCode());

                    currentPrice = getCurrentStockPriceWithRetry(request.getStockCode());
                    if (currentPrice == null) {
                        failedTrades.incrementAndGet();
                        throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE);
                    }
                } else {
                    // 지정가 주문 처리 - 여기에 들어오지 않아야 함
                    return createErrorResponse("내부 로직 오류: 시장가 주문이 아닌 요청이 들어왔습니다.");
                }

                // 총 구매 금액 계산
                Long totalAmount = currentPrice * request.getQuantity();

                // 잔액 확인 - 재조회하여 최신 데이터로 확인
                member = memberRepository.findById(member.getMemberId())
                        .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

                if (member.getMemberMoney() < totalAmount) {
                    failedTrades.incrementAndGet();
                    throw new CustomException(StockErrorCode.INSUFFICIENT_BALANCE);
                }

                // 매수 처리
                member.subtractMemberMoney(totalAmount);
                memberRepository.save(member);

                // 거래 내역 생성
                TradeHistory tradeHistory = TradeHistory.of(
                        member,
                        stock,
                        TradeHistory.TradeType.BUY,
                        request.getQuantity(),
                        currentPrice,
                        totalAmount,
                        LocalDateTime.now()
                );
                tradeHistoryRepository.save(tradeHistory);

                // 포지션 업데이트
                updateHoldingPosition(member, stock, request.getQuantity(), currentPrice, TradeHistory.TradeType.BUY);

                // 투자 요약 업데이트
                updateInvestmentSummary(member);

                // 임시 구독이었다면 구독 해제
                if (temporarySubscription) {
                    unsubscribeStockAfterTrade(request.getStockCode());
                }

                successfulTrades.incrementAndGet();
                return TradeResponseDTO.fromTradeHistory(tradeHistory);
            } catch (Exception e) {
                log.error("매수 주문 처리 중 오류 발생", e);
                failedTrades.incrementAndGet();
                throw e;
            } finally {
                stockLock.unlock();
            }
        } finally {
            memberLock.unlock();
        }
    }


    @Transactional
    public TradeResponseDTO sellStock(String authorizationHeader, TradeRequestDTO request) {
        // 입력값 검증
        validateTradeRequest(request);

        String token = jwtTokenProvider.resolveToken(authorizationHeader);

        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        StockData stock = stockDataRepository.findByShortCode(request.getStockCode())
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        // 동시성 제어를 위한 락 획득
        ReentrantLock stockLock = getStockLock(request.getStockCode());
        ReentrantLock memberLock = getMemberLock(memberId);

        // 데드락 방지를 위해 항상 동일한 순서로 락 획득
        memberLock.lock();
        try {
            stockLock.lock();
            try {
                // 보유 수량 확인 - 최신 데이터로 다시 조회
                HoldingPosition position = holdingPositionRepository.findByMemberAndStockData(member, stock)
                        .orElseThrow(() -> new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "해당 종목을 보유하고 있지 않습니다"));

                if (position.getQuantity() < request.getQuantity()) {
                    failedTrades.incrementAndGet();
                    throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "보유 수량이 부족합니다");
                }

                // 시장가 주문인 경우 실시간 가격 조회
                Long currentPrice;
                boolean temporarySubscription = false;

                if (Boolean.TRUE.equals(request.getMarketOrder())) {
                    // 임시 구독 필요 여부 확인
                    temporarySubscription = !kiwoomWebSocketClient.isSubscribed(request.getStockCode());

                    currentPrice = getCurrentStockPriceWithRetry(request.getStockCode());
                    if (currentPrice == null) {
                        failedTrades.incrementAndGet();
                        throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE);
                    }
                } else {
                    // 지정가 주문 처리
                    return createPendingSellOrder(member, stock, request);
                }

                // 총 판매 금액 계산
                Long totalAmount = currentPrice * request.getQuantity();

                // 매도 처리
                member.addMemberMoney(totalAmount);
                memberRepository.save(member);

                // 거래 내역 생성
                TradeHistory tradeHistory = TradeHistory.of(
                        member,
                        stock,
                        TradeHistory.TradeType.SELL,
                        request.getQuantity(),
                        currentPrice,
                        totalAmount,
                        LocalDateTime.now()
                );
                tradeHistoryRepository.save(tradeHistory);

                // 포지션 업데이트
                updateHoldingPosition(member, stock, request.getQuantity(), currentPrice, TradeHistory.TradeType.SELL);

                // 투자 요약 업데이트
                updateInvestmentSummary(member);

                // 임시 구독이었다면 구독 해제
                if (temporarySubscription) {
                    unsubscribeStockAfterTrade(request.getStockCode());
                }

                successfulTrades.incrementAndGet();
                return TradeResponseDTO.fromTradeHistory(tradeHistory);
            } catch (Exception e) {
                log.error("매도 주문 처리 중 오류 발생", e);
                failedTrades.incrementAndGet();
                throw e;
            } finally {
                stockLock.unlock();
            }
        } finally {
            memberLock.unlock();
        }
    }


    // 오버로딩
    @Transactional
    public TradeResponseDTO sellStock(TradeRequestDTO request, Member member) {
        // 입력값 검증
        validateTradeRequest(request);

        StockData stock = stockDataRepository.findByShortCode(request.getStockCode())
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        // 동시성 제어를 위한 락 획득
        ReentrantLock stockLock = getStockLock(request.getStockCode());
        ReentrantLock memberLock = getMemberLock(member.getMemberId());

        // 데드락 방지를 위해 항상 동일한 순서로 락 획득
        memberLock.lock();
        try {
            stockLock.lock();
            try {
                // 보유 수량 확인 - 최신 데이터로 다시 조회
                HoldingPosition position = holdingPositionRepository.findByMemberAndStockData(member, stock)
                        .orElseThrow(() -> new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "해당 종목을 보유하고 있지 않습니다"));

                if (position.getQuantity() < request.getQuantity()) {
                    failedTrades.incrementAndGet();
                    throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "보유 수량이 부족합니다");
                }

                // 시장가 주문인 경우 실시간 가격 조회
                Long currentPrice;
                boolean temporarySubscription = false;

                if (Boolean.TRUE.equals(request.getMarketOrder())) {
                    // 임시 구독 필요 여부 확인
                    temporarySubscription = !kiwoomWebSocketClient.isSubscribed(request.getStockCode());

                    currentPrice = getCurrentStockPriceWithRetry(request.getStockCode());
                    if (currentPrice == null) {
                        failedTrades.incrementAndGet();
                        throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE);
                    }
                } else {
                    // 지정가 주문 처리 - 여기에 들어오지 않아야 함
                    return createErrorResponse("내부 로직 오류: 시장가 주문이 아닌 요청이 들어왔습니다.");
                }

                // 총 판매 금액 계산
                Long totalAmount = currentPrice * request.getQuantity();

                // 매도 처리
                member.addMemberMoney(totalAmount);
                memberRepository.save(member);

                // 거래 내역 생성
                TradeHistory tradeHistory = TradeHistory.of(
                        member,
                        stock,
                        TradeHistory.TradeType.SELL,
                        request.getQuantity(),
                        currentPrice,
                        totalAmount,
                        LocalDateTime.now()
                );
                tradeHistoryRepository.save(tradeHistory);

                // 포지션 업데이트
                updateHoldingPosition(member, stock, request.getQuantity(), currentPrice, TradeHistory.TradeType.SELL);

                // 투자 요약 업데이트
                updateInvestmentSummary(member);

                // 임시 구독이었다면 구독 해제
                if (temporarySubscription) {
                    unsubscribeStockAfterTrade(request.getStockCode());
                }

                successfulTrades.incrementAndGet();
                return TradeResponseDTO.fromTradeHistory(tradeHistory);
            } catch (Exception e) {
                log.error("매도 주문 처리 중 오류 발생", e);
                failedTrades.incrementAndGet();
                throw e;
            } finally {
                stockLock.unlock();
            }
        } finally {
            memberLock.unlock();
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

    // 현재가 조회 메서드 (임시 구독 처리 추가됨)
    private Long getCurrentStockPriceWithRetry(String stockCode) {
        int maxRetries = 3;
        int retryCount = 0;
        long retryDelayMs = 300;
        boolean temporarySubscription = false;
        String purpose = "TEMPORARY_PRICE_CHECK_" + UUID.randomUUID();

        try {
            // 구독되어 있지 않으면 임시 구독
            if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
                log.info("종목 {} 임시 구독 시작 (목적: {})", stockCode, purpose);

                // 목적을 지정하여 구독
                boolean success = kiwoomWebSocketClient.subscribeStockWithPurpose(stockCode, purpose);
                temporarySubscription = success;

                if (!success) {
                    log.error("종목 {} 임시 구독 실패", stockCode);
                    return null;
                }

                // 데이터 수신 대기
                try {
                    Thread.sleep(500);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    log.warn("데이터 수신 대기 중 인터럽트 발생", e);
                }
            }

            // 가격 조회 시도
            Long price = null;
            while (retryCount < maxRetries) {
                price = getCurrentStockPrice(stockCode);
                if (price != null) {
                    break;
                }

                retryCount++;
                if (retryCount < maxRetries) {
                    try {
                        log.warn("{}번째 가격 조회 실패, {}ms 후 재시도", retryCount, retryDelayMs);
                        Thread.sleep(retryDelayMs);
                        retryDelayMs *= 2;
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        break;
                    }
                }
            }

            if (price == null) {
                log.error("종목 {} 가격 조회 최대 재시도 횟수 초과", stockCode);
            }

            return price;
        } catch (Exception e) {
            log.error("종목 {} 가격 조회 중 예외 발생", stockCode, e);
            return null;
        } finally {
            // 임시 구독이었다면 해당 목적의 구독만 해제
            if (temporarySubscription) {
                try {
                    log.info("임시 구독한 종목 {} 특정 목적({}) 구독 해제", stockCode, purpose);
                    kiwoomWebSocketClient.unsubscribeStockForPurpose(stockCode, purpose);
                } catch (Exception e) {
                    log.warn("종목 {} 구독 해제 중 오류 발생", stockCode, e);
                }
            }
        }
    }

    private boolean hasPendingOrderForStock(String stockCode) {
        return pendingOrderRepository.existsByStockDataShortCodeAndStatus(
                stockCode, PendingOrder.OrderStatus.PENDING);
    }

    // 거래 후 구독 해제 메서드
    private void unsubscribeStockAfterTrade(String stockCode) {
        try {
            // 먼저 구독 상태 확인
            if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
                log.debug("종목 {}은 이미 구독 해제되어 있음", stockCode);
                return;
            }

            // 해당 종목이 다른 곳에서 필요한지 확인
            boolean isNeededElsewhere = isStockNeededElsewhere(stockCode);

            if (!isNeededElsewhere) {
                log.info("거래 완료 후 불필요한 종목 구독 해제: {}", stockCode);
                kiwoomWebSocketClient.unsubscribeStock(stockCode);
            } else {
                log.info("종목 {}은 다른 곳에서 사용 중이므로 구독 유지", stockCode);
            }
        } catch (Exception e) {
            log.warn("종목 {} 구독 해제 중 오류 발생", stockCode, e);
            // 구독 해제 실패는 거래 자체에 영향을 주지 않도록 예외 처리
        }
    }

    private boolean isStockNeededElsewhere(String stockCode) {
        log.debug("종목 {} 필요 여부 확인 시작", stockCode);

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

        log.debug("종목 {} 필요 여부 확인: 지정가 주문={}, 포트폴리오={}, 웹소켓 구독={}, " +
                        "지정가 목적={}, 포트폴리오 목적={}, 결과={}",
                stockCode, hasPendingOrders, inPortfolios, hasWebSocketSubscribers,
                hasPendingOrderPurpose, hasPortfolioPurpose, result);

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
            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 1억으로 초기화 (100,000,000)
            member.updateMemberMoney(100_000_000L);
            memberRepository.save(member);

            log.info("회원 ID: {}의 자산을 3억으로 초기화했습니다.", memberId);

            // 투자 요약이 없으면 생성
            if (member.getInvestmentSummary() == null) {
                InvestmentSummary summary = InvestmentSummary.of(
                        member,
                        0L,  // 총 투자금
                        0L,  // 총 평가액
                        0L,  // 총 손익
                        0.0  // 수익률
                );
                // Member와 연관관계는 이미 InvestmentSummary.of() 메서드 내에서 설정됨
            }
            return new InitializeMoneyResponseDTO(
                    "success",
                    "회원 기본금이 1억원으로 초기화되었습니다.",
                    memberId
            );
        } finally {
            memberLock.unlock();
        }
    }

    @Transactional
    public TradeResponseDTO createPendingBuyOrder(Member member, StockData stock, TradeRequestDTO request) {
        // 회원 락 획득 - 자금 차감 전 락 확보
        ReentrantLock memberLock = getMemberLock(member.getMemberId());
        memberLock.lock();
        try {
            // 총 금액 계산 및 잔액 확보
            Long totalAmount = request.getPrice() * request.getQuantity();

            // 최신 회원 정보 조회
            member = memberRepository.findById(member.getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 잔액 확인
            if (member.getMemberMoney() < totalAmount) {
                failedTrades.incrementAndGet();
                throw new CustomException(StockErrorCode.INSUFFICIENT_BALANCE);
            }

            // 현재 가격 확인하여 매수 조건 즉시 충족 여부 체크
            boolean temporarySubscription = !kiwoomWebSocketClient.isSubscribed(request.getStockCode());
            Long currentPrice = getCurrentStockPriceWithRetry(request.getStockCode());

            // 현재가가 지정가보다 낮거나 같으면 즉시 체결 (즉, 지정가 >= 현재가)
            if (currentPrice != null && request.getPrice() >= currentPrice) {
                log.info("지정가({}원)가 현재가({}원)보다 높거나 같아 즉시 체결합니다.", request.getPrice(), currentPrice);

                // 시장가와 동일한 buyStock 메서드 사용
                TradeRequestDTO marketRequest = new TradeRequestDTO();
                marketRequest.setStockCode(request.getStockCode());
                marketRequest.setQuantity(request.getQuantity());
                marketRequest.setMarketOrder(true);

                // 임시 구독이었다면 구독 해제
                if (temporarySubscription) {
                    unsubscribeStockAfterTrade(request.getStockCode());
                }

                // buyStock 메서드 내부에서 트랜잭션 처리와 금액 차감이 이루어지므로 여기서는 별도 처리 없음
                return buyStock(marketRequest, member);
            }

            PendingOrder pendingOrder = null;
            try {
                // 지정가 매수를 위한 금액을 예약
                member.subtractMemberMoney(totalAmount);
                memberRepository.save(member);

                // 지정가 주문 생성
                pendingOrder = PendingOrder.of(
                        member,
                        stock,
                        TradeHistory.TradeType.BUY,
                        request.getQuantity(),
                        request.getPrice()
                );
                pendingOrderRepository.save(pendingOrder);

                // 해당 종목 실시간 가격 구독은 유지 (지정가 주문 모니터링 필요)
                subscribeStockIfNeeded(request.getStockCode());

                TradeResponseDTO response = new TradeResponseDTO();
                response.setOrderId(pendingOrder.getOrderId());
                response.setStockCode(stock.getShortCode());
                response.setStockName(stock.getShortName());
                response.setTradeType("BUY");
                response.setQuantity(request.getQuantity());
                response.setUnitPrice(request.getPrice());
                response.setTotalPrice(totalAmount);
                response.setTradedAt(LocalDateTime.now());
                response.setStatus("PENDING");
                response.setMessage("지정가 매수 주문이 접수되었습니다.");

                // 임시 구독이었던 경우 구독 유지 (지정가 주문 모니터링 필요)

                return response;
            } catch (Exception e) {
                log.error("지정가 매수 주문 처리 중 오류 발생", e);

                // 오류 발생 시 차감된 금액 환불 처리
                refundMoneyOnOrderFailure(member, totalAmount);

                // 주문이 생성되었다면 실패 상태로 변경
                if (pendingOrder != null) {
                    pendingOrder.fail();
                    pendingOrderRepository.save(pendingOrder);
                }

                // 임시 구독이었고 주문 실패 시 구독 해제
                if (temporarySubscription) {
                    unsubscribeStockAfterTrade(request.getStockCode());
                }

                failedTrades.incrementAndGet();
                // 예외를 던지지 않고 에러 응답 생성
                TradeResponseDTO errorResponse = new TradeResponseDTO();
                errorResponse.setStatus("ERROR");
                errorResponse.setMessage("지정가 매수 주문 처리 중 오류 발생: " + e.getMessage());
                return errorResponse;
            }
        } finally {
            memberLock.unlock();
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
            log.info("주문 실패로 인한 금액 환불: 회원ID={}, 금액={}원", member.getMemberId(), amount);
        } catch (Exception e) {
            log.error("주문 실패 후 금액 환불 처리 중 오류 발생", e);
            // 환불 실패 시 별도 처리 필요 (관리자 알림, 에러 로그 등)
        }
    }

    @Transactional
    public TradeResponseDTO createPendingSellOrder(Member member, StockData stock, TradeRequestDTO request) {
        // 종목 락 획득 - 보유 수량 확인 전 락 확보
        ReentrantLock stockLock = getStockLock(stock.getShortCode());
        stockLock.lock();
        try {
            // 최신 상태로 회원 및 포지션 정보 조회
            member = memberRepository.findById(member.getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 보유 수량 확인
            HoldingPosition position = holdingPositionRepository.findByMemberAndStockData(member, stock)
                    .orElseThrow(() -> new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "해당 종목을 보유하고 있지 않습니다"));

            if (position.getQuantity() < request.getQuantity()) {
                failedTrades.incrementAndGet();
                throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "보유 수량이 부족합니다");
            }

            // 현재 가격 확인하여 매도 조건 즉시 충족 여부 체크
            boolean temporarySubscription = !kiwoomWebSocketClient.isSubscribed(request.getStockCode());
            Long currentPrice = getCurrentStockPriceWithRetry(request.getStockCode());

            // 현재가가 지정가보다 높거나 같으면 즉시 체결
            if (currentPrice != null && currentPrice >= request.getPrice()) {
                // 시장가와 동일한 sellStock 메서드 사용
                TradeRequestDTO marketRequest = new TradeRequestDTO();
                marketRequest.setStockCode(request.getStockCode());
                marketRequest.setQuantity(request.getQuantity());
                marketRequest.setMarketOrder(true);

                // 임시 구독이었다면 구독 해제
                if (temporarySubscription) {
                    unsubscribeStockAfterTrade(request.getStockCode());
                }

                return sellStock(marketRequest, member);
            }

            PendingOrder pendingOrder = null;
            try {
                // 지정가 주문 생성
                pendingOrder = PendingOrder.of(
                        member,
                        stock,
                        TradeHistory.TradeType.SELL,
                        request.getQuantity(),
                        request.getPrice()
                );
                pendingOrderRepository.save(pendingOrder);

                // 해당 종목 실시간 가격 구독은 유지 (지정가 주문 모니터링 필요)
                subscribeStockIfNeeded(request.getStockCode());

                TradeResponseDTO response = new TradeResponseDTO();
                response.setOrderId(pendingOrder.getOrderId());
                response.setStockCode(stock.getShortCode());
                response.setStockName(stock.getShortName());
                response.setTradeType("SELL");
                response.setQuantity(request.getQuantity());
                response.setUnitPrice(request.getPrice());
                response.setTotalPrice(request.getPrice() * request.getQuantity());
                response.setTradedAt(LocalDateTime.now());
                response.setStatus("PENDING");
                response.setMessage("지정가 매도 주문이 접수되었습니다.");

                // 임시 구독이었던 경우 구독 유지 (지정가 주문 모니터링 필요)

                return response;
            } catch (Exception e) {
                log.error("지정가 매도 주문 처리 중 오류 발생", e);

                // 주문이 생성되었다면 실패 상태로 변경
                if (pendingOrder != null) {
                    pendingOrder.fail();
                    pendingOrderRepository.save(pendingOrder);
                }

                // 임시 구독이었고 주문 실패 시 구독 해제
                if (temporarySubscription) {
                    unsubscribeStockAfterTrade(request.getStockCode());
                }

                failedTrades.incrementAndGet();
                // 예외를 던지지 않고 에러 응답 생성
                TradeResponseDTO errorResponse = new TradeResponseDTO();
                errorResponse.setStatus("ERROR");
                errorResponse.setMessage("지정가 매도 주문 처리 중 오류 발생: " + e.getMessage());
                return errorResponse;
            }
        } finally {
            stockLock.unlock();
        }
    }


    // 종목 구독 메서드 (에러 핸들링 추가)
    private boolean subscribeStockIfNeeded(String stockCode) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
            log.error("유효하지 않은 종목 코드로 구독 시도: {}", stockCode);
            return false;
        }

        stockCode = stockCode.trim();

        if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
            try {
                log.info("종목 {} 실시간 데이터 구독 시작", stockCode);
                // 메서드가 성공하면 예외가 발생하지 않으므로 성공으로 간주
                stockSubscriptionService.registerStockForSubscription(stockCode);
                return true;
            } catch (CustomException e) {
                log.warn("종목 {} 구독 실패, 재시도 중", stockCode);
                try {
                    // 재시도
                    stockSubscriptionService.registerStockForSubscription(stockCode);
                    return true;
                } catch (CustomException e2) {
                    log.error("종목 {} 구독 두 번 실패, 관리자 확인 필요", stockCode);
                    return false;
                }
            } catch (Exception e) {
                log.error("종목 {} 구독 중 예외 발생", stockCode, e);
                return false;
            }
        }
        return true; // 이미 구독 중인 경우 true 반환
    }

    // 향상된 주식 가격 조회 메서드 (NPE 방지 및 캐시 사용)
    private Long getCurrentStockPrice(String stockCode) {
        // 스톡 코드 유효성 검사
        if (stockCode == null || stockCode.trim().isEmpty()) {
            log.error("유효하지 않은 종목 코드: {}", stockCode);
            return null;
        }

        stockCode = stockCode.trim();

        try {
            // 가격 정보 조회
            JsonNode priceData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);

            if (priceData != null && priceData.has("10")) {
                // 현재가 필드("10")에서 값을 추출하고 숫자로 변환
                String currentPriceStr = priceData.get("10").asText()
                        .replace(",", "")
                        .replace("+", "")
                        .replace("-", "")
                        .trim();
                try {
                    return Long.parseLong(currentPriceStr);
                } catch (NumberFormatException e) {
                    log.error("종목 {} 가격 변환 오류: {}", stockCode, currentPriceStr, e);
                    return null;
                }
            } else {
                log.warn("종목 {} 실시간 가격 데이터가 없음", stockCode);
                return null;
            }
        } catch (Exception e) {
            log.error("종목 {} 가격 조회 중 오류 발생", stockCode, e);
            return null;
        }
    }

    // 포지션 업데이트 메서드 (오류 핸들링 추가)
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

    // 성능 개선을 위한 캐싱 적용 (투자 요약 업데이트)
    @Transactional
    public void updateInvestmentSummary(Member member) {
        try {
            // 회원 정보 및 투자 요약 조회
            Member freshMember = memberRepository.findById(member.getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            InvestmentSummary summary = freshMember.getInvestmentSummary();

            // StockData를 즉시 로딩하여 조회
            List<HoldingPosition> positions = holdingPositionRepository.findByMemberWithStockData(freshMember);

            long totalInvestment = 0L;
            long totalValuation = 0L;

            for (HoldingPosition position : positions) {
                // StockData는 이미 로드되어 있으므로 안전하게 접근 가능
                String stockCode = position.getStockData().getShortCode();

                long investedAmount = position.getAveragePrice() * position.getQuantity();

                // 현재가 조회
                Long currentPrice = getCurrentStockPrice(stockCode);
                if (currentPrice == null) {
                    currentPrice = position.getAveragePrice(); // 가격 조회 실패 시 평균단가 사용
                }

                long currentValue = currentPrice * position.getQuantity();

                totalInvestment += investedAmount;
                totalValuation += currentValue;
            }

            long totalProfitLoss = totalValuation - totalInvestment;
            double returnRate = totalInvestment > 0 ? ((double) totalValuation / totalInvestment - 1.0) * 100.0 : 0.0;

            if (summary == null) {
                summary = InvestmentSummary.of(freshMember, totalInvestment, totalValuation, totalProfitLoss, returnRate);
            } else {
                summary.updateValues(totalInvestment, totalValuation, totalProfitLoss, returnRate);
            }
        } catch (Exception e) {
            log.error("투자 요약 업데이트 중 오류 발생", e);
            // 투자 요약 업데이트 실패는 거래 자체에 영향을 주지 않도록 예외 전파하지 않음
        }
    }

    @Override
    @Transactional
    public void onStockPriceUpdate(String stockCode, JsonNode data) {
        // 실시간 가격 업데이트 시 지정가 주문 체결 확인
        try {
            if (data != null && data.has("10")) {
                String priceStr = data.get("10").asText().replace(",", "").replace("+", "").replace("-", "").trim();
                try {
                    Long currentPrice = Long.parseLong(priceStr);

                    // 별도의 트랜잭션에서 처리
                    checkAndExecutePendingOrdersInNewTransaction(stockCode, currentPrice);
                } catch (NumberFormatException e) {
                    log.error("가격 변환 오류: {}", priceStr, e);
                }
            }
        } catch (Exception e) {
            log.error("지정가 주문 체결 확인 중 오류 발생", e);
        }
    }

    // 별도의 트랜잭션에서 실행하는 메서드 추가
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void checkAndExecutePendingOrdersInNewTransaction(String stockCode, Long currentPrice) {
        // 동시성 제어를 위한 락 획득
        ReentrantLock lock = getStockLock(stockCode);
        lock.lock();

        try {
            // 매수 주문 중 현재가가 지정가 이하인 주문 체결 (FETCH JOIN 사용)
            List<PendingOrder> buyOrders = pendingOrderRepository.findExecutableOrdersWithRelations(
                    stockCode, TradeHistory.TradeType.BUY, currentPrice);

            for (PendingOrder order : buyOrders) {
                executeBuyOrderWithFreshData(order, currentPrice);
            }

            // 매도 주문 중 현재가가 지정가 이상인 주문 체결
            List<PendingOrder> sellOrders = pendingOrderRepository.findExecutableOrdersWithRelations(
                    stockCode, TradeHistory.TradeType.SELL, currentPrice);

            for (PendingOrder order : sellOrders) {
                executeSellOrderWithFreshData(order, currentPrice);
            }
        } finally {
            lock.unlock();
        }
    }

    // 새로운 메서드 - executeBuyOrder 대체
    @Transactional
    private void executeBuyOrderWithFreshData(PendingOrder order, Long currentPrice) {
        // 이미 처리된 주문인지 확인 (동시 실행 방지)
        if (order.getStatus() != PendingOrder.OrderStatus.PENDING) {
            log.warn("이미 처리된 주문 건너뛰기: {}", order.getOrderId());
            return;
        }

        try {
            // 관련 엔티티는 이미 JOIN FETCH로 로드되었음
            Member member = order.getMember();
            StockData stock = order.getStockData();
            String stockCode = stock.getShortCode(); // 여기서 LazyInitializationException 발생 X

            // 주문 상태를 먼저 처리 중으로 변경
            order.processing();
            pendingOrderRepository.save(order);

            // 거래 내역 생성
            long totalAmount = currentPrice * order.getQuantity();

            // 지정가와 현재가 차이만큼 환불 (이미 예약된 금액을 사용)
            long refundAmount = (order.getTargetPrice() - currentPrice) * order.getQuantity();
            if (refundAmount > 0) {
                member.addMemberMoney(refundAmount);
                memberRepository.save(member);
            }

            TradeHistory tradeHistory = TradeHistory.of(
                    member,
                    stock,
                    TradeHistory.TradeType.BUY,
                    order.getQuantity(),
                    currentPrice,
                    totalAmount,
                    LocalDateTime.now()
            );
            tradeHistoryRepository.save(tradeHistory);

            // 포지션 업데이트 - 안전한 버전 사용
            updateHoldingPositionSafely(member, stock, order.getQuantity(), currentPrice, TradeHistory.TradeType.BUY);

            // 투자 요약 업데이트
            updateInvestmentSummary(member);

            // 주문 상태 업데이트
            order.complete();
            pendingOrderRepository.save(order);

            log.info("지정가 매수 주문 체결: 주문ID={}, 종목={}, 수량={}, 가격={}",
                    order.getOrderId(), stockCode, order.getQuantity(), currentPrice);

            // 해당 종목의 다른 지정가 주문이 없고, 포트폴리오에도 없으면 구독 해제
            if (!isStockNeededElsewhere(stockCode)) {
                unsubscribeStockAfterTrade(stockCode);
            }

            try {
                // 포트폴리오 웹소켓을 통해 업데이트 알림
                portfolioWebSocketHandler.sendFullPortfolioUpdate(member.getMemberId());
            } catch (Exception e) {
                log.warn("주문 체결 후 포트폴리오 업데이트 알림 실패: {}", e.getMessage());
            }

            successfulTrades.incrementAndGet();
        } catch (Exception e) {
            log.error("지정가 매수 주문 체결 중 오류 발생: {}", e.getMessage(), e);

            try {
                // 오류 발생 시 주문 상태 실패로 변경 및 환불
                order.fail();
                pendingOrderRepository.save(order);

                // 매수 주문이 실패하면 전체 금액 환불 처리
                if (order.getOrderType() == TradeHistory.TradeType.BUY) {
                    Member member = order.getMember();
                    StockData stock = order.getStockData();
                    String stockCode = stock.getShortCode();

                    refundMoneyOnOrderFailure(member, order.getTargetPrice() * order.getQuantity());
                    log.info("매수 주문 실패로 전액 환불: 주문ID={}, 환불금액={}원",
                            order.getOrderId(), order.getTargetPrice() * order.getQuantity());

                    // 다른 지정가 주문이 없고, 포트폴리오에도 없으면 구독 해제
                    if (!isStockNeededElsewhere(stockCode)) {
                        unsubscribeStockAfterTrade(stockCode);
                    }
                }
            } catch (Exception ex) {
                log.error("주문 실패 처리 중 추가 오류 발생", ex);
            }

            failedTrades.incrementAndGet();
        }
    }


    @Override
    public void onStockBidAskUpdate(String stockCode, JsonNode data) {
        // 호가 업데이트는 별도 처리 필요 없음
    }

    @Transactional
    public void checkAndExecutePendingOrders(String stockCode, Long currentPrice) {
        // 동시성 제어를 위한 락 획득
        ReentrantLock lock = getStockLock(stockCode);
        lock.lock();

        try {
            // 매수 주문 중 현재가가 지정가 이하인 주문 체결 (FETCH JOIN 사용)
            List<PendingOrder> buyOrders = pendingOrderRepository.findExecutableOrdersWithRelations(
                    stockCode, TradeHistory.TradeType.BUY, currentPrice);

            for (PendingOrder order : buyOrders) {
                // 각 주문은 별도의 트랜잭션으로 처리
                executeOrderInNewTransaction(order, currentPrice);
            }

            // 매도 주문 중 현재가가 지정가 이상인 주문 체결
            List<PendingOrder> sellOrders = pendingOrderRepository.findExecutableOrdersWithRelations(
                    stockCode, TradeHistory.TradeType.SELL, currentPrice);

            for (PendingOrder order : sellOrders) {
                // 각 주문은 별도의 트랜잭션으로 처리
                executeOrderInNewTransaction(order, currentPrice);
            }
        } finally {
            lock.unlock();
        }
    }

    // 별도의 트랜잭션으로 주문을 처리하는 메서드
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void executeOrderInNewTransaction(PendingOrder order, Long currentPrice) {
        try {
            // FETCH JOIN을 사용하여 관련 엔티티를 함께 로드
            PendingOrder freshOrder = pendingOrderRepository.findByIdWithRelations(order.getOrderId())
                    .orElse(null);

            if (freshOrder == null || freshOrder.getStatus() != PendingOrder.OrderStatus.PENDING) {
                return;
            }

            if (freshOrder.getOrderType() == TradeHistory.TradeType.BUY) {
                executeBuyOrderWithFreshData(freshOrder, currentPrice);
            } else {
                executeSellOrderWithFreshData(freshOrder, currentPrice);
            }
        } catch (Exception e) {
            log.error("주문 처리 중 오류: orderId={}, error={}", order.getOrderId(), e.getMessage());
        }
    }

    @Transactional
    private void executeBuyOrder(PendingOrder order, Long currentPrice) {
        // 이미 처리된 주문인지 확인 (동시 실행 방지)
        if (order.getStatus() != PendingOrder.OrderStatus.PENDING) {
            log.warn("이미 처리된 주문 건너뛰기: {}", order.getOrderId());
            return;
        }

        try {
            // 트랜잭션 내에서 모든 엔티티 다시 조회 (세션 유지)
            PendingOrder freshOrder = pendingOrderRepository.findById(order.getOrderId())
                    .orElseThrow(() -> new RuntimeException("주문을 찾을 수 없음"));

            // 상태 재확인
            if (freshOrder.getStatus() != PendingOrder.OrderStatus.PENDING) {
                return;
            }

            // 관련 엔티티 명시적으로 다시 조회
            Member member = memberRepository.findById(freshOrder.getMember().getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // shortCode로 StockData 조회 (id가 아님)
            String stockCode = freshOrder.getStockData().getShortCode();
            StockData stock = stockDataRepository.findByShortCode(stockCode)
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 주문 상태를 먼저 처리 중으로 변경
            freshOrder.processing();
            pendingOrderRepository.save(freshOrder);

            // 거래 내역 생성
            long totalAmount = currentPrice * freshOrder.getQuantity();

            // 지정가와 현재가 차이만큼 환불 (이미 예약된 금액을 사용)
            long refundAmount = (freshOrder.getTargetPrice() - currentPrice) * freshOrder.getQuantity();
            if (refundAmount > 0) {
                member.addMemberMoney(refundAmount);
                memberRepository.save(member);
            }

            TradeHistory tradeHistory = TradeHistory.of(
                    member,
                    stock,
                    TradeHistory.TradeType.BUY,
                    freshOrder.getQuantity(),
                    currentPrice,
                    totalAmount,
                    LocalDateTime.now()
            );
            tradeHistoryRepository.save(tradeHistory);

            // 포지션 업데이트 - 안전한 버전 사용
            updateHoldingPositionSafely(member, stock, freshOrder.getQuantity(), currentPrice, TradeHistory.TradeType.BUY);

            // 투자 요약 업데이트
            updateInvestmentSummary(member);

            // 주문 상태 업데이트
            freshOrder.complete();
            pendingOrderRepository.save(freshOrder);

            log.info("지정가 매수 주문 체결: 주문ID={}, 종목={}, 수량={}, 가격={}",
                    freshOrder.getOrderId(), stock.getShortCode(), freshOrder.getQuantity(), currentPrice);

            // 해당 종목의 다른 지정가 주문이 없고, 포트폴리오에도 없으면 구독 해제
            if (!isStockNeededElsewhere(stock.getShortCode())) {
                unsubscribeStockAfterTrade(stock.getShortCode());
            }

            try {
                // 포트폴리오 웹소켓을 통해 업데이트 알림
                portfolioWebSocketHandler.sendFullPortfolioUpdate(member.getMemberId());
            } catch (Exception e) {
                log.warn("주문 체결 후 포트폴리오 업데이트 알림 실패: {}", e.getMessage());
            }

            successfulTrades.incrementAndGet();
        } catch (Exception e) {
            log.error("지정가 매수 주문 체결 중 오류 발생: {}", e.getMessage(), e);

            try {
                // 오류 발생 시 주문 상태 실패로 변경 및 환불
                PendingOrder freshOrder = pendingOrderRepository.findById(order.getOrderId()).orElse(null);
                if (freshOrder != null &&
                        (freshOrder.getStatus() == PendingOrder.OrderStatus.PENDING ||
                                freshOrder.getStatus() == PendingOrder.OrderStatus.PROCESSING)) {

                    freshOrder.fail();
                    pendingOrderRepository.save(freshOrder);

                    // 매수 주문이 실패하면 전체 금액 환불 처리
                    if (freshOrder.getOrderType() == TradeHistory.TradeType.BUY) {
                        Member member = memberRepository.findById(freshOrder.getMember().getMemberId()).orElse(null);
                        if (member != null) {
                            // 안전하게 stockCode 가져오기
                            String stockCode = "unknown";
                            try {
                                // 세션이 열려있는 상태에서 stockCode 가져오려고 시도
                                stockCode = stockDataRepository.findByShortCode(freshOrder.getStockData().getShortCode())
                                        .map(StockData::getShortCode)
                                        .orElse("unknown");
                            } catch (Exception ex) {
                                log.warn("실패 처리 중 종목 코드 조회 실패");
                            }

                            refundMoneyOnOrderFailure(member, freshOrder.getTargetPrice() * freshOrder.getQuantity());
                            log.info("매수 주문 실패로 전액 환불: 주문ID={}, 환불금액={}원",
                                    freshOrder.getOrderId(), freshOrder.getTargetPrice() * freshOrder.getQuantity());

                            // 다른 지정가 주문이 없고, 포트폴리오에도 없으면 구독 해제
                            if (!isStockNeededElsewhere(stockCode)) {
                                unsubscribeStockAfterTrade(stockCode);
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                log.error("주문 실패 처리 중 추가 오류 발생", ex);
            }

            failedTrades.incrementAndGet();
        }
    }

    @Transactional
    public void updateHoldingPositionSafely(Member member, StockData stock, Integer quantity, Long price, TradeHistory.TradeType type) {
        try {
            // 명시적으로 트랜잭션 내에서 재조회
            Member freshMember = memberRepository.findById(member.getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            StockData freshStock = stockDataRepository.findByShortCode(stock.getShortCode())
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            Optional<HoldingPosition> optionalPosition = holdingPositionRepository.findByMemberAndStockData(freshMember, freshStock);

            if (type == TradeHistory.TradeType.BUY) {
                if (optionalPosition.isPresent()) {
                    // 기존 포지션 업데이트
                    HoldingPosition position = optionalPosition.get();
                    int newQuantity = position.getQuantity() + quantity;
                    long newAvgPrice = ((position.getAveragePrice() * position.getQuantity()) + (price * quantity)) / newQuantity;

                    // 현재가는 파라미터로 전달된 가격 사용 (실시간 조회 대신)
                    long currentProfit = (price - newAvgPrice) * newQuantity;
                    double returnRate = ((double) price / newAvgPrice - 1.0) * 100.0;

                    position.updatePosition(newQuantity, newAvgPrice, currentProfit, returnRate);
                    holdingPositionRepository.save(position);
                } else {
                    // 새 포지션 생성
                    HoldingPosition newPosition = HoldingPosition.of(
                            freshMember,
                            freshStock,
                            quantity,
                            price,
                            0L, // 초기 수익
                            0.0 // 초기 수익률
                    );
                    holdingPositionRepository.save(newPosition);
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
                    long currentProfit = (price - position.getAveragePrice()) * newQuantity;
                    double returnRate = ((double) price / position.getAveragePrice() - 1.0) * 100.0;

                    position.updatePosition(newQuantity, position.getAveragePrice(), currentProfit, returnRate);
                    holdingPositionRepository.save(position);
                }
            }
        } catch (Exception e) {
            log.error("포지션 업데이트 중 오류 발생", e);
            throw new CustomException(StockErrorCode.POSITION_UPDATE_FAILED, "포지션 업데이트 실패: " + e.getMessage());
        }
    }

    // 매도 주문 처리 메서드 수정 - 구독 해제 추가
    @Transactional
    private void executeSellOrder(PendingOrder order, Long currentPrice) {
        // 이미 처리된 주문인지 확인 (동시 실행 방지)
        if (order.getStatus() != PendingOrder.OrderStatus.PENDING) {
            log.warn("이미 처리된 주문 건너뛰기: {}", order.getOrderId());
            return;
        }

        try {
            // 트랜잭션 내에서 모든 엔티티 다시 조회 (세션 유지)
            PendingOrder freshOrder = pendingOrderRepository.findById(order.getOrderId())
                    .orElseThrow(() -> new RuntimeException("주문을 찾을 수 없음"));

            // 상태 재확인
            if (freshOrder.getStatus() != PendingOrder.OrderStatus.PENDING) {
                return;
            }

            // 관련 엔티티 명시적으로 다시 조회
            Member member = memberRepository.findById(freshOrder.getMember().getMemberId())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // shortCode로 StockData 조회 (id가 아님)
            String stockCode = freshOrder.getStockData().getShortCode();
            StockData stock = stockDataRepository.findByShortCode(stockCode)
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 주문 상태를 먼저 처리 중으로 변경
            freshOrder.processing();
            pendingOrderRepository.save(freshOrder);

            // 실제 보유 수량 확인 (동시 처리 방지)
            HoldingPosition position = holdingPositionRepository.findByMemberAndStockData(member, stock)
                    .orElseThrow(() -> new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "해당 종목을 보유하고 있지 않습니다"));

            if (position.getQuantity() < freshOrder.getQuantity()) {
                // 보유 수량 부족 시 주문 실패 처리
                freshOrder.fail();
                pendingOrderRepository.save(freshOrder);
                failedTrades.incrementAndGet();
                throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "보유 수량이 부족합니다");
            }

            // 거래 내역 생성
            long totalAmount = currentPrice * freshOrder.getQuantity();

            // 매도 금액 입금
            member.addMemberMoney(totalAmount);
            memberRepository.save(member);

            TradeHistory tradeHistory = TradeHistory.of(
                    member,
                    stock,
                    TradeHistory.TradeType.SELL,
                    freshOrder.getQuantity(),
                    currentPrice,
                    totalAmount,
                    LocalDateTime.now()
            );
            tradeHistoryRepository.save(tradeHistory);

            // 포지션 업데이트
            updateHoldingPositionSafely(member, stock, freshOrder.getQuantity(), currentPrice, TradeHistory.TradeType.SELL);

            // 투자 요약 업데이트
            updateInvestmentSummary(member);

            // 주문 상태 업데이트
            freshOrder.complete();
            pendingOrderRepository.save(freshOrder);

            log.info("지정가 매도 주문 체결: 주문ID={}, 종목={}, 수량={}, 가격={}",
                    freshOrder.getOrderId(), stock.getShortCode(), freshOrder.getQuantity(), currentPrice);

            // 해당 종목의 다른 지정가 주문이 없고, 포트폴리오에도 없으면 구독 해제
            if (!isStockNeededElsewhere(stock.getShortCode())) {
                unsubscribeStockAfterTrade(stock.getShortCode());
            }

            try {
                // 포트폴리오 웹소켓을 통해 업데이트 알림
                portfolioWebSocketHandler.sendFullPortfolioUpdate(member.getMemberId());
            } catch (Exception e) {
                // 포트폴리오 업데이트 실패는 거래 처리에 영향을 주지 않도록 예외 처리
                log.warn("주문 체결 후 포트폴리오 업데이트 알림 실패: {}", e.getMessage());
            }

            successfulTrades.incrementAndGet();
        } catch (Exception e) {
            log.error("지정가 매도 주문 체결 중 오류 발생: {}", e.getMessage(), e);

            try {
                // 오류 발생 시 주문 상태 실패로 변경
                PendingOrder freshOrder = pendingOrderRepository.findById(order.getOrderId()).orElse(null);
                if (freshOrder != null &&
                        (freshOrder.getStatus() == PendingOrder.OrderStatus.PENDING ||
                                freshOrder.getStatus() == PendingOrder.OrderStatus.PROCESSING)) {

                    freshOrder.fail();
                    pendingOrderRepository.save(freshOrder);

                    // 안전하게 stockCode 가져오기
                    String stockCode = "unknown";
                    try {
                        // 세션이 열려있는 상태에서 stockCode 가져오려고 시도
                        stockCode = stockDataRepository.findByShortCode(freshOrder.getStockData().getShortCode())
                                .map(StockData::getShortCode)
                                .orElse("unknown");
                    } catch (Exception ex) {
                        log.warn("실패 처리 중 종목 코드 조회 실패");
                    }

                    // 다른 지정가 주문이 없고, 포트폴리오에도 없으면 구독 해제
                    if (!isStockNeededElsewhere(stockCode)) {
                        unsubscribeStockAfterTrade(stockCode);
                    }
                }
            } catch (Exception ex) {
                log.error("주문 실패 처리 중 추가 오류 발생", ex);
            }

            failedTrades.incrementAndGet();
            throw e;
        }
    }

    private TradeResponseDTO createErrorResponse(String message) {
        TradeResponseDTO response = new TradeResponseDTO();
        response.setStatus("ERROR");
        response.setMessage(message);
        return response;
    }

    @Transactional(readOnly = true)
    public List<PendingOrderDTO> getPendingOrdersByMember(String authorizationHeader) {

        String token = jwtTokenProvider.resolveToken(authorizationHeader);

        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        List<PendingOrder> pendingOrders = pendingOrderRepository.findByMemberAndStatus(
                member, PendingOrder.OrderStatus.PENDING);

        return pendingOrders.stream()
                .map(PendingOrderDTO::fromPendingOrder)
                .collect(Collectors.toList());
    }

    // 주문 취소 메서드 수정 - 구독 해제 추가
    @Transactional
    public boolean cancelPendingOrder(String authorizationHeader, Long orderId) {
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // 동시성 문제 방지를 위해 쓰기 잠금으로 회원 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        Optional<PendingOrder> optionalOrder = pendingOrderRepository.findById(orderId);

        if (optionalOrder.isEmpty()) {
            log.warn("취소할 주문을 찾을 수 없음: {}", orderId);
            return false;
        }

        PendingOrder order = optionalOrder.get();

        // 주문이 현재 회원의 것인지 확인
        if (!order.getMember().getMemberId().equals(memberId)) {
            log.warn("다른 회원의 주문을 취소할 수 없음: 주문ID={}, 요청회원={}, 주문회원={}",
                    orderId, memberId, order.getMember().getMemberId());
            return false;
        }

        // 취소 가능한 상태인지 확인
        if (order.getStatus() != PendingOrder.OrderStatus.PENDING) {
            log.warn("이미 처리된 주문은 취소할 수 없음: 주문ID={}, 상태={}", orderId, order.getStatus());
            return false;
        }

        // 동시성 제어를 위한 락 획득
        ReentrantLock lock = getStockLock(order.getStockData().getShortCode());
        lock.lock();

        try {
            // 상태 재확인 (락 획득 후)
            if (order.getStatus() != PendingOrder.OrderStatus.PENDING) {
                log.warn("락 획득 후 상태 변경됨: 주문ID={}, 상태={}", orderId, order.getStatus());
                return false;
            }

            // 매수 주문인 경우 예약된 금액 환불
            if (order.getOrderType() == TradeHistory.TradeType.BUY) {
                Long refundAmount = order.getTargetPrice() * order.getQuantity();
                member.addMemberMoney(refundAmount);
                memberRepository.save(member);
                log.info("취소된 매수 주문에 대한 금액 환불: 주문ID={}, 금액={}원", orderId, refundAmount);
            }

            // 주문 취소 처리
            order.cancel();
            pendingOrderRepository.save(order);
            log.info("주문 취소 완료: 주문ID={}, 종목={}, 타입={}",
                    order.getOrderId(), order.getStockData().getShortCode(), order.getOrderType());

            // 해당 종목의 다른 지정가 주문이 없고, 포트폴리오에도 없으면 구독 해제
            String stockCode = order.getStockData().getShortCode();
            if (!isStockNeededElsewhere(stockCode)) {
                unsubscribeStockAfterTrade(stockCode);
            }

            return true;
        } catch (Exception e) {
            log.error("주문 취소 중 오류 발생: {}", e.getMessage(), e);
            return false;
        } finally {
            lock.unlock();
        }
    }

    // 지정가 주문 처리 스케줄러 메서드 수정 - 구독 적절한 정리 추가
    @Transactional(readOnly = true)
    public void processPendingOrders() {
        log.debug("지정가 주문 처리 스케줄러 실행");

        try {
            // JOIN FETCH를 사용하여 모든 관련 데이터를 즉시 로딩
            List<PendingOrder> pendingOrders = pendingOrderRepository.findPendingOrdersWithStockData();

            if (pendingOrders.isEmpty()) {
                return;
            }

            log.info("처리할 지정가 주문 수: {}", pendingOrders.size());

            // 종목별로 그룹화하여 효율적으로 처리
            Map<String, List<PendingOrder>> ordersByStockCode = pendingOrders.stream()
                    .collect(Collectors.groupingBy(order -> order.getStockData().getShortCode()));

            // 각 종목별로 현재가 조회 및 주문 처리
            for (Map.Entry<String, List<PendingOrder>> entry : ordersByStockCode.entrySet()) {
                String stockCode = entry.getKey();
                List<PendingOrder> orders = entry.getValue();

                // 종목 구독 확인 및 필요시 구독 추가
                boolean temporarySubscription = false;
                if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
                    boolean success = subscribeStockIfNeeded(stockCode);
                    if (!success) {
                        log.warn("종목 {} 구독 실패, 주문 처리 건너뜀", stockCode);
                        continue;
                    }
                    temporarySubscription = true;

                    // 구독 후 잠시 대기하여 데이터 수신을 기다림
                    try {
                        Thread.sleep(300);
                    } catch (InterruptedException ie) {
                        Thread.currentThread().interrupt();
                    }
                }

                try {
                    // 현재가 조회
                    Long currentPrice = getCurrentStockPriceWithRetry(stockCode);
                    if (currentPrice == null) {
                        log.warn("종목 {} 현재가 조회 실패, 주문 처리 건너뜀", stockCode);
                        continue;
                    }

                    log.debug("종목 {} 현재가 조회 성공: {}원, 처리할 주문 수: {}",
                            stockCode, currentPrice, orders.size());

                    // 각 주문 처리 (개별 트랜잭션으로 분리)
                    for (PendingOrder order : orders) {
                        processOrderWithTransaction(order, currentPrice);
                    }
                } finally {
                    // 임시 구독이었고, 모든 주문 처리 후 필요 없으면 구독 해제
                    if (temporarySubscription && !isStockNeededElsewhere(stockCode)) {
                        log.info("임시 구독한 종목 {} 주문 처리 완료 후 구독 해제", stockCode);
                        kiwoomWebSocketClient.unsubscribeStock(stockCode);
                    }
                }
            }
        } catch (Exception e) {
            log.error("지정가 주문 처리 스케줄러 실행 중 오류 발생", e);
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
                log.debug("주문 {} 처리 건너뜀: 이미 처리되었거나 존재하지 않음", order.getOrderId());
                return;
            }

            if (freshOrder.getOrderType() == TradeHistory.TradeType.BUY) {
                // 매수 주문: 지정가가 현재가보다 크거나 같으면 체결
                if (freshOrder.getTargetPrice() >= currentPrice) {
                    log.info("매수 주문 체결 조건 충족: 주문ID={}, 지정가={}, 현재가={}",
                            freshOrder.getOrderId(), freshOrder.getTargetPrice(), currentPrice);
                    executeBuyOrder(freshOrder, currentPrice);
                }
            } else {
                // 매도 주문: 지정가가 현재가보다 작거나 같으면 체결
                if (freshOrder.getTargetPrice() <= currentPrice) {
                    log.info("매도 주문 체결 조건 충족: 주문ID={}, 지정가={}, 현재가={}",
                            freshOrder.getOrderId(), freshOrder.getTargetPrice(), currentPrice);
                    executeSellOrder(freshOrder, currentPrice);
                }
            }
        } catch (Exception e) {
            log.error("주문 처리 중 오류 발생: orderId={}, error={}",
                    order.getOrderId(), e.getMessage(), e);
            // 예외를 전파하지 않고 로그만 남김
        }
    }

    // 각 주문을 트랜잭션으로 처리하는 별도의 메서드
    @Transactional
    public void processOrder(PendingOrder order, Long currentPrice) {
        try {
            if (order.getOrderType() == TradeHistory.TradeType.BUY) {
                // 매수 주문: 지정가가 현재가보다 크거나 같으면 체결
                if (order.getTargetPrice() >= currentPrice) {
                    executeBuyOrder(order, currentPrice);
                }
            } else {
                // 매도 주문: 지정가가 현재가보다 작거나 같으면 체결
                if (order.getTargetPrice() <= currentPrice) {
                    executeSellOrder(order, currentPrice);
                }
            }
        } catch (Exception e) {
            log.error("주문 처리 중 오류: orderId={}, error={}", order.getOrderId(), e.getMessage());
        }
    }

    // 매핑된 현재가 정보 반환 (메트릭 및 모니터링용)
    public Map<String, Long> getAllCurrentPrices() {
        Map<String, Long> prices = new HashMap<>();

        List<StockData> stocks = stockDataRepository.findAll();
        for (StockData stock : stocks) {
            Long price = getCurrentStockPrice(stock.getShortCode());
            if (price != null) {
                prices.put(stock.getShortCode(), price);
            }
        }

        return prices;
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

    // 새로운 메서드 - executeSellOrder 대체
    @Transactional
    private void executeSellOrderWithFreshData(PendingOrder order, Long currentPrice) {
        // 이미 처리된 주문인지 확인 (동시 실행 방지)
        if (order.getStatus() != PendingOrder.OrderStatus.PENDING) {
            log.warn("이미 처리된 주문 건너뛰기: {}", order.getOrderId());
            return;
        }

        try {
            // 관련 엔티티는 이미 JOIN FETCH로 로드되었음
            Member member = order.getMember();
            StockData stock = order.getStockData();
            String stockCode = stock.getShortCode(); // 여기서 LazyInitializationException 발생 X

            // 주문 상태를 먼저 처리 중으로 변경
            order.processing();
            pendingOrderRepository.save(order);

            // 실제 보유 수량 확인 (동시 처리 방지)
            HoldingPosition position = holdingPositionRepository.findByMemberAndStockData(member, stock)
                    .orElseThrow(() -> new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "해당 종목을 보유하고 있지 않습니다"));

            if (position.getQuantity() < order.getQuantity()) {
                // 보유 수량 부족 시 주문 실패 처리
                order.fail();
                pendingOrderRepository.save(order);
                failedTrades.incrementAndGet();
                throw new CustomException(StockErrorCode.INSUFFICIENT_STOCK, "보유 수량이 부족합니다");
            }

            // 거래 내역 생성
            long totalAmount = currentPrice * order.getQuantity();

            // 매도 금액 입금
            member.addMemberMoney(totalAmount);
            memberRepository.save(member);

            TradeHistory tradeHistory = TradeHistory.of(
                    member,
                    stock,
                    TradeHistory.TradeType.SELL,
                    order.getQuantity(),
                    currentPrice,
                    totalAmount,
                    LocalDateTime.now()
            );
            tradeHistoryRepository.save(tradeHistory);

            // 포지션 업데이트
            updateHoldingPositionSafely(member, stock, order.getQuantity(), currentPrice, TradeHistory.TradeType.SELL);

            // 투자 요약 업데이트
            updateInvestmentSummary(member);

            // 주문 상태 업데이트
            order.complete();
            pendingOrderRepository.save(order);

            log.info("지정가 매도 주문 체결: 주문ID={}, 종목={}, 수량={}, 가격={}",
                    order.getOrderId(), stockCode, order.getQuantity(), currentPrice);

            // 해당 종목의 다른 지정가 주문이 없고, 포트폴리오에도 없으면 구독 해제
            if (!isStockNeededElsewhere(stockCode)) {
                unsubscribeStockAfterTrade(stockCode);
            }

            try {
                // 포트폴리오 웹소켓을 통해 업데이트 알림
                portfolioWebSocketHandler.sendFullPortfolioUpdate(member.getMemberId());
            } catch (Exception e) {
                // 포트폴리오 업데이트 실패는 거래 처리에 영향을 주지 않도록 예외 처리
                log.warn("주문 체결 후 포트폴리오 업데이트 알림 실패: {}", e.getMessage());
            }

            successfulTrades.incrementAndGet();
        } catch (Exception e) {
            log.error("지정가 매도 주문 체결 중 오류 발생: {}", e.getMessage(), e);

            try {
                // 오류 발생 시 주문 상태 실패로 변경
                order.fail();
                pendingOrderRepository.save(order);

                // 안전하게 stockCode 가져오기
                String stockCode = "";
                try {
                    stockCode = order.getStockData().getShortCode();
                } catch (Exception ex) {
                    log.warn("실패 처리 중 종목 코드 조회 실패");
                    stockCode = "unknown";
                }

                // 다른 지정가 주문이 없고, 포트폴리오에도 없으면 구독 해제
                if (!isStockNeededElsewhere(stockCode)) {
                    unsubscribeStockAfterTrade(stockCode);
                }
            } catch (Exception ex) {
                log.error("주문 실패 처리 중 추가 오류 발생", ex);
            }

            failedTrades.incrementAndGet();
        }
    }
}