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
import realClassOne.chickenStock.stock.repository.StockMasterDataRepository;
import realClassOne.chickenStock.stock.repository.TradeHistoryRepository;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;
@Service
@RequiredArgsConstructor
@Slf4j
public class StockTradeService implements KiwoomWebSocketClient.StockDataListener {

    private final MemberRepository memberRepository;
    private final StockMasterDataRepository stockMasterDataRepository;
    private final TradeHistoryRepository tradeHistoryRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final PendingOrderRepository pendingOrderRepository;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final StockSubscriptionService stockSubscriptionService;
    private final JwtTokenProvider jwtTokenProvider;

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

    @Transactional
    public TradeResponseDTO buyStock(TradeRequestDTO request) {
        // 입력값 검증
        validateTradeRequest(request);

        Long memberId = request.getMemberId();

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        StockData stock = stockMasterDataRepository.findByShortCode(request.getStockCode())
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
                if (Boolean.TRUE.equals(request.getMarketOrder())) {
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
    public TradeResponseDTO sellStock(TradeRequestDTO request) {
        // 입력값 검증
        validateTradeRequest(request);

        Long memberId = request.getMemberId();

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        StockData stock = stockMasterDataRepository.findByShortCode(request.getStockCode())
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
                if (Boolean.TRUE.equals(request.getMarketOrder())) {
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

    // 실시간 가격 조회 메서드 (재시도 로직 추가)
    private Long getCurrentStockPriceWithRetry(String stockCode) {
        int maxRetries = 3;
        int retryCount = 0;
        long retryDelayMs = 300;

        while (retryCount < maxRetries) {
            Long price = getCurrentStockPrice(stockCode);
            if (price != null) {
                return price;
            }

            retryCount++;
            if (retryCount < maxRetries) {
                try {
                    log.warn("{}번째 가격 조회 실패, {}ms 후 재시도", retryCount, retryDelayMs);
                    Thread.sleep(retryDelayMs);
                    retryDelayMs *= 2; // 지수 백오프
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }

        log.error("종목 {} 가격 조회 최대 재시도 횟수 초과", stockCode);
        return null;
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

            // 3억으로 초기화 (300,000,000)
            member.updateMemberMoney(300_000_000L);
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
                    "회원 기본금이 3억원으로 초기화되었습니다.",
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
            Long currentPrice = getCurrentStockPriceWithRetry(request.getStockCode());

            // 현재가가 지정가보다 낮거나 같으면 즉시 체결 (즉, 지정가 >= 현재가)
            if (currentPrice != null && request.getPrice() >= currentPrice) {
                log.info("지정가({}원)가 현재가({}원)보다 높거나 같아 즉시 체결합니다.", request.getPrice(), currentPrice);

                // 시장가와 동일한 buyStock 메서드 사용
                TradeRequestDTO marketRequest = new TradeRequestDTO();
                marketRequest.setMemberId(request.getMemberId());
                marketRequest.setStockCode(request.getStockCode());
                marketRequest.setQuantity(request.getQuantity());
                marketRequest.setMarketOrder(true);

                // buyStock 메서드 내부에서 트랜잭션 처리와 금액 차감이 이루어지므로 여기서는 별도 처리 없음
                return buyStock(marketRequest);
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

                // 해당 종목 실시간 가격 구독
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
            Long currentPrice = getCurrentStockPriceWithRetry(request.getStockCode());

            // 현재가가 지정가보다 높거나 같으면 즉시 체결
            if (currentPrice != null && currentPrice >= request.getPrice()) {
                // 시장가와 동일한 sellStock 메서드 사용 (수정 필요)
                TradeRequestDTO marketRequest = new TradeRequestDTO();
                marketRequest.setMemberId(request.getMemberId());
                marketRequest.setStockCode(request.getStockCode());
                marketRequest.setQuantity(request.getQuantity());
                marketRequest.setMarketOrder(true);
                return sellStock(marketRequest);
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

                // 해당 종목 실시간 가격 구독
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

                return response;
            } catch (Exception e) {
                log.error("지정가 매도 주문 처리 중 오류 발생", e);

                // 주문이 생성되었다면 실패 상태로 변경
                if (pendingOrder != null) {
                    pendingOrder.fail();
                    pendingOrderRepository.save(pendingOrder);
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

        // 이미 구독 중인지 확인하고, 아니면 구독
        if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
            log.info("종목 {} 실시간 데이터 구독 시작", stockCode);
            stockSubscriptionService.registerStockForSubscription(stockCode);

            // 구독 후 잠시 대기하여 데이터 수신
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.warn("데이터 수신 대기 중 인터럽트 발생", e);
            }
        }

        // KiwoomWebSocketClient에서 캐시된 최신 가격 정보 조회
        try {
            // 가격 정보 조회
            JsonNode priceData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);

            if (priceData != null && priceData.has("10")) {
                // 현재가 필드("10")에서 값을 추출하고 숫자로 변환
//                String currentPriceStr = priceData.get("10").asText().replace(",", "");
                String currentPriceStr = priceData.get("10").asText().replace(",", "").replace("+", "").replace("-", "");

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
            InvestmentSummary summary = member.getInvestmentSummary();
            List<HoldingPosition> positions = holdingPositionRepository.findByMember(member);

            long totalInvestment = 0L;
            long totalValuation = 0L;

            for (HoldingPosition position : positions) {
                long investedAmount = position.getAveragePrice() * position.getQuantity();

                // 현재가 조회 전략 변경 - 캐싱된 값 사용, 없으면 평균가 사용
                Long currentPrice = getCurrentStockPrice(position.getStockData().getShortCode());
                if (currentPrice == null) {
                    currentPrice = position.getAveragePrice(); // 가격 조회 실패 시 평균단가 사용
                    log.warn("종목 {} 현재가 조회 실패, 평균단가 사용", position.getStockData().getShortCode());
                }

                long currentValue = currentPrice * position.getQuantity();

                totalInvestment += investedAmount;
                totalValuation += currentValue;
            }

            long totalProfitLoss = totalValuation - totalInvestment;
            double returnRate = totalInvestment > 0 ? ((double) totalValuation / totalInvestment - 1.0) * 100.0 : 0.0;

            if (summary == null) {
                summary = InvestmentSummary.of(member, totalInvestment, totalValuation, totalProfitLoss, returnRate);
            } else {
                summary.updateValues(totalInvestment, totalValuation, totalProfitLoss, returnRate);
            }
        } catch (Exception e) {
            log.error("투자 요약 업데이트 중 오류 발생", e);
            // 투자 요약 업데이트 실패는 거래 자체에 영향을 주지 않도록 예외 전파하지 않음
        }
    }

    @Override
    public void onStockPriceUpdate(String stockCode, JsonNode data) {
        // 실시간 가격 업데이트 시 지정가 주문 체결 확인
        try {
            if (data != null && data.has("10")) {
                String priceStr = data.get("10").asText().replace(",", "");
                try {
                    Long currentPrice = Long.parseLong(priceStr);
                    checkAndExecutePendingOrders(stockCode, currentPrice);
                } catch (NumberFormatException e) {
                    log.error("가격 변환 오류: {}", priceStr, e);
                }
            }
        } catch (Exception e) {
            log.error("지정가 주문 체결 확인 중 오류 발생", e);
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
            // 매수 주문 중 현재가가 지정가 이하인 주문 체결
            List<PendingOrder> buyOrders = pendingOrderRepository.findExecutableOrders(
                    stockCode, TradeHistory.TradeType.BUY, currentPrice);

            for (PendingOrder order : buyOrders) {
                executeBuyOrder(order, currentPrice);
            }

            // 매도 주문 중 현재가가 지정가 이상인 주문 체결
            List<PendingOrder> sellOrders = pendingOrderRepository.findExecutableOrders(
                    stockCode, TradeHistory.TradeType.SELL, currentPrice);

            for (PendingOrder order : sellOrders) {
                executeSellOrder(order, currentPrice);
            }
        } finally {
            lock.unlock();
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
            // 주문 상태를 먼저 처리 중으로 변경
            order.processing();
            pendingOrderRepository.save(order);

            Member member = order.getMember();
            StockData stock = order.getStockData();

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

            // 포지션 업데이트
            updateHoldingPosition(member, stock, order.getQuantity(), currentPrice, TradeHistory.TradeType.BUY);

            // 투자 요약 업데이트
            updateInvestmentSummary(member);

            // 주문 상태 업데이트
            order.complete();
            pendingOrderRepository.save(order);

            log.info("지정가 매수 주문 체결: 주문ID={}, 종목={}, 수량={}, 가격={}",
                    order.getOrderId(), stock.getShortCode(), order.getQuantity(), currentPrice);

            successfulTrades.incrementAndGet();
        } catch (Exception e) {
            log.error("지정가 매수 주문 체결 중 오류 발생: {}", e.getMessage(), e);

            // 오류 발생 시 주문 상태 원복
            order.fail();
            pendingOrderRepository.save(order);
            failedTrades.incrementAndGet();
            throw e;
        }
    }

    @Transactional
    private void executeSellOrder(PendingOrder order, Long currentPrice) {
        // 이미 처리된 주문인지 확인 (동시 실행 방지)
        if (order.getStatus() != PendingOrder.OrderStatus.PENDING) {
            log.warn("이미 처리된 주문 건너뛰기: {}", order.getOrderId());
            return;
        }

        try {
            // 주문 상태를 먼저 처리 중으로 변경
            order.processing();
            pendingOrderRepository.save(order);

            Member member = order.getMember();
            StockData stock = order.getStockData();

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
            updateHoldingPosition(member, stock, order.getQuantity(), currentPrice, TradeHistory.TradeType.SELL);

            // 투자 요약 업데이트
            updateInvestmentSummary(member);

            // 주문 상태 업데이트
            order.complete();
            pendingOrderRepository.save(order);

            log.info("지정가 매도 주문 체결: 주문ID={}, 종목={}, 수량={}, 가격={}",
                    order.getOrderId(), stock.getShortCode(), order.getQuantity(), currentPrice);

            successfulTrades.incrementAndGet();
        } catch (Exception e) {
            log.error("지정가 매도 주문 체결 중 오류 발생: {}", e.getMessage(), e);

            // 오류 발생 시 주문 상태 원복
            order.fail();
            pendingOrderRepository.save(order);
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

            return true;
        } catch (Exception e) {
            log.error("주문 취소 중 오류 발생: {}", e.getMessage(), e);
            return false;
        } finally {
            lock.unlock();
        }
    }

    // 지정가 주문 주기적 체결 처리를 위한 스케줄러 메서드
    public void processPendingOrders() {
        log.debug("지정가 주문 처리 스케줄러 실행");

        try {
            // 보류 중인 모든 주문 조회
            List<PendingOrder> pendingOrders = pendingOrderRepository.findByStatus(PendingOrder.OrderStatus.PENDING);

            if (pendingOrders.isEmpty()) {
                return;
            }

            // 종목별로 그룹화하여 효율적으로 처리
            Map<String, List<PendingOrder>> ordersByStockCode = pendingOrders.stream()
                    .collect(Collectors.groupingBy(order -> order.getStockData().getShortCode()));

            // 각 종목별로 현재가 조회 및 주문 처리
            for (Map.Entry<String, List<PendingOrder>> entry : ordersByStockCode.entrySet()) {
                String stockCode = entry.getKey();
                List<PendingOrder> orders = entry.getValue();

                // 현재가 조회
                Long currentPrice = getCurrentStockPriceWithRetry(stockCode);
                if (currentPrice == null) {
                    log.warn("종목 {} 현재가 조회 실패, 주문 처리 건너뜀", stockCode);
                    continue;
                }

                // 각 주문 처리
                for (PendingOrder order : orders) {
                    if (order.getOrderType() == TradeHistory.TradeType.BUY) {
                        // 매수 주문: 지정가가 현재가보다 크거나 같으면 체결
                        if (order.getTargetPrice() >= currentPrice) {
                            try {
                                executeBuyOrder(order, currentPrice);
                            } catch (Exception e) {
                                log.error("지정가 매수 주문 체결 중 오류 발생: orderId={}, error={}",
                                        order.getOrderId(), e.getMessage());
                            }
                        }
                    } else {
                        // 매도 주문: 지정가가 현재가보다 작거나 같으면 체결
                        if (order.getTargetPrice() <= currentPrice) {
                            try {
                                executeSellOrder(order, currentPrice);
                            } catch (Exception e) {
                                log.error("지정가 매도 주문 체결 중 오류 발생: orderId={}, error={}",
                                        order.getOrderId(), e.getMessage());
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("지정가 주문 처리 스케줄러 실행 중 오류 발생", e);
        }
    }

    // 매핑된 현재가 정보 반환 (메트릭 및 모니터링용)
    public Map<String, Long> getAllCurrentPrices() {
        Map<String, Long> prices = new HashMap<>();

        List<StockData> stocks = stockMasterDataRepository.findAll();
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
}