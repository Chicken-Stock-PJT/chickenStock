package realClassOne.chickenStock.stock.service.trade;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.dto.common.PendingOrderDTO;
import realClassOne.chickenStock.stock.dto.request.TradeRequestDTO;
import realClassOne.chickenStock.stock.dto.response.TradeResponseDTO;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;
import realClassOne.chickenStock.stock.service.StockTradeService;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Service
@RequiredArgsConstructor
@Slf4j
public class StockTradeFacadeService {

    private final StockTradeService stockTradeService;
    private final JwtTokenProvider jwtTokenProvider;
    private final MemberRepository memberRepository;
    private final StockDataRepository stockDataRepository;
    private final TradingSecurityService tradingSecurityService;
    private final DistributedLockService distributedLockService;
    private final StockPriceCacheService stockPriceCacheService;
    private final KiwoomApiCircuitBreaker circuitBreaker;

    // 스레드별 락 관리를 위한 ThreadLocal 추가
    private final ThreadLocal<ConcurrentHashMap<String, String>> threadLocalLocks =
            ThreadLocal.withInitial(ConcurrentHashMap::new);

    /**
     * 매수 주문 처리 메서드 - 향상된 락 처리
     */
    public TradeResponseDTO processBuyOrder(String authorization, TradeRequestDTO request) {
        String lockName = "trade:" + request.getStockCode();
        String ownerId = null;
        boolean lockAcquired = false;

        try {
            // 1. 토큰으로부터 회원 정보 추출
            String token = jwtTokenProvider.resolveToken(authorization);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
            ownerId = "member-" + memberId + "-" + UUID.randomUUID();

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 2. 종목 정보 조회
            StockData stockData = stockDataRepository.findByShortCode(request.getStockCode())
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 3. 요청 유효성 검증
            validateTradeRequest(request);

            // 4. 비정상 거래 패턴 체크
            checkTradingSecurity(memberId, request.getStockCode(), "BUY");

            // 5. 가격 미리 조회 (캐시에서만)
            Long cachedPrice = stockPriceCacheService.getFromCacheOnly(request.getStockCode());

            // 시장가 주문이고 가격도 캐시에 있는 경우, 즉시 처리 (락 획득 전)
            if (Boolean.TRUE.equals(request.getMarketOrder()) && cachedPrice != null) {
                log.debug("캐시된 가격으로 시장가 매수 처리: memberId={}, stockCode={}, price={}",
                        memberId, request.getStockCode(), cachedPrice);
                return stockTradeService.buyStockInternal(authorization, request);
            }

            // 6. 락 획득 시도 - 향상된 로직 (지수 백오프 및 더 많은 재시도)
            lockAcquired = acquireLockWithBackoff(lockName, ownerId);
            if (!lockAcquired) {
                log.warn("거래 락 획득 실패: memberId={}, stockCode={}", memberId, request.getStockCode());

                // 락 획득 실패 시 캐시 직접 갱신 시도 (강제 리프레시)
                try {
                    Long refreshedPrice = stockPriceCacheService.getCurrentStockPrice(request.getStockCode());
                    if (refreshedPrice != null && Boolean.TRUE.equals(request.getMarketOrder())) {
                        log.info("락 획득 실패 후 캐시 리프레시 성공, 시장가 매수 처리: price={}", refreshedPrice);
                        return stockTradeService.buyStockInternal(authorization, request);
                    }
                } catch (Exception e) {
                    log.error("캐시 리프레시 실패", e);
                }

                return createErrorResponse("현재 해당 종목에 대한 거래가 많습니다. 잠시 후 다시 시도해주세요.");
            }

            // 7. buyStockInternal 메서드 호출 - 이 메서드는 락 관리를 하지 않음
            log.debug("락 획득 성공 후 buyStockInternal 메서드 호출: memberId={}, stockCode={}",
                    memberId, request.getStockCode());
            return stockTradeService.buyStockInternal(authorization, request);

        } catch (CustomException e) {
            log.error("매수 주문 처리 중 예외 발생: {}", e.getMessage());
            return createErrorResponse(e.getMessage());
        } catch (Exception e) {
            log.error("매수 주문 처리 중 시스템 오류", e);
            return createErrorResponse("시스템 오류가 발생했습니다.");
        } finally {
            // 8. 락 해제
            if (lockAcquired && ownerId != null) {
                releaseLock(lockName, ownerId);
            }

            // ThreadLocal 내용 정리 (메모리 누수 방지)
            cleanupThreadLocal();
        }
    }

    /**
     * 매도 주문 처리 - 향상된 락 처리
     */
    public TradeResponseDTO processSellOrder(String authorization, TradeRequestDTO request) {
        String lockName = "trade:" + request.getStockCode();
        String ownerId = null;
        boolean lockAcquired = false;

        try {
            // 1. 토큰으로부터 회원 정보 추출
            String token = jwtTokenProvider.resolveToken(authorization);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
            ownerId = "member-" + memberId + "-" + UUID.randomUUID();

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 2. 종목 정보 조회
            StockData stockData = stockDataRepository.findByShortCode(request.getStockCode())
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 3. 요청 유효성 검증
            validateTradeRequest(request);

            // 4. 비정상 거래 패턴 체크
            checkTradingSecurity(memberId, request.getStockCode(), "SELL");

            // 5. 가격 미리 조회 (캐시에서만)
            Long cachedPrice = stockPriceCacheService.getFromCacheOnly(request.getStockCode());

            // 시장가 주문이고 가격도 캐시에 있는 경우, 즉시 처리 (락 획득 전)
            if (Boolean.TRUE.equals(request.getMarketOrder()) && cachedPrice != null) {
                log.debug("캐시된 가격으로 시장가 매도 처리: memberId={}, stockCode={}, price={}",
                        memberId, request.getStockCode(), cachedPrice);
                return stockTradeService.sellStockInternal(authorization, request);
            }

            // 6. 락 획득 시도 - 향상된 로직
            lockAcquired = acquireLockWithBackoff(lockName, ownerId);
            if (!lockAcquired) {
                log.warn("거래 락 획득 실패: memberId={}, stockCode={}", memberId, request.getStockCode());

                // 락 획득 실패 시 캐시 직접 갱신 시도 (강제 리프레시)
                try {
                    Long refreshedPrice = stockPriceCacheService.getCurrentStockPrice(request.getStockCode());
                    if (refreshedPrice != null && Boolean.TRUE.equals(request.getMarketOrder())) {
                        log.info("락 획득 실패 후 캐시 리프레시 성공, 시장가 매도 처리: price={}", refreshedPrice);
                        return stockTradeService.sellStockInternal(authorization, request);
                    }
                } catch (Exception e) {
                    log.error("캐시 리프레시 실패", e);
                }

                return createErrorResponse("현재 해당 종목에 대한 거래가 많습니다. 잠시 후 다시 시도해주세요.");
            }

            // 7. sellStockInternal 메서드 호출 - 이 메서드는 락 관리를 하지 않음
            log.debug("락 획득 성공 후 sellStockInternal 메서드 호출: memberId={}, stockCode={}",
                    memberId, request.getStockCode());
            return stockTradeService.sellStockInternal(authorization, request);

        } catch (CustomException e) {
            log.error("매도 주문 처리 중 예외 발생: {}", e.getMessage());
            return createErrorResponse(e.getMessage());
        } catch (Exception e) {
            log.error("매도 주문 처리 중 시스템 오류", e);
            return createErrorResponse("시스템 오류가 발생했습니다.");
        } finally {
            // 8. 락 해제
            if (lockAcquired && ownerId != null) {
                releaseLock(lockName, ownerId);
            }

            // ThreadLocal 내용 정리 (메모리 누수 방지)
            cleanupThreadLocal();
        }
    }

    /**
     * 미체결 주문 조회
     */
    public List<PendingOrderDTO> getPendingOrdersByMember(String authorization) {
        return stockTradeService.getPendingOrdersByMember(authorization);
    }

    /**
     * 주문 취소 처리
     */
    public boolean cancelPendingOrder(String authorization, Long orderId) {
        return stockTradeService.cancelPendingOrder(authorization, orderId);
    }

    // 요청 유효성 검증
    private void validateTradeRequest(TradeRequestDTO request) {
        if (request.getStockCode() == null || request.getStockCode().trim().isEmpty()) {
            throw new CustomException(StockErrorCode.INVALID_REQUEST, "종목 코드가 필요합니다.");
        }

        if (request.getQuantity() == null || request.getQuantity() <= 0) {
            throw new CustomException(StockErrorCode.INVALID_QUANTITY, "유효한 수량이 필요합니다.");
        }

        // 지정가 주문인 경우 가격 검증
        if (Boolean.FALSE.equals(request.getMarketOrder()) && (request.getPrice() == null || request.getPrice() <= 0)) {
            throw new CustomException(StockErrorCode.INVALID_PRICE, "지정가 주문에는 가격이 필요합니다.");
        }
    }

    // 거래 보안 검사
    private void checkTradingSecurity(Long memberId, String stockCode, String tradeType) {
        // 비정상 거래 패턴 체크
        boolean isAbnormal = tradingSecurityService.checkAbnormalTradingPattern(
                memberId, stockCode, tradeType);

        // 제한된 회원인지 체크
        boolean isRestricted = tradingSecurityService.isRestricted(memberId);

        if (isAbnormal || isRestricted) {
            throw new CustomException(StockErrorCode.TRADE_RESTRICTED,
                    "비정상적인 거래 패턴이 감지되어 제한되었습니다.");
        }
    }

    // 에러 응답 생성
    private TradeResponseDTO createErrorResponse(String message) {
        return TradeResponseDTO.builder()
                .status("ERROR")
                .message(message)
                .build();
    }

    // 향상된 락 획득 메서드 (지수 백오프 포함)
    private boolean acquireLockWithBackoff(String lockName, String ownerId) {
        // 이미 현재 스레드에서 획득한 락인지 확인
        if (threadLocalLocks.get().containsKey(lockName)) {
            log.debug("이미 획득한 락 재사용: {}, 소유자: {}", lockName, ownerId);
            return true;
        }

        // 최대 재시도 횟수와 초기 대기 시간
        int maxRetries = 8; // 최대 재시도 횟수 증가
        int retryCount = 0;
        long waitTimeMs = 100; // 초기 대기 시간

        while (retryCount < maxRetries) {
            // 락 획득 시도
            boolean acquired = distributedLockService.tryLockImmediate(lockName, ownerId, 10);

            if (acquired) {
                // 획득 성공 시 ThreadLocal에 저장
                threadLocalLocks.get().put(lockName, ownerId);
                log.debug("새로운 락 획득 성공 (재시도: {}): {}, 소유자: {}", retryCount, lockName, ownerId);
                return true;
            }

            // 실패 시 재시도
            retryCount++;
            try {
                // 지수 백오프 적용 (최대 2초까지)
                long backoffMs = Math.min(waitTimeMs * (long)Math.pow(1.5, retryCount), 2000);
                log.debug("락 획득 실패, {}ms 후 재시도 {}/{}: {}", backoffMs, retryCount, maxRetries, lockName);
                Thread.sleep(backoffMs);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.warn("락 획득 대기 중 인터럽트 발생: {}", lockName);
                return false;
            }
        }

        log.warn("락 획득 최대 재시도 횟수 초과: {}, 재시도: {}", lockName, maxRetries);
        return false;
    }

    // 락 해제 메서드 (ThreadLocal 사용)
    private void releaseLock(String lockName, String ownerId) {
        // ThreadLocal에서 락 정보 조회
        if (threadLocalLocks.get().containsKey(lockName)) {
            boolean unlocked = distributedLockService.unlock(lockName, ownerId);
            if (unlocked) {
                threadLocalLocks.get().remove(lockName);
                log.debug("거래 락 해제 완료: lockName={}, ownerId={}", lockName, ownerId);
            } else {
                log.warn("거래 락 해제 실패: lockName={}, ownerId={}", lockName, ownerId);
            }
        }
    }

    // ThreadLocal 정리 (메모리 누수 방지)
    private void cleanupThreadLocal() {
        try {
            // 현재 스레드에 남아있는 모든 락 해제 시도
            ConcurrentHashMap<String, String> locks = threadLocalLocks.get();
            if (locks != null && !locks.isEmpty()) {
                for (String lockName : locks.keySet()) {
                    String ownerId = locks.get(lockName);
                    distributedLockService.unlock(lockName, ownerId);
                    log.debug("스레드 종료 시 미해제 락 정리: lockName={}, ownerId={}", lockName, ownerId);
                }
                locks.clear();
            }
        } catch (Exception e) {
            log.error("ThreadLocal 정리 중 오류", e);
        }
    }
}