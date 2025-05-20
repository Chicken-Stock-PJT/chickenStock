package realClassOne.chickenStock.stock.trade.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

@Service
@Slf4j
public class KiwoomApiCircuitBreaker {

    private enum State { CLOSED, OPEN, HALF_OPEN }

    private final AtomicReference<State> state = new AtomicReference<>(State.CLOSED);
    private final AtomicInteger failureCount = new AtomicInteger(0);
    private final AtomicLong lastFailureTime = new AtomicLong(0);
    private final int failureThreshold = 5;
    private final long resetTimeoutMs = 30000; // 30초

    /**
     * API 호출을 실행하고 서킷 브레이커 상태를 관리
     */
    public <T> T executeCall(Supplier<T> apiCall, T fallbackValue) {
        switch (state.get()) {
            case CLOSED:
                try {
                    T result = apiCall.get();
                    failureCount.set(0); // 성공시 카운트 리셋
                    return result;
                } catch (Exception e) {
                    int failures = failureCount.incrementAndGet();
                    lastFailureTime.set(System.currentTimeMillis());

                    if (failures >= failureThreshold) {
                        log.warn("서킷 브레이커 상태 변경: CLOSED -> OPEN (실패 횟수: {})", failures);
                        state.set(State.OPEN);
                    }

                    log.error("API 호출 실패: {}", e.getMessage());
                    return fallbackValue;
                }

            case OPEN:
                // 일정 시간 경과 후 HALF_OPEN으로 전환
                long now = System.currentTimeMillis();
                if (now - lastFailureTime.get() > resetTimeoutMs) {
                    log.info("서킷 브레이커 상태 변경: OPEN -> HALF_OPEN");
                    state.set(State.HALF_OPEN);
                    return executeCall(apiCall, fallbackValue); // 재귀 호출로 HALF_OPEN 상태에서 처리
                }
                return fallbackValue;

            case HALF_OPEN:
                try {
                    T result = apiCall.get();
                    log.info("서킷 브레이커 상태 변경: HALF_OPEN -> CLOSED (복구 성공)");
                    state.set(State.CLOSED);
                    failureCount.set(0);
                    return result;
                } catch (Exception e) {
                    log.error("HALF_OPEN 상태에서 API 호출 실패: {}", e.getMessage());
                    state.set(State.OPEN);
                    lastFailureTime.set(System.currentTimeMillis());
                    return fallbackValue;
                }

            default:
                return fallbackValue;
        }
    }
}