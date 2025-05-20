package realClassOne.chickenStock.stock.service.trade;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.concurrent.TimeUnit;

@Service
@RequiredArgsConstructor
@Slf4j
public class DistributedLockService {

    private final StringRedisTemplate redisTemplate;

    // 락 키 접두사 및 기본 설정
    private static final String LOCK_KEY_PREFIX = "lock:";
    private static final long DEFAULT_EXPIRE_SECONDS = 10; // 기본 10초 타임아웃
    private static final long SPIN_WAIT_MILLIS = 100; // 스핀락 대기 간격

    /**
     * 분산 락 획득 시도 (스핀 락)
     * @param lockName 락 이름 (e.g. "stock:005930")
     * @param owner 락 소유자 식별자 (e.g. "thread-123")
     * @param waitTimeSeconds 대기 시간 (초)
     * @param expireTimeSeconds 락 만료 시간 (초)
     * @return 락 획득 성공 여부
     */
    public boolean tryLock(String lockName, String owner, long waitTimeSeconds, long expireTimeSeconds) {
        String lockKey = LOCK_KEY_PREFIX + lockName;
        long startTime = System.currentTimeMillis();
        long waitTimeMillis = waitTimeSeconds * 1000;

        try {
            while (System.currentTimeMillis() - startTime < waitTimeMillis) {
                // SET NX EX 명령으로 락 획득 시도
                Boolean acquired = redisTemplate.opsForValue()
                        .setIfAbsent(lockKey, owner, expireTimeSeconds, TimeUnit.SECONDS);

                if (Boolean.TRUE.equals(acquired)) {
                    log.debug("락 획득 성공: {}, 소유자: {}, 만료: {}초",
                            lockName, owner, expireTimeSeconds);
                    return true;
                }

                // 짧은 시간 대기 후 재시도
                Thread.sleep(SPIN_WAIT_MILLIS);
            }

            log.warn("락 획득 타임아웃: {}, 대기시간: {}초", lockName, waitTimeSeconds);
            return false;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.warn("락 획득 중 인터럽트 발생: {}", lockName);
            return false;
        } catch (Exception e) {
            log.error("락 획득 중 오류 발생: {}", lockName, e);
            return false;
        }
    }

    /**
     * 락 해제
     * @param lockName 락 이름
     * @param owner 락 소유자 식별자
     * @return 락 해제 성공 여부
     */
    public boolean unlock(String lockName, String owner) {
        String lockKey = LOCK_KEY_PREFIX + lockName;

        try {
            // 현재 락 소유자 확인 (다른 스레드의 락을 해제하지 않도록)
            String currentOwner = redisTemplate.opsForValue().get(lockKey);

            if (currentOwner == null) {
                // 락이 이미 만료됨
                log.warn("락 해제 실패: {}, 이미 만료됨", lockName);
                return false;
            }

            if (!currentOwner.equals(owner)) {
                // 다른 프로세스가 소유한 락
                log.warn("락 해제 실패: {}, 소유자 불일치 (현재: {}, 요청: {})",
                        lockName, currentOwner, owner);
                return false;
            }

            // 락 삭제
            Boolean deleted = redisTemplate.delete(lockKey);
            if (Boolean.TRUE.equals(deleted)) {
                log.debug("락 해제 성공: {}, 소유자: {}", lockName, owner);
                return true;
            } else {
                log.warn("락 해제 실패: {}, 삭제 명령 실패", lockName);
                return false;
            }
        } catch (Exception e) {
            log.error("락 해제 중 오류 발생: {}", lockName, e);
            return false;
        }
    }

    /**
     * 간단한 락 획득 메서드 (기본 설정 사용)
     */
    public boolean tryLock(String lockName, String owner) {
        return tryLock(lockName, owner, 5, DEFAULT_EXPIRE_SECONDS);
    }

    /**
     * 락 상태 확인
     */
    public boolean isLocked(String lockName) {
        String lockKey = LOCK_KEY_PREFIX + lockName;
        return redisTemplate.hasKey(lockKey);
    }

    /**
     * 락 소유자 확인
     */
    public String getLockOwner(String lockName) {
        String lockKey = LOCK_KEY_PREFIX + lockName;
        return redisTemplate.opsForValue().get(lockKey);
    }

    /**
     * 분산 락 획득 시도 (재시도 로직 포함)
     * @param lockName 락 이름 (e.g. "stock:005930")
     * @param owner 락 소유자 식별자 (e.g. "thread-123")
     * @param maxRetries 최대 재시도 횟수
     * @return 락 획득 성공 여부
     */
    public boolean tryLockWithRetry(String lockName, String owner, int maxRetries) {
        int retries = 0;
        long waitTimeMs = 100; // 초기 대기 시간

        while (retries < maxRetries) {
            boolean acquired = tryLock(lockName, owner, 2, 10);
            if (acquired) {
                log.debug("락 획득 성공 (재시도: {}): {}, 소유자: {}", retries, lockName, owner);
                return true;
            }

            retries++;
            log.debug("락 획득 실패, 재시도 {}/{}: {}", retries, maxRetries, lockName);

            try {
                Thread.sleep(waitTimeMs);
                waitTimeMs *= 1.5; // 지수 백오프
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.warn("락 획득 재시도 중 인터럽트 발생: {}", lockName);
                return false;
            }
        }

        log.warn("락 획득 최대 재시도 횟수 초과: {}, 재시도: {}", lockName, maxRetries);
        return false;
    }
}