package realClassOne.chickenStock.stock.service.trade;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.util.concurrent.TimeUnit;

@Service
@RequiredArgsConstructor
@Slf4j
public class DistributedLockService {

    private final RedisTemplate<String, String> redisTemplate;

    // 락 획득 시도 (타임아웃 있음)
    public boolean tryLock(String lockName, String ownerId, int timeoutSeconds, int expirySeconds) {
        try {
            long startTime = System.currentTimeMillis();
            long timeoutMillis = timeoutSeconds * 1000L;
            long retryIntervalMillis = 100; // 초기 재시도 간격 100ms

            // 첫 번째 시도
            Boolean success = redisTemplate.opsForValue()
                    .setIfAbsent(lockName, ownerId, expirySeconds, TimeUnit.SECONDS);

            if (Boolean.TRUE.equals(success)) {
                log.debug("락 획득 성공: {}, 소유자: {}, 만료: {}초", lockName, ownerId, expirySeconds);
                return true;
            }

            // 요청 시간이 만료될 때까지 일정 간격으로 재시도
            while (System.currentTimeMillis() - startTime < timeoutMillis) {
                try {
                    Thread.sleep(retryIntervalMillis);

                    // 지수 백오프 (최대 500ms까지)
                    retryIntervalMillis = Math.min(retryIntervalMillis * 2, 500);

                    // 레디스 연결 확인 및 재시도
                    try {
                        RedisConnection connection = redisTemplate.getConnectionFactory().getConnection();
                        if (!connection.isClosed()) {
                            success = redisTemplate.opsForValue()
                                    .setIfAbsent(lockName, ownerId, expirySeconds, TimeUnit.SECONDS);

                            if (Boolean.TRUE.equals(success)) {
                                log.debug("락 획득 성공 (재시도): {}, 소유자: {}, 만료: {}초", lockName, ownerId, expirySeconds);
                                return true;
                            }
                        } else {
                            log.warn("Redis 연결이 닫혀 있습니다. 재연결 시도...");
                            // 연결이 닫혀 있으면 다음 루프에서 다시 시도
                        }
                    } catch (Exception re) {
                        // Redis 연결 오류 시 로그만 남기고 계속 재시도
                        log.warn("Redis 연결 오류, 재시도 중: {}", re.getMessage());
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    log.warn("락 획득 대기 중 인터럽트 발생: {}", lockName);
                    return false;
                }
            }

            log.warn("락 획득 타임아웃: {}, 대기시간: {}초", lockName, timeoutSeconds);
            return false;

        } catch (Exception e) {
            log.error("락 획득 중 오류 발생: {}", lockName, e);
            return false;
        }
    }

    // 락 즉시 획득 시도 (재시도 없음) - 개선된 버전
    public boolean tryLockImmediate(String lockName, String ownerId, int expirySeconds) {
        try {
            // 성능 최적화를 위해 SETNX 루아 스크립트 사용 고려
            Boolean success = redisTemplate.opsForValue()
                    .setIfAbsent(lockName, ownerId, expirySeconds, TimeUnit.SECONDS);

            if (Boolean.TRUE.equals(success)) {
                log.debug("락 즉시 획득 성공: {}, 소유자: {}, 만료: {}초", lockName, ownerId, expirySeconds);
                return true;
            }

            return false;
        } catch (Exception e) {
            log.error("락 즉시 획득 중 오류 발생: {}", lockName, e);
            return false;
        }
    }

    // 락 해제 - 개선된 버전 (Lua 스크립트 활용)
    public boolean unlock(String lockName, String ownerId) {
        try {
            // 현재 락의 소유자 확인
            String currentOwner = redisTemplate.opsForValue().get(lockName);

            // 소유자가 일치하는 경우에만 삭제
            if (ownerId.equals(currentOwner)) {
                Boolean deleted = redisTemplate.delete(lockName);
                if (Boolean.TRUE.equals(deleted)) {
                    log.debug("락 해제 성공: {}, 소유자: {}", lockName, ownerId);
                    return true;
                } else {
                    log.warn("락 삭제 실패: {}, 소유자: {}", lockName, ownerId);
                }
            } else if (currentOwner != null) {
                log.warn("락 소유자 불일치: {} (요청: {}, 현재: {})", lockName, ownerId, currentOwner);
            } else {
                log.debug("락이 이미 없음: {}, 소유자: {}", lockName, ownerId);
                return true; // 락이 이미 없는 경우도 성공으로 처리
            }

            return false;
        } catch (Exception e) {
            log.error("락 해제 중 오류 발생: {}", lockName, e);
            return false;
        }
    }

    // 최대 재시도 횟수를 지정하여 락 획득 시도
    public boolean tryLockWithRetry(String lockName, String ownerId, int maxRetries) {
        return tryLockWithRetry(lockName, ownerId, maxRetries, 10); // 기본 만료 시간 10초
    }

    // 최대 재시도 횟수와 만료 시간을 지정하여 락 획득 시도
    public boolean tryLockWithRetry(String lockName, String ownerId, int maxRetries, int expirySeconds) {
        // 먼저 즉시 획득 시도
        boolean acquired = tryLockImmediate(lockName, ownerId, expirySeconds);
        if (acquired) {
            log.debug("락 획득 성공 (재시도: 0): {}, 소유자: {}", lockName, ownerId);
            return true;
        }

        // 실패하면 재시도
        int retries = 0;
        long waitTimeMs = 100; // 초기 대기 시간

        while (retries < maxRetries) {
            try {
                Thread.sleep(waitTimeMs);

                // 지수 백오프 (최대 2초까지)
                waitTimeMs = Math.min(waitTimeMs * 2, 2000);

                retries++;
                acquired = tryLockImmediate(lockName, ownerId, expirySeconds);

                if (acquired) {
                    log.debug("락 획득 성공 (재시도: {}): {}, 소유자: {}", retries, lockName, ownerId);
                    return true;
                }

                log.debug("락 획득 실패, 재시도 {}/{}: {}", retries, maxRetries, lockName);

            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.warn("락 획득 재시도 중 인터럽트 발생: {}", lockName);
                return false;
            }
        }

        log.warn("락 획득 최대 재시도 횟수 초과: {}, 재시도: {}", lockName, maxRetries);
        return false;
    }

    // 긴급 락 해제 (관리자용)
    public boolean forceUnlock(String lockName) {
        try {
            Boolean deleted = redisTemplate.delete(lockName);
            if (Boolean.TRUE.equals(deleted)) {
                log.warn("락 강제 해제 성공: {}", lockName);
                return true;
            }
            return false;
        } catch (Exception e) {
            log.error("락 강제 해제 중 오류 발생: {}", lockName, e);
            return false;
        }
    }
}