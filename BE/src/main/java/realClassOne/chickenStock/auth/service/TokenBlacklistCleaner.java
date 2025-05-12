package realClassOne.chickenStock.auth.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.Set;

// 매일 오전 01:00:00에 자동으로 Redis에 저장된 만료된 블랙리스트 토큰을 제거.
@Component
public class TokenBlacklistCleaner {
    private final RedisTemplate<String, String> redisTemplate;
    private final Logger logger = LoggerFactory.getLogger(TokenBlacklistCleaner.class);

    public TokenBlacklistCleaner(RedisTemplate<String, String> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    // 매일 01:00:00에 실행 (초, 분, 시, 일, 월, 요일)
    @Scheduled(cron = "0 0 1 * * *", zone = "Asia/Seoul")
    public void cleanExpiredBlacklistedTokens() {
        logger.info("[Scheduler Start] 블랙리스트 토큰 클리너 실행됨");
        Set<String> keys = redisTemplate.keys("blacklist:*");
        if (keys == null || keys.isEmpty()) {
            logger.info("블랙리스트에서 제거할 토큰이 없습니다.");
            return;
        }

        int expiredCount = 0;
        for (String key : keys) {
            // 각 키의 TTL(만료 시간)을 확인
            Long ttl = redisTemplate.getExpire(key);

            // 이미 만료되었거나 TTL이 0 이하인 경우 (TTL이 -1인 경우는 만료 설정이 없는 경우, -2는 키가 없는 경우)
            if (ttl != null && ttl <= 0 && ttl != -1) {
                redisTemplate.delete(key);
                expiredCount++;
            }
        }

        logger.info("블랙리스트에서 만료된 토큰 {}건 제거됨", expiredCount);
    }
}
