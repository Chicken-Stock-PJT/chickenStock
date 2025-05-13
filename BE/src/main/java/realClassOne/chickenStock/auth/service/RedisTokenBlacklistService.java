package realClassOne.chickenStock.auth.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.auth.exception.AuthErrorCode;
import realClassOne.chickenStock.common.exception.CustomException;

import java.util.Date;
import java.util.concurrent.TimeUnit;

@Service
@RequiredArgsConstructor
public class RedisTokenBlacklistService {

    private final RedisTemplate<String, String> redisTemplate;
    private static final String ONE_TIME_CODE_PREFIX = "onetime:";
    private static final String BLACKLIST_PREFIX = "blacklist:";


    public void addToBlacklist(String token, long timeToLiveMillis) {
        String key = BLACKLIST_PREFIX + token;
        redisTemplate.opsForValue().set(key, "1");
        redisTemplate.expire(key, timeToLiveMillis, TimeUnit.MILLISECONDS);
    }

    // Check if token is in blacklist
    public boolean isBlacklisted(String token) {
        String key = BLACKLIST_PREFIX + token;
        return Boolean.TRUE.equals(redisTemplate.hasKey(key));
    }

    // Store one-time code
    public void storeOneTimeCode(String oneTimeCode, Long memberId) {
        try {
            String key = ONE_TIME_CODE_PREFIX + oneTimeCode;
            long expireAt = System.currentTimeMillis() + (10 * 60 * 1000L); // 10분
            redisTemplate.opsForValue().set(key, memberId.toString());
            System.out.println("Stored in Redis: key=" + key + ", value=" + redisTemplate.opsForValue().get(key));
            redisTemplate.expireAt(key, new Date(expireAt));
            System.out.println("redisTemplate: " + redisTemplate);

        } catch (Exception e) {
            throw new CustomException(AuthErrorCode.REDIS_OPERATION_FAILED);
        }
    }

    // Get token from one-time code (fixed key consistency)
    public String getTokenFromOneTimeCode(String oneTimeCode) {
        String key = ONE_TIME_CODE_PREFIX + oneTimeCode;
        String memberId = redisTemplate.opsForValue().get(key);  // No "_memberId" suffix here

        redisTemplate.delete(key);  // Delete the key after use

        return memberId;
    }
}
