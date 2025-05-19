package realClassOne.chickenStock.notification.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Service
@Slf4j
@RequiredArgsConstructor
public class FCMTokenService {

    private final RedisTemplate<String, String> redisTemplate;

    // Redis 키 이름 상수
    private static final String FCM_TOKEN_KEY_PREFIX = "fcm:token:";
    private static final long TOKEN_EXPIRY_DAYS = 60; // 토큰 60일 유효

    /**
     * 사용자 FCM 토큰 저장 (중복 방지를 위해 Set 사용)
     */
    public void saveUserToken(Long memberId, String token) {
        String key = FCM_TOKEN_KEY_PREFIX + memberId;
        redisTemplate.opsForSet().add(key, token);
        redisTemplate.expire(key, TOKEN_EXPIRY_DAYS, TimeUnit.DAYS);
//        log.info("FCM 토큰 저장 완료: 회원ID={}, 토큰={}", memberId, token);
    }

    /**
     * 사용자의 모든 FCM 토큰 조회
     */
    public List<String> getUserTokens(Long memberId) {
        String key = FCM_TOKEN_KEY_PREFIX + memberId;
        Set<String> tokens = redisTemplate.opsForSet().members(key);

        if (tokens == null || tokens.isEmpty()) {
            log.debug("회원 ID {}의 FCM 토큰이 없습니다.", memberId);
            return List.of();
        }

        return tokens.stream().collect(Collectors.toList());
    }

    /**
     * 사용자의 특정 FCM 토큰 삭제
     */
    public void removeUserToken(Long memberId, String token) {
        String key = FCM_TOKEN_KEY_PREFIX + memberId;
        redisTemplate.opsForSet().remove(key, token);
//        log.info("FCM 토큰 삭제 완료: 회원ID={}, 토큰={}", memberId, token);
    }

    /**
     * 사용자의 모든 FCM 토큰 삭제
     */
    public void removeAllUserTokens(Long memberId) {
        String key = FCM_TOKEN_KEY_PREFIX + memberId;
        redisTemplate.delete(key);
//        log.info("회원 ID {}의 모든 FCM 토큰 삭제 완료", memberId);
    }
}