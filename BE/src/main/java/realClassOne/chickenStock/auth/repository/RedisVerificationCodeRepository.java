package realClassOne.chickenStock.auth.repository;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Repository;

import java.time.Duration;
import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class RedisVerificationCodeRepository implements VerificationCodeRepository {

    private final RedisTemplate<String, Object> redisTemplate;

    private static final Duration CODE_TTL = Duration.ofMinutes(5);
    private static final Duration VERIFIED_TTL = Duration.ofMinutes(10);

    @Override
    public void saveCode(String email, String code) {
        redisTemplate.opsForValue().set("code:" + email, code, CODE_TTL);
    }

    @Override
    public Optional<String> getCode(String email) {
        Object value = redisTemplate.opsForValue().get("code:" + email);
        return Optional.ofNullable(value != null ? value.toString() : null);
    }

    @Override
    public void deleteCode(String email) {
        redisTemplate.delete("code:" + email);
    }

    @Override
    public void markVerified(String email) {
        redisTemplate.opsForValue().set("verified:" + email, "true", VERIFIED_TTL);
    }

    @Override
    public boolean isVerified(String email) {
        Object value = redisTemplate.opsForValue().get("verified:" + email);
        return "true".equals(value != null ? value.toString() : null);
    }

    @Override
    public void removeVerified(String email) {
        redisTemplate.delete("verified:" + email);
    }
}