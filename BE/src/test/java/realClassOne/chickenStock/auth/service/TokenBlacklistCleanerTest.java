package realClassOne.chickenStock.auth.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.redis.core.RedisTemplate;

import java.util.HashSet;
import java.util.Set;

import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class TokenBlacklistCleanerTest {

    @Mock
    private RedisTemplate<String, String> redisTemplate;

    @InjectMocks
    private TokenBlacklistCleaner tokenBlacklistCleaner;

    @Test
    void cleanExpiredBlacklistedTokens_정상_작동() {
        // Given
        Set<String> keys = new HashSet<>();
        keys.add("blacklist:token1");
        keys.add("blacklist:token2");
        keys.add("blacklist:token3");

        when(redisTemplate.keys("blacklist:*")).thenReturn(keys);
        when(redisTemplate.getExpire("blacklist:token1")).thenReturn(0L);  // 만료된 키
        when(redisTemplate.getExpire("blacklist:token2")).thenReturn(-2L); // 키가 없는 경우
        when(redisTemplate.getExpire("blacklist:token3")).thenReturn(-1L); // 만료되지 않은 키

        // When
        tokenBlacklistCleaner.cleanExpiredBlacklistedTokens();

        // Then
        verify(redisTemplate).delete("blacklist:token1");  // 만료된 키는 삭제되어야 함
        verify(redisTemplate).delete("blacklist:token2");  // 키가 없는 경우도 삭제되어야 함
        verify(redisTemplate, never()).delete("blacklist:token3"); // 만료되지 않은 키는 삭제되지 않아야 함
    }

    @Test
    void cleanExpiredBlacklistedTokens_만료된_토큰_없음() {
        // Given
        Set<String> emptySet = new HashSet<>();
        when(redisTemplate.keys("blacklist:*")).thenReturn(emptySet);

        // When
        tokenBlacklistCleaner.cleanExpiredBlacklistedTokens();

        // Then
        verify(redisTemplate, never()).delete(anyString());  // 삭제되지 않아야 함
    }
}