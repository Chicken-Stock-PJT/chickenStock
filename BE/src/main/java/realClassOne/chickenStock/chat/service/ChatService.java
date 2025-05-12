package realClassOne.chickenStock.chat.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.chat.dto.ChatMessage;

import java.util.List;
import java.util.concurrent.TimeUnit;

@Service
@RequiredArgsConstructor
@Slf4j
public class ChatService {

    private final RedisTemplate<String, Object> redisTemplate;
    private static final String CHAT_HISTORY_KEY = "chat:history";
    private static final int CHAT_HISTORY_SIZE = 100; // 최근 100개 메시지 저장

    public void saveChatMessage(ChatMessage chatMessage) {
        try {
            // Redis에 채팅 메시지 저장 (리스트 형태)
            redisTemplate.opsForList().rightPush(CHAT_HISTORY_KEY, chatMessage);

            // 리스트 크기 제한
            Long size = redisTemplate.opsForList().size(CHAT_HISTORY_KEY);
            if (size != null && size > CHAT_HISTORY_SIZE) {
                redisTemplate.opsForList().leftPop(CHAT_HISTORY_KEY);
            }

            // 채팅 히스토리에 TTL 설정 (7일)
            redisTemplate.expire(CHAT_HISTORY_KEY, 7, TimeUnit.DAYS);

        } catch (Exception e) {
            log.error("채팅 메시지 저장 중 오류 발생", e);
        }
    }

    public List<Object> getChatHistory() {
        try {
            List<Object> messages = redisTemplate.opsForList().range(CHAT_HISTORY_KEY, 0, -1);
            return messages != null ? messages : List.of();
        } catch (Exception e) {
            log.error("채팅 히스토리 조회 중 오류 발생", e);
            return List.of();
        }
    }
}