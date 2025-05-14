package realClassOne.chickenStock.chat.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.chat.dto.ChatMessage;

import java.util.List;
import java.util.concurrent.TimeUnit;

// ChatService 개선
@Service
@RequiredArgsConstructor
@Slf4j
public class ChatService {

    private final RedisTemplate<String, Object> redisTemplate;
    private static final String CHAT_HISTORY_KEY = "chat:history";
    private static final int CHAT_HISTORY_SIZE = 100;
    private static final int RETRY_ATTEMPTS = 3;
    private static final long RETRY_DELAY_MS = 100;

    @Retryable(value = {Exception.class}, maxAttempts = RETRY_ATTEMPTS, backoff = @Backoff(delay = RETRY_DELAY_MS))
    public void saveChatMessage(ChatMessage chatMessage) {
        try {
            // 메시지 유효성 검증
            if (chatMessage == null || chatMessage.getMessage() == null || chatMessage.getMessage().trim().isEmpty()) {
                log.warn("유효하지 않은 채팅 메시지 시도");
                return;
            }

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
            // 실패 시 로컬 파일 시스템에 백업 저장
            saveMessageToFailoverStorage(chatMessage);
            throw e; // Retry를 위해 예외 재발생
        }
    }

    public List<Object> getChatHistory() {
        try {
            List<Object> messages = redisTemplate.opsForList().range(CHAT_HISTORY_KEY, 0, -1);

            if (messages == null || messages.isEmpty()) {
                // Redis에 데이터가 없으면 백업에서 로드 시도
                messages = loadFromFailoverStorage();
            }

            return messages != null ? messages : List.of();
        } catch (Exception e) {
            log.error("채팅 히스토리 조회 중 오류 발생", e);
            // 실패 시 백업 스토리지에서 로드
            return loadFromFailoverStorage();
        }
    }

    // 채팅 방 별 메시지 관리 (향후 확장을 위해)
    public void saveChatMessageToRoom(String roomId, ChatMessage chatMessage) {
        String roomKey = "chat:room:" + roomId;
        try {
            redisTemplate.opsForList().rightPush(roomKey, chatMessage);

            Long size = redisTemplate.opsForList().size(roomKey);
            if (size != null && size > CHAT_HISTORY_SIZE) {
                redisTemplate.opsForList().leftPop(roomKey);
            }

            redisTemplate.expire(roomKey, 7, TimeUnit.DAYS);
        } catch (Exception e) {
            log.error("룸 채팅 메시지 저장 실패: {}", roomId, e);
            throw e;
        }
    }

    // 백업 스토리지 메서드 (예시)
    private void saveMessageToFailoverStorage(ChatMessage message) {
        // 실제 구현 시 파일 시스템이나 다른 DB에 저장
        log.info("백업 스토리지에 메시지 저장: {}", message);
    }

    private List<Object> loadFromFailoverStorage() {
        // 실제 구현 시 백업에서 로드
        log.info("백업 스토리지에서 메시지 로드");
        return List.of();
    }
}