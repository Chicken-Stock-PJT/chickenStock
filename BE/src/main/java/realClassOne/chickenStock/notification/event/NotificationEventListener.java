package realClassOne.chickenStock.notification.event;


import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.chat.websocket.handler.ChatWebSocketHandler;
import realClassOne.chickenStock.notification.entity.Notification;
import realClassOne.chickenStock.notification.event.CommentNotificationEvent;
import realClassOne.chickenStock.notification.event.LikeNotificationEvent;
import realClassOne.chickenStock.notification.event.TradeNotificationEvent;
import realClassOne.chickenStock.notification.repository.NotificationRepository;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Component
@Slf4j
@RequiredArgsConstructor
public class NotificationEventListener {

    private final ChatWebSocketHandler chatWebSocketHandler;
    private final NotificationRepository notificationRepository;
    private final ObjectMapper objectMapper;

//    @EventListener
    public void handleTradeNotificationEvent(TradeNotificationEvent event) {
        try {
            log.info("트레이드 알림 이벤트 수신: memberId={}, stockName={}, orderType={}, quantity={}, price={}",
                    event.getMemberId(), event.getStockName(), event.getOrderType(), event.getQuantity(), event.getPrice());

            Long memberId = event.getMemberId();

            // 알림 조회 (DB에 저장된 가장 최근 해당 유형 알림)
            List<Notification> notifications = notificationRepository.findByMemberIdAndTypeOrderByCreatedAtDesc(
                    memberId, "TRADE");

            log.info("회원 ID {} 의 TRADE 타입 알림 개수: {}", memberId, notifications.size());

            Notification notification = notifications.isEmpty() ? null : notifications.get(0);

            if (notification != null) {
                log.info("알림 ID: {}, 메시지: {}", notification.getId(), notification.getMessage());

                // 웹소켓으로 알림 전송
                Map<String, Object> payload = new HashMap<>();
                payload.put("type", "notification");
                payload.put("notificationType", "TRADE");
                payload.put("title", notification.getTitle());
                payload.put("message", notification.getMessage());
                payload.put("timestamp", notification.getCreatedAt().toInstant(ZoneOffset.UTC).toEpochMilli());
                payload.put("notificationId", notification.getId());
                payload.put("isRead", notification.isRead());

                String message = objectMapper.writeValueAsString(payload);

                log.info("웹소켓으로 전송할 메시지: {}", message);

                // 웹소켓 세션이 존재하는지 확인하는 로직 추가
                boolean sessionExists = chatWebSocketHandler.hasSession(memberId);
                log.info("회원 ID {}의 웹소켓 세션 존재 여부: {}", memberId, sessionExists);

                chatWebSocketHandler.sendMessageToUser(memberId, message);

                log.info("지정가 체결 알림 전송 완료: 회원ID={}, 종목={}, 가격={}원, 수량={}주",
                        memberId, event.getStockName(), event.getPrice(), event.getQuantity());
            } else {
                log.warn("해당 트레이드 알림을 찾을 수 없음: 회원ID={}", memberId);
            }
        } catch (Exception e) {
            log.error("지정가 체결 알림 이벤트 처리 중 오류 발생", e);
        }
    }


    @EventListener
    public void handleCommentNotificationEvent(CommentNotificationEvent event) {
        try {
            Long memberId = event.getMemberId();

            // 알림 조회 (DB에 저장된 가장 최근 해당 유형 알림)
            Notification notification = notificationRepository.findByMemberIdAndTypeOrderByCreatedAtDesc(
                    memberId, "COMMENT").stream().findFirst().orElse(null);

            if (notification != null) {
                // 웹소켓으로 알림 전송
                Map<String, Object> payload = new HashMap<>();
                payload.put("type", "notification");
                payload.put("notificationType", "COMMENT");
                payload.put("title", notification.getTitle());
                payload.put("message", notification.getMessage());
                payload.put("timestamp", notification.getCreatedAt().toInstant(ZoneOffset.UTC).toEpochMilli());
                payload.put("notificationId", notification.getId());
                payload.put("isRead", notification.isRead());
                if (notification.getRelatedId() != null) {
                    payload.put("relatedId", notification.getRelatedId());
                }

                String message = objectMapper.writeValueAsString(payload);
                chatWebSocketHandler.sendMessageToUser(memberId, message);

                log.info("댓글 알림 전송 완료: 회원ID={}, 종목={}, 작성자={}",
                        memberId, event.getStockName(), event.getCommenterNickname());
            } else {
                log.warn("해당 댓글 알림을 찾을 수 없음: 회원ID={}", memberId);
            }
        } catch (Exception e) {
            log.error("댓글 알림 이벤트 처리 중 오류 발생", e);
        }
    }

    @EventListener
    public void handleLikeNotificationEvent(LikeNotificationEvent event) {
        try {
            Long memberId = event.getMemberId();

            // 알림 조회 (DB에 저장된 가장 최근 해당 유형 알림)
            Notification notification = notificationRepository.findByMemberIdAndTypeOrderByCreatedAtDesc(
                    memberId, "LIKE").stream().findFirst().orElse(null);

            if (notification != null) {
                // 웹소켓으로 알림 전송
                Map<String, Object> payload = new HashMap<>();
                payload.put("type", "notification");
                payload.put("notificationType", "LIKE");
                payload.put("title", notification.getTitle());
                payload.put("message", notification.getMessage());
                payload.put("timestamp", notification.getCreatedAt().toInstant(ZoneOffset.UTC).toEpochMilli());
                payload.put("notificationId", notification.getId());
                payload.put("isRead", notification.isRead());
                if (notification.getRelatedId() != null) {
                    payload.put("relatedId", notification.getRelatedId());
                }

                String message = objectMapper.writeValueAsString(payload);
                chatWebSocketHandler.sendMessageToUser(memberId, message);

                log.info("좋아요 알림 전송 완료: 회원ID={}, 종목={}, 작성자={}",
                        memberId, event.getStockName(), event.getLikerNickname());
            } else {
                log.warn("해당 좋아요 알림을 찾을 수 없음: 회원ID={}", memberId);
            }
        } catch (Exception e) {
            log.error("좋아요 알림 이벤트 처리 중 오류 발생", e);
        }
    }
}
