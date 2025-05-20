package realClassOne.chickenStock.notification.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.chat.websocket.handler.ChatWebSocketHandler;
import realClassOne.chickenStock.notification.entity.Notification;
import realClassOne.chickenStock.notification.event.CommentNotificationEvent;
import realClassOne.chickenStock.notification.event.LikeNotificationEvent;
import realClassOne.chickenStock.notification.repository.NotificationRepository;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class NotificationService {

    private final NotificationRepository notificationRepository;
    private final ApplicationEventPublisher eventPublisher;
    private final ChatWebSocketHandler chatWebSocketHandler;
    private final FCMTokenService fcmTokenService;
    private final FCMService fcmService;
    private final ObjectMapper objectMapper;

    // 지정가 체결 알림 생성 및 전송
    @Transactional
    public void createTradeNotification(Long memberId, String stockName, String orderType,
                                        Integer quantity, Long price) {
        log.info("지정가 체결 알림 생성 시작: 회원ID={}, 종목={}, 타입={}, 수량={}, 가격={}",
                memberId, stockName, orderType, quantity, price);

        boolean isBuy = "BUY".equalsIgnoreCase(orderType);

        // DB에 알림 저장
        Notification notification = Notification.builder()
                .memberId(memberId)
                .type("TRADE")
                .title("지정가 체결 알림")
                .message(String.format("%s %s %d주가 %d원에 체결되었습니다.",
                        stockName, isBuy ? "매수" : "매도", quantity, price))
                .isRead(false)
                .createdAt(LocalDateTime.now())
                .build();

        Notification savedNotification = notificationRepository.save(notification);
//        log.info("알림 DB 저장 완료: 알림ID={}", savedNotification.getId());

        // 직접적인 웹소켓 알림 전송만 사용 (이벤트 발행 제거)
        try {
            sendDirectTradeNotification(savedNotification);
        } catch (Exception e) {
            log.error("직접 웹소켓 알림 전송 중 오류 발생", e);
        }
    }

    // 직접 알림 전송 메서드 (이벤트 메커니즘을 우회)
    private void sendDirectTradeNotification(Notification notification) {
        try {
            Long memberId = notification.getMemberId();
            boolean sessionExists = chatWebSocketHandler.hasSession(memberId);
//            log.info("직접 알림 전송: 회원ID={}, 웹소켓 세션 존재={}", memberId, sessionExists);

            if (sessionExists) {
                Map<String, Object> payload = new HashMap<>();
                payload.put("type", "notification");
                payload.put("notificationType", notification.getType());
                payload.put("title", notification.getTitle());
                payload.put("message", notification.getMessage());
                payload.put("timestamp", notification.getCreatedAt().toInstant(ZoneOffset.UTC).toEpochMilli());
                payload.put("notificationId", notification.getId());
                payload.put("isRead", notification.isRead());

                String message = objectMapper.writeValueAsString(payload);
//                log.info("직접 전송할 웹소켓 메시지: {}", message);

                chatWebSocketHandler.sendMessageToUser(memberId, message);
//                log.info("웹소켓을 통한 직접 알림 전송 완료: 회원ID={}", memberId);
            } else {
                log.warn("웹소켓 세션이 없어 직접 알림을 전송할 수 없습니다: 회원ID={}", memberId);
            }
        } catch (Exception e) {
            log.error("직접 알림 전송 중 오류 발생", e);
        }
    }

    // 댓글 알림 생성 및 전송 - memberId 비교로 변경
    @Transactional
    public void createCommentNotification(Long targetMemberId, Long commenterId, String stockName,
                                          String commenterNickname, Long commentId) {
        // 자기 자신에게는 알림을 보내지 않음
        if (targetMemberId.equals(commenterId)) {
            return;
        }

        Notification notification = Notification.builder()
                .memberId(targetMemberId)
                .type("COMMENT")
                .title("댓글 알림")
                .message(String.format("%s님이 %s 종목에 댓글을 작성했습니다.",
                        commenterNickname, stockName))
                .relatedId(commentId)
                .isRead(false)
                .createdAt(LocalDateTime.now())
                .build();

        notificationRepository.save(notification);

        // 이벤트 발행으로 웹소켓 알림 전송
        eventPublisher.publishEvent(new CommentNotificationEvent(targetMemberId, stockName, commenterNickname));
    }

    // 좋아요 알림 생성 및 전송 - memberId 비교로 변경
    @Transactional
    public void createLikeNotification(Long targetMemberId, Long likerId, String stockName,
                                       String likerNickname, Long commentId) {
        // 자기 자신에게는 알림을 보내지 않음
        if (targetMemberId.equals(likerId)) {
            return;
        }

        Notification notification = Notification.builder()
                .memberId(targetMemberId)
                .type("LIKE")
                .title("좋아요 알림")
                .message(String.format("%s님이 %s 종목의 댓글에 좋아요를 눌렀습니다.",
                        likerNickname, stockName))
                .relatedId(commentId)
                .isRead(false)
                .createdAt(LocalDateTime.now())
                .build();

        notificationRepository.save(notification);

        // 이벤트 발행으로 웹소켓 알림 전송
        eventPublisher.publishEvent(new LikeNotificationEvent(targetMemberId, stockName, likerNickname));
    }

    // 미확인 알림 조회
    @Transactional(readOnly = true)
    public List<Notification> getUnreadNotifications(Long memberId) {
        return notificationRepository.findByMemberIdAndIsReadFalseOrderByCreatedAtDesc(memberId);
    }

    // 알림 읽음 처리
    @Transactional
    public void markAsRead(Long notificationId) {
        notificationRepository.findById(notificationId).ifPresent(notification -> {
            notification.markAsRead();
            notificationRepository.save(notification);
        });
    }

    // 회원의 모든 알림 읽음 처리
    @Transactional
    public void markAllAsRead(Long memberId) {
        List<Notification> unreadNotifications = notificationRepository
                .findByMemberIdAndIsReadFalseOrderByCreatedAtDesc(memberId);

        unreadNotifications.forEach(Notification::markAsRead);
        notificationRepository.saveAll(unreadNotifications);
    }

    public void createTradeFCMNotification(Long memberId, String stockName, String orderType,
                                           Integer quantity, Long price) {
        log.info("FCM 지정가 체결 알림 생성 시작: 회원ID={}, 종목={}, 타입={}, 수량={}, 가격={}",
                memberId, stockName, orderType, quantity, price);

        try {
            // FCM 서비스를 통해 알림 발송 - memberId를 직접 전달
            boolean success = fcmService.sendTradeNotificationToMember(
                    memberId, stockName, orderType, quantity, price);

            if (success) {
                log.info("FCM 지정가 체결 알림 전송 성공: 회원ID={}, 종목={}", memberId, stockName);
            } else {
                log.warn("FCM 지정가 체결 알림 전송 실패: 회원ID={}, 종목={}", memberId, stockName);
            }
        } catch (Exception e) {
            log.error("FCM 지정가 체결 알림 전송 중 오류 발생", e);
        }
    }

}