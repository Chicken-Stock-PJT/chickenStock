package realClassOne.chickenStock.notification.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.notification.entity.Notification;
import realClassOne.chickenStock.notification.event.CommentNotificationEvent;
import realClassOne.chickenStock.notification.event.LikeNotificationEvent;
import realClassOne.chickenStock.notification.event.TradeNotificationEvent;
import realClassOne.chickenStock.notification.repository.NotificationRepository;

import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class NotificationService {

    private final NotificationRepository notificationRepository;
    private final ApplicationEventPublisher eventPublisher;

    // 지정가 체결 알림 생성 및 전송
    @Transactional
    public void createTradeNotification(Long memberId, String stockName, String orderType,
                                        Integer quantity, Long price) {
        // DB에 알림 저장
        Notification notification = Notification.builder()
                .memberId(memberId)
                .type("TRADE")
                .title("지정가 체결 알림")
                .message(String.format("%s %s %d주가 %d원에 체결되었습니다.",
                        stockName, orderType.equals("BUY") ? "매수" : "매도", quantity, price))
                .isRead(false)
                .createdAt(LocalDateTime.now())
                .build();

        notificationRepository.save(notification);

        // 이벤트 발행으로 웹소켓 알림 전송
        eventPublisher.publishEvent(new TradeNotificationEvent(memberId, stockName, orderType, quantity, price));
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
}