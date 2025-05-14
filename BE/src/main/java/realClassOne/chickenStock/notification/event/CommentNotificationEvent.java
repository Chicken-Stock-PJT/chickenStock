package realClassOne.chickenStock.notification.event;

import lombok.Getter;

// 댓글 알림 이벤트
@Getter
public class CommentNotificationEvent extends NotificationEvent {
    private final String stockName;
    private final String commenterNickname;

    public CommentNotificationEvent(Long targetMemberId, String stockName,
                                    String commenterNickname) {
        super(targetMemberId);
        this.stockName = stockName;
        this.commenterNickname = commenterNickname;
    }
}
