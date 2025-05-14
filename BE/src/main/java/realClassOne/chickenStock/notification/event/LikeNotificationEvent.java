package realClassOne.chickenStock.notification.event;

import lombok.Getter;

// 좋아요 알림 이벤트
@Getter
public class LikeNotificationEvent extends NotificationEvent {
    private final String stockName;
    private final String likerNickname;

    public LikeNotificationEvent(Long targetMemberId, String stockName,
                                 String likerNickname) {
        super(targetMemberId);
        this.stockName = stockName;
        this.likerNickname = likerNickname;
    }
}
