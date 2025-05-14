package realClassOne.chickenStock.notification.event;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public abstract class NotificationEvent {
    private final Long memberId;
}