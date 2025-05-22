package realClassOne.chickenStock.notification.event;

import lombok.Getter;

// 지정가 체결 이벤트
@Getter
public class TradeNotificationEvent extends NotificationEvent {
    private final String stockName;
    private final String orderType;
    private final Integer quantity;
    private final Long price;

    public TradeNotificationEvent(Long memberId, String stockName, String orderType,
                                  Integer quantity, Long price) {
        super(memberId);
        this.stockName = stockName;
        this.orderType = orderType;
        this.quantity = quantity;
        this.price = price;
    }
}
