package realClassOne.chickenStock.stock.event;

import lombok.Getter;
import realClassOne.chickenStock.stock.entity.PendingOrder;

@Getter
public class OrderExecutionEvent {
    private final PendingOrder order;
    private final Long currentPrice;

    public OrderExecutionEvent(PendingOrder order, Long currentPrice) {
        this.order = order;
        this.currentPrice = currentPrice;
    }
}