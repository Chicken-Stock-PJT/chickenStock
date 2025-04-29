package realClassOne.chickenStock.stock.dto.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.entity.TradeHistory;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PendingOrderDTO {
    private Long orderId;
    private String stockCode;
    private String stockName;
    private String orderType;  // BUY, SELL
    private Integer quantity;
    private Long targetPrice;
    private LocalDateTime createdAt;
    private String status;

    public static PendingOrderDTO fromPendingOrder(PendingOrder order) {
        return PendingOrderDTO.builder()
                .orderId(order.getOrderId())
                .stockCode(order.getStockData().getShortCode())
                .stockName(order.getStockData().getShortName())
                .orderType(order.getOrderType().toString())
                .quantity(order.getQuantity())
                .targetPrice(order.getTargetPrice())
                .createdAt(order.getCreatedAt())
                .status(order.getStatus().toString())
                .build();
    }
}