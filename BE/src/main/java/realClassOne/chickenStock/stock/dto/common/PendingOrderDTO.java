package realClassOne.chickenStock.stock.dto.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.stock.entity.PendingOrder;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PendingOrderDTO {
    private Long orderId;
    private Long memberId;
    private String stockCode;
    private String stockName;
    private String orderType;  // BUY, SELL
    private Integer quantity;
    private Long targetPrice;
    private Long reservedFee;     // 매수 주문시 예약된 수수료
    private Long expectedFee;     // 매도 주문시 예상 수수료
    private Long expectedTax;     // 매도 주문시 예상 세금
    private LocalDateTime createdAt;
    private String status;

    public static PendingOrderDTO fromEntity(PendingOrder order) {
        return PendingOrderDTO.builder()
                .orderId(order.getOrderId())
                .memberId(order.getMember().getMemberId())
                .stockCode(order.getStockData().getShortCode())
                .stockName(order.getStockData().getShortName())
                .orderType(order.getOrderType().name())
                .quantity(order.getQuantity())
                .targetPrice(order.getTargetPrice())
                .reservedFee(order.getReservedFee())
                .expectedFee(order.getExpectedFee())
                .expectedTax(order.getExpectedTax())
                .createdAt(order.getCreatedAt())
                .status(order.getStatus().name())
                .build();
    }
}