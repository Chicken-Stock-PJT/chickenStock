package realClassOne.chickenStock.stock.event;

import lombok.AllArgsConstructor;
import lombok.Data;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.stock.dto.request.TradeRequestDTO;
import realClassOne.chickenStock.stock.trade.dto.request.TradeTaskDTO;

@Data
@AllArgsConstructor
public class TradeEvent {
    public enum EventType {  // private에서 public으로 변경
        ORDER_RECEIVED,
        ORDER_PROCESSING,
        ORDER_EXECUTED,
        ORDER_FAILED,
        PRICE_UPDATED
    }

    private EventType type;
    private Long orderId;
    private String stockCode;
    private Long price;
    private Member member;
    private TradeRequestDTO request;
    private String message;
    private long timestamp;

    // 정적 팩토리 메서드들 (변경 없음)
    public static TradeEvent orderReceived(TradeTaskDTO task) {
        return new TradeEvent(
                EventType.ORDER_RECEIVED,
                task.getOrderId(),
                task.getRequest().getStockCode(),
                null,
                task.getMember(),
                task.getRequest(),
                "주문이 접수되었습니다",
                System.currentTimeMillis()
        );
    }

    public static TradeEvent orderProcessing(TradeTaskDTO task) {
        return new TradeEvent(
                EventType.ORDER_PROCESSING,
                task.getOrderId(),
                task.getRequest().getStockCode(),
                null,
                task.getMember(),
                task.getRequest(),
                "주문을 처리 중입니다",
                System.currentTimeMillis()
        );
    }

    public static TradeEvent orderExecuted(TradeTaskDTO task, Long executionPrice) {
        return new TradeEvent(
                EventType.ORDER_EXECUTED,
                task.getOrderId(),
                task.getRequest().getStockCode(),
                executionPrice,
                task.getMember(),
                task.getRequest(),
                "주문이 체결되었습니다",
                System.currentTimeMillis()
        );
    }

    public static TradeEvent orderFailed(TradeTaskDTO task, String errorMessage) {
        return new TradeEvent(
                EventType.ORDER_FAILED,
                task.getOrderId(),
                task.getRequest().getStockCode(),
                null,
                task.getMember(),
                task.getRequest(),
                errorMessage,
                System.currentTimeMillis()
        );
    }

    public static TradeEvent priceUpdated(String stockCode, Long price) {
        return new TradeEvent(
                EventType.PRICE_UPDATED,
                null,
                stockCode,
                price,
                null,
                null,
                "가격이 업데이트되었습니다",
                System.currentTimeMillis()
        );
    }
}