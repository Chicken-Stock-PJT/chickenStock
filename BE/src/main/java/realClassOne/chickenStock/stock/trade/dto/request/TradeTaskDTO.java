package realClassOne.chickenStock.stock.trade.dto.request;

import lombok.*;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.stock.dto.request.TradeRequestDTO;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TradeTaskDTO {
    private Member member;
    private TradeRequestDTO request;
    private String type; // "BUY" or "SELL"
    private long timestamp; // 주문 접수 시간
    private Long orderId; // 주문 ID (추적용)
    private String status; // 처리 상태

    // 간편 생성자
    public TradeTaskDTO(Member member, TradeRequestDTO request, String type) {
        this.member = member;
        this.request = request;
        this.type = type;
        this.timestamp = System.currentTimeMillis();
    }
}