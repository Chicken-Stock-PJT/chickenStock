package realClassOne.chickenStock.stock.dto.request;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import realClassOne.chickenStock.member.entity.Member;

@Getter
@Setter
@AllArgsConstructor
public class TradeTaskDTO {
    private Member member;
    private String type; // "BUY" 또는 "SELL"
    private TradeRequestDTO request;
}