package realClassOne.chickenStock.stock.trade.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@AllArgsConstructor
public class TradeHistoryDTO {
    private String stockName;    // 주식 종목명
    private String tradeType;    // BUY or SELL
    private int quantity;        // 수량
    private long unitPrice;      // 거래 단가
    private LocalDateTime createdAt;  // 주문 시간
    private LocalDateTime tradedAt;   // 체결 시간
}