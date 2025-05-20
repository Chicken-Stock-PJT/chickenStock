package realClassOne.chickenStock.stock.trade.dto.common;

import lombok.AllArgsConstructor;
import lombok.Builder;  // 이 import 문이 필요합니다
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TradeStatusDTO {
    private Long orderId;           // 주문 ID
    private String stockCode;       // 종목 코드
    private String stockName;       // 종목명
    private String tradeType;       // 거래 유형 (BUY, SELL)
    private String status;          // 주문 상태
    private Long timestamp;         // 처리 시간
    private Long executionPrice;    // 체결 가격
    private LocalDateTime executedAt; // 체결 시간
    private String message;         // 상태 메시지
}