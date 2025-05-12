package realClassOne.chickenStock.stock.dto.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.stock.entity.TradeHistory;

import java.time.LocalDateTime;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TradeResponseDTO {
    private Long tradeHistoryId;    // 거래 내역 ID
    private Long orderId;           // 보류 주문 ID
    private String stockCode;       // 종목 코드
    private String stockName;       // 종목명
    private String tradeType;       // 거래 유형 (BUY, SELL)
    private Integer quantity;       // 거래 수량
    private Long unitPrice;         // 단가
    private Long totalPrice;        // 총 거래 금액
    private Long fee;               // 수수료
    private Long tax;               // 세금
    private LocalDateTime tradedAt; // 거래 시간
    private String status;          // 거래 상태 (COMPLETED, PENDING)
    private String message;         // 응답 메시지

    public static TradeResponseDTO fromTradeHistory(TradeHistory history) {
        return TradeResponseDTO.builder()
                .tradeHistoryId(history.getTradeHistoryId())
                .stockCode(history.getStockData().getShortCode())
                .stockName(history.getStockData().getShortName())
                .tradeType(history.getTradeType().toString())
                .quantity(history.getQuantity())
                .unitPrice(history.getUnitPrice())
                .totalPrice(history.getTotalPrice())
                .fee(history.getFee())
                .tax(history.getTax())
                .tradedAt(history.getTradedAt())
                .status("COMPLETED")
                .build();
    }
}