package realClassOne.chickenStock.member.dto.response;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@Builder
public class StockTradeHistoryResponseDTO {
    private String stockCode;
    private String stockName;
    private List<TradeHistoryDTO> tradeHistories;
    private Long currentQuantity;
    private Long averagePrice;
    private Long currentPrice;
    private Double returnRate;
    private String message;

    @Getter
    @Setter
    @Builder
    public static class TradeHistoryDTO {
        private Long tradeId;
        private String tradeType; // BUY, SELL
        private Integer quantity;
        private Long unitPrice;
        private Long totalPrice;
        private LocalDateTime tradedAt;
    }
}