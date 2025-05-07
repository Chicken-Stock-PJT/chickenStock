package realClassOne.chickenStock.stock.trade.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter
@AllArgsConstructor
public class TradeHistoriesCursorResponse {
    private List<TradeHistoryDTO> tradeHistories;
    private long realizedProfit;
    private boolean hasNext;         // 다음 페이지 존재 여부
    private String nextCursor;       // 다음 요청용 기준 시각 (createdAt)
}