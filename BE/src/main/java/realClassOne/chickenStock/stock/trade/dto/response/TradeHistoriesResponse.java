package realClassOne.chickenStock.stock.trade.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter
@AllArgsConstructor
public class TradeHistoriesResponse {
    private List<TradeHistoryDTO> tradeHistories; // 거래내역 리스트
    private long realizedProfit;                  // 총 실현 손익
}