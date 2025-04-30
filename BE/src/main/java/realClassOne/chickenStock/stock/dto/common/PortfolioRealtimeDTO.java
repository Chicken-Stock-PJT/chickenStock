package realClassOne.chickenStock.stock.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PortfolioRealtimeDTO {
    private Long totalAsset;   // 총 자산 금액
    private Long totalProfitLoss;  // 총 누적 손익
    private Double totalReturnRate;  // 총 수익률
    private List<StockRealtimeDTO> positions;  // 실시간 주식 정보 목록

    @Getter
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class StockRealtimeDTO {
        private String stockCode;     // 종목 코드
        private Long currentPrice;    // 현재가
        private Long valuationAmount; // 평가 금액
        private Long profitLoss;      // 평가 손익
        private Double returnRate;    // 수익률
    }
}