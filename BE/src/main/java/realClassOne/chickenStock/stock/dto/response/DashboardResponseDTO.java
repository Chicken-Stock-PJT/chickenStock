package realClassOne.chickenStock.stock.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Getter
@Builder
@NoArgsConstructor  // 기본 생성자 추가
@AllArgsConstructor // 모든 필드 생성자 추가
public class DashboardResponseDTO implements Serializable {  // Serializable 구현

    private Long memberMoney;      // 예수금 잔고 (순 현금성 자산)
    private Long cash;             // 현금 (순 현금성 자산 + 지정가 매수 대기중인 금액)
    private Long stockValuation;   // 보유 주식 평가금액
    private Long totalValuationAmount;  // 총 평가금액 (보유 주식 + 지정가 매도 대기)
    private Long pendingOrderAmount;  // 지정가 매수 대기중인 금액
    private Long pendingSellAmount;   // 지정가 매도 대기중인 금액
    private Long totalAsset;       // 총 자산
    private Long totalInvestment;  // 총 투자금액
    private Long totalProfitLoss;  // 총 손익
    private Double totalReturnRate;  // 총 수익률
    private Long todayProfitLoss;  // 금일 수익
    private Double todayReturnRate;  // 금일 수익률
    private Long todayTradeAmount; // 금일 매매 규모
    private Integer holdingStockCount;  // 보유 종목 수
    private List<StockHoldingDTO> holdings;  // 보유 종목 상세 정보
    private String updatedAt;      // 업데이트 시간

    @Getter
    @Builder
    @NoArgsConstructor  // 기본 생성자 추가
    @AllArgsConstructor // 모든 필드 생성자 추가
    public static class StockHoldingDTO implements Serializable {  // Serializable 구현
        private String stockCode;       // 종목 코드
        private String stockName;       // 종목명
        private Integer quantity;       // 보유수량
        private Long averagePrice;      // 평균단가
        private Long currentPrice;      // 현재가 (실시간)
        private Long valuationAmount;   // 평가금액 (현재가 × 보유수량)
        private Long profitLoss;        // 손익 (평가금액 - 투자금액)
        private Double returnRate;      // 수익률

        // 추가: 전일 대비 정보
        private String priceChange;     // 전일대비
        private String changeRate;      // 등락률
    }
}