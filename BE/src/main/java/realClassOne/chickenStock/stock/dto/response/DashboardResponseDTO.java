package realClassOne.chickenStock.stock.dto.response;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Builder
public class DashboardResponseDTO {

    // 회원 자산 정보
    private Long memberMoney;           // 보유 현금
    private Long stockValuation;        // 주식 평가금액 (보유 종목의 현재가 합계)
    private Long pendingOrderAmount;    // 미체결 금액 (매수 대기중인 금액)
    private Long totalAsset;            // 총 자산 (현금 + 주식평가금액)

    // 수익률 정보
    private Long totalInvestment;       // 총 투자금액
    private Long totalProfitLoss;       // 총 손익
    private Double totalReturnRate;     // 총 수익률

    // 당일 수익 정보
    private Long todayProfitLoss;       // 금일 손익
    private Double todayReturnRate;     // 금일 수익률

    // 보유 종목 개요
    private Integer holdingStockCount;  // 보유 종목 개수
    private List<StockHoldingDTO> holdings; // 보유 종목 리스트

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime updatedAt;    // 데이터 업데이트 시간

    @Getter
    @Builder
    public static class StockHoldingDTO {
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