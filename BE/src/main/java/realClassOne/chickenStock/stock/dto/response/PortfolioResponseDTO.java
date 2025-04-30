package realClassOne.chickenStock.stock.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

/**
 * 회원 보유 주식 포트폴리오 응답 DTO
 */
@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PortfolioResponseDTO {
    private Long memberMoney;  // 예치금(현금) 잔고 (7번)
    private Long totalAsset;   // 총 자산 금액 (8번)
    private Long totalInvestment;  // 총 투자 금액
    private Long totalValuation;   // 총 평가 금액
    private Long totalProfitLoss;  // 총 누적 손익 (8번)
    private Double totalReturnRate;  // 총 수익률
    private List<StockPositionDTO> positions;  // 보유 주식 정보 목록
    private LocalDateTime updatedAt;  // 최종 업데이트 시간

    /**
     * 보유 주식 종목별 정보 DTO
     */
    @Getter
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class StockPositionDTO {
        private String stockCode;     // 종목 코드 (1번)
        private String stockName;     // 종목 명 (1번)
        private Integer quantity;     // 보유 수량 (2번)
        private Long averagePrice;    // 평균 매입가 (3번)
        private Long currentPrice;    // 현재가 (4번)
        private Long valuationAmount;  // 평가 금액
        private Long profitLoss;      // 평가 손익 (5번)
        private Double returnRate;    // 수익률 (6번)
    }
}

