package realClassOne.chickenStock.member.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * 수익률 응답 DTO
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ReturnRateResponseDTO {

    private Long initialInvestment;   // 초기 투자금
    private Long currentValuation;    // 현재 평가금액
    private Long totalProfitLoss;     // 총 손익
    private Double overallReturnRate; // 전체 수익률 (%)

    // 기간별 수익률 (일/주/월/연간)
    private Map<String, PeriodReturnDTO> periodReturns;
    private LocalDateTime updatedAt;  // 정보 업데이트 시간

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PeriodReturnDTO {
        private String period;        // "daily", "weekly", "monthly", "yearly" 중 하나
        private Double returnRate;    // 해당 기간 수익률 (%)
        private Long profitLoss;      // 해당 기간 손익금액
        private LocalDateTime startDate;  // 기간 시작일
        private LocalDateTime endDate;    // 기간 종료일
    }
}
