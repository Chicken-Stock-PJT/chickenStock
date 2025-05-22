package realClassOne.chickenStock.member.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

/**
 * 자산 비중 응답 DTO
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AssetAllocationResponseDTO {

    private Long totalAsset;                  // 총 자산 (현금 + 주식 평가액)
    private Long cashAmount;                  // 현금 자산
    private Long stockValuation;              // 주식 평가 총액
    private Double cashRatio;                 // 현금 비중 (%)
    private Double stockRatio;                // 주식 비중 (%)
    private List<StockAllocationDTO> stocks;  // 개별 종목 비중 (상세 조회 시)
    private LocalDateTime updatedAt;          // 정보 업데이트 시간

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class StockAllocationDTO {
        private String stockCode;
        private String stockName;
        private Long currentValuation;    // 현재 평가액
        private Double allocationRatio;   // 총 자산 중 차지하는 비중 (%)
        private Integer quantity;         // 보유 수량
        private Long averagePrice;        // 평균 매수가
        private Long currentPrice;        // 현재가
    }
}
