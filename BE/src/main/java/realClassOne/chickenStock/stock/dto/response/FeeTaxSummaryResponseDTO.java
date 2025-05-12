package realClassOne.chickenStock.stock.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.stock.entity.FeeTaxSummary;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FeeTaxSummaryResponseDTO {
    private Long totalBuyFee;     // 총 매수 수수료
    private Long totalSellFee;    // 총 매도 수수료
    private Long totalSellTax;    // 총 매도 세금
    private Long totalAmount;     // 전체 합계
    private LocalDateTime lastUpdatedAt;  // 마지막 업데이트 시간

    public static FeeTaxSummaryResponseDTO fromEntity(FeeTaxSummary entity) {
        return FeeTaxSummaryResponseDTO.builder()
                .totalBuyFee(entity.getTotalBuyFee())
                .totalSellFee(entity.getTotalSellFee())
                .totalSellTax(entity.getTotalSellTax())
                .totalAmount(entity.getTotalAmount())
                .lastUpdatedAt(entity.getLastUpdatedAt())
                .build();
    }
}