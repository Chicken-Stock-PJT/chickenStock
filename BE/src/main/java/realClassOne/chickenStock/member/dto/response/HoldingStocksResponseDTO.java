package realClassOne.chickenStock.member.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class HoldingStocksResponseDTO {

    private Long cashAmount;  // 현금성 자산 (MEMBER_MONEY)

    private List<HoldingStockDTO> holdingStocks;  // 보유 주식 목록

    private LocalDateTime updatedAt;

    private String message;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class HoldingStockDTO {
        private String stockCode;       // 단축코드 (SHORT_CODE)
        private String stockName;       // 종목명
        private Long quantity;          // 보유 수량
        private Long averagePrice;      // 평균 구매가
    }
}