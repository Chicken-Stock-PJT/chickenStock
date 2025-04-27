package realClassOne.chickenStock.member.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

/**
 * 관심종목 응답 DTO
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WatchListResponseDTO {

    private String message;
    private List<WatchListItemDTO> watchList;
    private LocalDateTime updatedAt;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class WatchListItemDTO {
        private String stockCode;
        private String stockName;
        private Long currentPrice;
        private String priceChange;       // 전일대비 변동금액 (예: "+1,000", "-500")
        private String changeRate;        // 전일대비 변동률 (예: "+1.5%", "-0.8%")
        private String tradingVolume;     // 거래량
        private LocalDateTime timestamp;  // 가격 정보 업데이트 시간
    }
}
