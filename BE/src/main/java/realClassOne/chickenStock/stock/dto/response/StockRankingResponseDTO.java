package realClassOne.chickenStock.stock.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.stock.dto.common.StockRankingItemDTO;

import java.io.Serializable;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StockRankingResponseDTO implements Serializable {
    private String rankingType; // 순위 유형
    private List<StockRankingItemDTO> rankingItems; // 순위 항목 목록
    private boolean hasNext; // 추가 데이터 존재 여부
    private String nextKey; // 연속 조회 키
    private int code; // 응답 코드
    private String message; // 응답 메시지
}