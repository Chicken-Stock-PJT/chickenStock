package realClassOne.chickenStock.stock.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.stock.dto.common.ChartDataDTO;
import realClassOne.chickenStock.stock.dto.common.StockRankingItemDTO;

import java.io.Serializable;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ChartResponseDTO implements Serializable {
    private String stockCode;        // 종목코드
    private String chartType;        // 차트 타입 (DAILY, WEEKLY, MONTHLY, YEARLY, MINUTE)
    private List<ChartDataDTO> chartData; // 차트 데이터 목록
    private boolean hasNext;         // 연속조회 여부
    private String nextKey;          // 연속조회키
    private int code;                // 응답 코드
    private String message;          // 응답 메시지
}