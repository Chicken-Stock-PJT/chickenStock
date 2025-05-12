package realClassOne.chickenStock.stock.dto.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StockResponse {
    private String shortCode;
    private String shortName;
    private String market;
    private String stockType;
    private String faceValue; // 액면가
    private String prevDayCompare; // 전일대비
    private String fluctuationRate; // 등락률
}