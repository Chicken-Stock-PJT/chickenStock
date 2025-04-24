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
    private String shortCode;     // 단축코드 (6자리)
    private String shortName;     // 한글 종목약명
    private String market;        // 시장구분 (KOSPI, KOSDAQ 등)
    private String stockType;     // 주식종류
    private String faceValue;     // 액면가
}
