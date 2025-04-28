package realClassOne.chickenStock.stock.dto.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ChartDataDTO implements Serializable {
    private String date;             // 일자/시간
    private String currentPrice;     // 현재가
    private String openPrice;        // 시가
    private String highPrice;        // 고가
    private String lowPrice;         // 저가
    private String volume;           // 거래량
    private String tradingValue;     // 거래대금
    private String modifiedRatio;    // 수정비율
    private String previousClosePrice; // 전일종가
}