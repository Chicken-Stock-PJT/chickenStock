package realClassOne.chickenStock.stock.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ChartRequestDTO implements Serializable {
    private String chartType;        // DAILY, WEEKLY, MONTHLY, YEARLY, MINUTE
    private String stockCode;        // 종목코드
    private String baseDate;         // 기준일자 YYYYMMDD
    private String timeInterval;     // 분봉 시간간격 (MINUTE 타입일 때만 사용)
    private String modifiedPriceType; // 수정주가구분 (기본값: 1)
    private String contYn;           // 연속조회여부 (Y/N)
    private String nextKey;          // 연속조회키

    public void setContYn(String contYn) {
        this.contYn = contYn;
    }

    public void setNextKey(String nextKey) {
        this.nextKey = nextKey;
    }
}