package realClassOne.chickenStock.stock.dto.response;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class StockInfoResponseDTO {
    private String stockCode;        // 종목코드
    private String stockName;        // 종목명
    private String currentPrice;     // 현재가
    private String priceChange;      // 전일대비
    private String changeRate;       // 등락률
}