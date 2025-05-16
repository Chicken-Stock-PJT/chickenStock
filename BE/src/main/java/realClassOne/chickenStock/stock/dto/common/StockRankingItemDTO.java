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
public class StockRankingItemDTO implements Serializable {
    // 공통 필드
    private String stockCode; // 종목코드
    private String stockName; // 종목명
    private String currentPrice; // 현재가
    private String previousDayCompareSign; // 전일대비기호
    private String previousDayCompare; // 전일대비
    private String fluctuationRate; // 등락률

    // 거래대금상위 관련 필드
    private String currentRank; // 현재순위
    private String previousRank; // 전일순위
    private String currentTradeVolume; // 현재거래량
    private String previousTradeVolume; // 전일거래량
    private String tradeAmount; // 거래대금

    // 전일대비등락률상위 관련 필드
    private String sellRemaining; // 매도잔량
    private String buyRemaining; // 매수잔량
    private String contractStrength; // 체결강도

    // 당일거래량상위 관련 필드
    private String tradeVolume; // 거래량
    private String previousRatio; // 전일비
    private String tradeTurnoverRate; // 거래회전율

    // StockRankingItemDTO.java에 추가할 setter 메서드
    public void setStockCode(String stockCode) {
        this.stockCode = stockCode;
    }
}
