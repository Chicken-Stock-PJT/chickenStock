package realClassOne.chickenStock.stock.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StockRankingRequestDTO {
    private String rankingType; // "TRADE_AMOUNT", "FLUCTUATION_RATE", "TRADE_VOLUME"

    // 공통 필드
    private String marketType; // 시장구분(000:전체, 001:코스피, 101:코스닥)
    private String exchangeType; // 거래소구분(1:KRX, 2:NXT, 3:통합)
    private String contYn; // 연속조회여부
    private String nextKey; // 연속조회키

    // 거래대금상위 관련 필드
    private String includeManagement; // 관리종목포함(0:관리종목 미포함, 1:관리종목 포함)

    // 전일대비등락률상위 관련 필드
    private String sortType; // 정렬구분(1:상승률, 2:상승폭, 3:하락률, 4:하락폭)
    private String tradeVolumeCondition; // 거래량조건
    private String stockCondition; // 종목조건
    private String creditCondition; // 신용조건
    private String includeUpDownLimit; // 상하한포함
    private String priceCondition; // 가격조건
    private String tradeAmountCondition; // 거래대금조건

    // 당일거래량상위 관련 필드
    private String creditType; // 신용구분
    private String tradeVolumeType; // 거래량구분
    private String priceType; // 가격구분
    private String tradeAmountType; // 거래대금구분
    private String marketOpenType; // 장운영구분
}
