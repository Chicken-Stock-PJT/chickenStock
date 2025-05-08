package realClassOne.chickenStock.stock.scheduler;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.stock.service.StockRankingService;

@Component
@Slf4j
@RequiredArgsConstructor
public class RankingScheduler {

    private final StockRankingService stockRankingService;

    // 기본 요청 파라미터 - 주요 조회 케이스에 대한 기본값
    private static final String DEFAULT_MARKET_TYPE = "000"; // 전체 시장
    private static final String DEFAULT_INCLUDE_MANAGEMENT = "1"; // 관리종목 포함
    private static final String DEFAULT_SORT_TYPE = "1"; // 기본 정렬 타입

    /**
     * 10초마다 거래대금 상위 종목 데이터를 업데이트합니다.
     */
    @Scheduled(fixedRate = 10000) // 10초마다 실행
    public void updateTradeAmountRanking() {
        try {
            log.info("거래대금 상위 데이터 업데이트 시작");
            // 전체 시장, 관리종목 포함
            stockRankingService.updateTradeAmountRankingForParams(DEFAULT_MARKET_TYPE, DEFAULT_INCLUDE_MANAGEMENT);
            // 코스피, 관리종목 포함
            stockRankingService.updateTradeAmountRankingForParams("001", DEFAULT_INCLUDE_MANAGEMENT);
            // 코스닥, 관리종목 포함
            stockRankingService.updateTradeAmountRankingForParams("101", DEFAULT_INCLUDE_MANAGEMENT);
            log.info("거래대금 상위 데이터 업데이트 완료");
        } catch (Exception e) {
            log.error("거래대금 상위 데이터 업데이트 실패", e);
        }
    }

    /**
     * 전일대비등락률 상위 종목 데이터를 업데이트합니다.
     * 거래대금과 간격을 두어 API 호출 부하 분산
     */
    @Scheduled(fixedRate = 10000, initialDelay = 3000) // 10초마다 실행, 초기 지연 3초
    public void updateFluctuationRateRanking() {
        try {
            log.info("전일대비등락률 상위 데이터 업데이트 시작");
            // 전체 시장, 상승률
            stockRankingService.updateFluctuationRateRankingForParams(DEFAULT_MARKET_TYPE, "1");
            // 전체 시장, 하락률
            stockRankingService.updateFluctuationRateRankingForParams(DEFAULT_MARKET_TYPE, "3");
            // 코스피, 상승률
            stockRankingService.updateFluctuationRateRankingForParams("001", "1");
            // 코스닥, 상승률
            stockRankingService.updateFluctuationRateRankingForParams("101", "1");
            log.info("전일대비등락률 상위 데이터 업데이트 완료");
        } catch (Exception e) {
            log.error("전일대비등락률 상위 데이터 업데이트 실패", e);
        }
    }

    /**
     * 당일거래량 상위 종목 데이터를 업데이트합니다.
     * 다른 API 호출과 간격을 두어 부하 분산
     */
    @Scheduled(fixedRate = 10000, initialDelay = 6000) // 10초마다 실행, 초기 지연 6초
    public void updateTradeVolumeRanking() {
        try {
            log.info("당일거래량 상위 데이터 업데이트 시작");
            // 전체 시장, 거래량
            stockRankingService.updateTradeVolumeRankingForParams(DEFAULT_MARKET_TYPE, "1");
            // 코스피, 거래량
            stockRankingService.updateTradeVolumeRankingForParams("001", "1");
            // 코스닥, 거래량
            stockRankingService.updateTradeVolumeRankingForParams("101", "1");
            log.info("당일거래량 상위 데이터 업데이트 완료");
        } catch (Exception e) {
            log.error("당일거래량 상위 데이터 업데이트 실패", e);
        }
    }
}