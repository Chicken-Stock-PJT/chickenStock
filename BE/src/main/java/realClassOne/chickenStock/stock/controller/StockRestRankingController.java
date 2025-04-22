package realClassOne.chickenStock.stock.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import realClassOne.chickenStock.stock.dto.response.StockRankingResponseDTO;
import realClassOne.chickenStock.stock.service.StockRankingService;

@RestController
@RequestMapping("/api/stock/ranking")
@RequiredArgsConstructor
@Slf4j
public class StockRestRankingController {

    private final StockRankingService stockRankingService;

    /**
     * 거래대금 상위 종목 조회
     *
     * @param marketType        시장구분 (000:전체, 001:코스피, 101:코스닥)
     * @param includeManagement 관리종목포함 여부 (0:미포함, 1:포함)
     * @return 거래대금 상위 종목 정보
     */
    @GetMapping("/tradeAmount")
    public ResponseEntity<StockRankingResponseDTO> getTradeAmountTopList(
            @RequestParam(required = false, defaultValue = "000") String marketType,
            @RequestParam(required = false, defaultValue = "1") String includeManagement) {

        log.info("거래대금 상위 종목 조회 - 시장구분: {}, 관리종목포함: {}", marketType, includeManagement);
        StockRankingResponseDTO response = stockRankingService.getTradeAmountTopList(marketType, includeManagement);
        return ResponseEntity.ok(response);
    }

    /**
     * 전일대비등락률 상위 종목 조회
     *
     * @param marketType 시장구분 (000:전체, 001:코스피, 101:코스닥)
     * @param sortType   정렬구분 (1:상승률, 2:상승폭, 3:하락률, 4:하락폭)
     * @return 전일대비등락률 상위 종목 정보
     */
    @GetMapping("/fluctuationRate")
    public ResponseEntity<StockRankingResponseDTO> getFluctuationRateTopList(
            @RequestParam(required = false, defaultValue = "000") String marketType,
            @RequestParam(required = false, defaultValue = "1") String sortType) {

        log.info("전일대비등락률 상위 종목 조회 - 시장구분: {}, 정렬구분: {}", marketType, sortType);
        StockRankingResponseDTO response = stockRankingService.getFluctuationRateTopList(marketType, sortType);
        return ResponseEntity.ok(response);
    }
    
    /**
     * 당일거래량 상위 종목 목록 조회
     *
     * @param marketType 시장구분 (000:전체, 001:코스피, 101:코스닥)
     * @param sortType 정렬구분 (1:거래량, 2:거래회전율, 3:거래대금)
     * @return StockRankingResponseDTO
     */
    @GetMapping("/volume")
    public ResponseEntity<StockRankingResponseDTO> getTradeVolumeTopList(
            @RequestParam(required = false, defaultValue = "000") String marketType,
            @RequestParam(required = false, defaultValue = "1") String sortType)
     {
        StockRankingResponseDTO response = stockRankingService.getTradeVolumeTopList(marketType, sortType);
        return ResponseEntity.ok(response);
    }
}