package realClassOne.chickenStock.stock.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.stock.dto.request.ChartRequestDTO;
import realClassOne.chickenStock.stock.dto.response.ChartResponseDTO;
import realClassOne.chickenStock.stock.service.StockChartService;

@RestController
@RequestMapping("/api/stock/chart")
@RequiredArgsConstructor
@Slf4j
public class StockRestController {

    private final StockChartService stockChartService;

    @PostMapping
    public ResponseEntity<ChartResponseDTO> getStockChart(@RequestBody ChartRequestDTO request) {
        log.info("차트 조회 요청: {}", request);
        ChartResponseDTO response = stockChartService.getStockChart(request);
        return ResponseEntity.ok(response);
    }

    @GetMapping("/{chartType}/{stockCode}")
    public ResponseEntity<ChartResponseDTO> getStockChartByType(
            @PathVariable String chartType,
            @PathVariable String stockCode,
            @RequestParam(required = false) String baseDate,
            @RequestParam(required = false) String timeInterval,
            @RequestParam(required = false) String modifiedPriceType,
            @RequestParam(required = false) String contYn,
            @RequestParam(required = false) String nextKey) {

        log.info("차트 조회 요청 - 타입: {}, 종목코드: {}", chartType, stockCode);

        ChartResponseDTO response = stockChartService.getStockChartByType(
                chartType, stockCode, baseDate, timeInterval, modifiedPriceType, contYn, nextKey);

        return ResponseEntity.ok(response);
    }

    @GetMapping("/all/{stockCode}")
    public ResponseEntity<ChartResponseDTO> getAllChartData(
            @PathVariable String stockCode,
            @RequestParam(required = false) String baseDate,
            @RequestParam(required = false, defaultValue = "DAILY") String chartType,
            @RequestParam(required = false) String modifiedPriceType) {

        log.info("모든 차트 데이터 조회 요청 - 종목코드: {}, 차트타입: {}", stockCode, chartType);

        ChartResponseDTO response = stockChartService.getAllChartData(
                stockCode, baseDate, chartType, modifiedPriceType);

        return ResponseEntity.ok(response);
    }
}