package realClassOne.chickenStock.stock.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import realClassOne.chickenStock.stock.dto.common.StockResponse;
import realClassOne.chickenStock.stock.dto.response.StockAskBidResponseDTO;
import realClassOne.chickenStock.stock.service.KiwoomStockApiService;
import realClassOne.chickenStock.stock.service.StockInfoService;

import java.util.List;

@RestController
@RequestMapping(value = "/api/stocks")
@RequiredArgsConstructor
@Slf4j
public class StockController {

    private final StockInfoService stockInfoService;
    private final KiwoomStockApiService kiwoomStockApiService;

    // 모든 주식 정보 요청 API
    @GetMapping("/all")
    public ResponseEntity<List<StockResponse>> getAllStocks() {
        return ResponseEntity.ok().body(stockInfoService.getAllStocks());
    }

    // 주식 코드로 실시간 주식 정보 요청
    @GetMapping("/code/{code}")
    public ResponseEntity<StockResponse> getStockByCode(@PathVariable String code) {
        StockResponse stock = kiwoomStockApiService.getEnhancedStockByCode(code);
        return ResponseEntity.ok(stock);
    }

    // 주식 코드로 호가 정보 요청
    @GetMapping("/askbid/{code}")
    public ResponseEntity<StockAskBidResponseDTO> getStockAskBid(@PathVariable String code) {
        log.info("종목코드 {}의 호가 정보 요청", code);
        StockAskBidResponseDTO askBidInfo = kiwoomStockApiService.getStockAskBidInfo(code);
        return ResponseEntity.ok(askBidInfo);
    }
}
