package realClassOne.chickenStock.stock.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.stock.dto.response.AvailableQuantityResponseDTO;
import realClassOne.chickenStock.stock.dto.common.StockResponse;
import realClassOne.chickenStock.stock.dto.response.StockAskBidResponseDTO;
import realClassOne.chickenStock.stock.dto.response.StockInfoResponseDTO;
import realClassOne.chickenStock.stock.service.KiwoomStockApiService;
import realClassOne.chickenStock.stock.service.StockInfoService;
import realClassOne.chickenStock.stock.service.StockTradeService;

import java.util.List;

@RestController
@RequestMapping(value = "/api/stocks")
@RequiredArgsConstructor
@Slf4j
public class StockController {

    private final StockInfoService stockInfoService;
    private final KiwoomStockApiService kiwoomStockApiService;
    private final StockTradeService stockTradeService;

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

    // 주식 종목코드로 현재가 및 기본정보를 조회합니다.
    @GetMapping("/info/{stockCode}")
    public ResponseEntity<StockInfoResponseDTO> getStockInfo(@PathVariable String stockCode) {
        StockInfoResponseDTO response = kiwoomStockApiService.getStockInfo(stockCode);
        return ResponseEntity.ok(response);
    }

    // 특정 멤버가 가지고 있는 특정 종목의 주식 수 조회
    @GetMapping("/{shortCode}/available-quantity")
    public ResponseEntity<AvailableQuantityResponseDTO> getAvailableSellQuantity(
            @PathVariable String shortCode,
            @RequestHeader("Authorization") String authorizationHeader) {

        int availableQty = stockTradeService.getAvailableSellQuantity(authorizationHeader, shortCode);
        return ResponseEntity.ok(new AvailableQuantityResponseDTO(shortCode, availableQty));
    }
}
