package realClassOne.chickenStock.stock.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.stock.dto.common.StockResponse;
import realClassOne.chickenStock.stock.dto.request.StockSubscriptionRequestDTO;
import realClassOne.chickenStock.stock.dto.response.StockSubscriptionResponseDTO;
import realClassOne.chickenStock.stock.service.StockInfoService;
import realClassOne.chickenStock.stock.service.StockSubscriptionService;

import java.util.List;
import java.util.Set;

@RestController
@RequestMapping(value = "/api/stocks")
@RequiredArgsConstructor
@Slf4j
public class StockController {

    private final StockInfoService stockInfoService;
    private final StockSubscriptionService stockSubscriptionService;

    @GetMapping("/all")
    public ResponseEntity<List<StockResponse>> getAllStocks() {
        log.debug("모든 주식 종목 정보 요청");
        return ResponseEntity.ok().body(stockInfoService.getAllStocks());
    }

    @GetMapping("/code/{code}")
    public ResponseEntity<StockResponse> getStockByCode(@PathVariable String code) {
        log.debug("종목코드로 주식 정보 요청: {}", code);
        StockResponse stock = stockInfoService.getStockByCode(code);
        return ResponseEntity.ok(stock);
    }

    @GetMapping("/name/{name}")
    public ResponseEntity<StockResponse> getStockByName(@PathVariable String name) {
        log.debug("종목명으로 주식 정보 요청: {}", name);
        StockResponse stock = stockInfoService.getStockByName(name);
        return ResponseEntity.ok(stock);
    }

    @PostMapping("/subscribe")
    public ResponseEntity<Void> subscribeStock(@RequestBody StockSubscriptionRequestDTO request) {
        log.info("종목 구독 요청: {}", request.getStockCode());
        stockSubscriptionService.registerStockForSubscription(request.getStockCode());
        return ResponseEntity.ok().build();
    }

    @PostMapping("/unsubscribe")
    public ResponseEntity<Void> unsubscribeStock(@RequestBody StockSubscriptionRequestDTO request) {
        log.info("종목 구독 해제 요청: {}", request.getStockCode());
        stockSubscriptionService.unregisterStockForSubscription(request.getStockCode());
        return ResponseEntity.ok().build();
    }

    @GetMapping("/subscribed")
    public ResponseEntity<Set<String>> getSubscribedStocks() {
        Set<String> subscribedStocks = stockSubscriptionService.getSubscribedStocks();
        return ResponseEntity.ok(subscribedStocks);
    }
}



//    @GetMapping("/register/{stockCode}")
//    public ResponseEntity<String> registerStock(@PathVariable String stockCode) {
//        kiwoomWebSocketClient.registerRealTimeData("0B", List.of(stockCode));
//        kiwoomWebSocketClient.registerRealTimeData("0D", List.of(stockCode));
//        return ResponseEntity.ok("종목 등록 완료: " + stockCode);
//    }
