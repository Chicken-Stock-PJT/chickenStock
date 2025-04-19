package realClassOne.chickenStock.stock.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import realClassOne.chickenStock.stock.dto.common.StockResponse;
import realClassOne.chickenStock.stock.service.KiwoomAuthService;
import realClassOne.chickenStock.stock.service.StockInfoService;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.util.List;

@RestController
@RequestMapping(value = "/api/stock")
@RequiredArgsConstructor
@Slf4j
public class StockController {

    private final KiwoomAuthService authService;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final StockInfoService stockInfoService;

    @GetMapping("/all")
    public ResponseEntity<List<StockResponse>> getAllStocks() {
        log.debug("모든 주식 종목 정보 요청");
        return ResponseEntity.ok().body(stockInfoService.getAllStocks());
    }

    @GetMapping("/code/{code}")
    public ResponseEntity<StockResponse> getStockByCode(@PathVariable String code) {
        log.debug("종목코드로 주식 정보 요청: {}", code);
        StockResponse stock = stockInfoService.getStockByCode(code);
        if (stock != null) {
            return ResponseEntity.ok(stock);
        } else {
            return ResponseEntity.notFound().build();
        }
    }

    @GetMapping("/name/{name}")
    public ResponseEntity<StockResponse> getStockByName(@PathVariable String name) {
        log.debug("종목명으로 주식 정보 요청: {}", name);
        StockResponse stock = stockInfoService.getStockByName(name);
        if (stock != null) {
            return ResponseEntity.ok(stock);
        } else {
            return ResponseEntity.notFound().build();
        }
    }

    @GetMapping("/test-encoding")
    public ResponseEntity<StockResponse> testEncoding() {
        StockResponse testStock = StockResponse.builder()
                .shortCode("000000")
                .shortName("테스트종목")
                .market("KOSPI")
                .stockType("보통주")
                .faceValue("500")
                .build();
        return ResponseEntity.ok(testStock);
    }
}





//    @GetMapping("/register/{stockCode}")
//    public ResponseEntity<String> registerStock(@PathVariable String stockCode) {
//        kiwoomWebSocketClient.registerRealTimeData("0B", List.of(stockCode));
//        kiwoomWebSocketClient.registerRealTimeData("0D", List.of(stockCode));
//        return ResponseEntity.ok("종목 등록 완료: " + stockCode);
//    }
