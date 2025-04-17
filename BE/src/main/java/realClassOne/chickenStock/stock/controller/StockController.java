package realClassOne.chickenStock.stock.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import realClassOne.chickenStock.stock.service.KiwoomAuthService;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.util.List;

@RestController
@RequestMapping("/api/stock")
@RequiredArgsConstructor
@Slf4j
public class StockController {

    private final KiwoomAuthService authService;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;

    @GetMapping("/token")
    public ResponseEntity<String> getToken() {
        return ResponseEntity.ok("토큰 발급 성공: " + authService.getAccessToken());
    }

    @GetMapping("/register/{stockCode}")
    public ResponseEntity<String> registerStock(@PathVariable String stockCode) {
        kiwoomWebSocketClient.registerRealTimeData("0B", List.of(stockCode));
        kiwoomWebSocketClient.registerRealTimeData("0D", List.of(stockCode));
        return ResponseEntity.ok("종목 등록 완료: " + stockCode);
    }
}