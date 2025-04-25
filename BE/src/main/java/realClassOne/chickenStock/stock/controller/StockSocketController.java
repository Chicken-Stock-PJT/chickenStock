package realClassOne.chickenStock.stock.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.stock.dto.request.StockSubscriptionRequestDTO;
import realClassOne.chickenStock.stock.service.StockSubscriptionService;

import java.util.Set;

@RestController
@RequestMapping(value = "/api/stocks")
@RequiredArgsConstructor
@Slf4j
public class StockSocketController {

    private final StockSubscriptionService stockSubscriptionService;

    // 종목 구독요청
    @PostMapping("/subscribe")
    public ResponseEntity<Void> subscribeStock(@RequestBody StockSubscriptionRequestDTO request) {
        stockSubscriptionService.registerStockForSubscription(request.getStockCode());
        return ResponseEntity.ok().build();
    }

    // 종목 구독 해제 요청
    @PostMapping("/unsubscribe")
    public ResponseEntity<Void> unsubscribeStock(@RequestBody StockSubscriptionRequestDTO request) {
        log.info("종목 구독 해제 요청: {}", request.getStockCode());
        stockSubscriptionService.unregisterStockForSubscription(request.getStockCode());
        return ResponseEntity.ok().build();
    }

    // 종목 구독된 항목 조회
    @GetMapping("/subscribed")
    public ResponseEntity<Set<String>> getSubscribedStocks() {
        Set<String> subscribedStocks = stockSubscriptionService.getSubscribedStocks();
        return ResponseEntity.ok(subscribedStocks);
    }

//    @PostMapping("/ai-price")
//    public ResponseEntity<StockPriceResponseDTO> getQuickPrices(@RequestBody StockPriceRequestDTO requestDTO) {
//        log.info("주식 가격 조회 요청: {} 종목", requestDTO.getStockCodes() != null ? requestDTO.getStockCodes().size() : 0);
//        StockPriceResponseDTO response = stockPriceService.getQuickStockPrices(requestDTO);
//        return ResponseEntity.ok(response);
//    }
}