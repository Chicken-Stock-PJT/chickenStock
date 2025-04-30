package realClassOne.chickenStock.stock.trade.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.stock.trade.dto.response.TradeHistoriesResponse;
import realClassOne.chickenStock.stock.trade.service.TradeHistoryService;

@RestController
@RequestMapping("/api/trade-histories")
@RequiredArgsConstructor
public class TradeHistoryController {

    private final TradeHistoryService tradeHistoryService;

    @GetMapping
    public ResponseEntity<TradeHistoriesResponse> getTradeHistories(
            @RequestHeader("Authorization") String authorizationHeader,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size) {

        TradeHistoriesResponse response = tradeHistoryService.getTradeHistories(authorizationHeader, page, size);

        return ResponseEntity.ok(response);
    }
}