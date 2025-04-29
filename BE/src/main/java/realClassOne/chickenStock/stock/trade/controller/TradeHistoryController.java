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
            @RequestHeader("Authorization") String authorizationHeader) {

        TradeHistoriesResponse response = tradeHistoryService.getTradeHistories(authorizationHeader);

        return ResponseEntity.ok(response);
    }
}