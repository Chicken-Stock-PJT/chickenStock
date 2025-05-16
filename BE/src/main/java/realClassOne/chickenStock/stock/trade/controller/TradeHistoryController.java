package realClassOne.chickenStock.stock.trade.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.stock.trade.dto.response.TradeHistoriesCursorResponse;
import realClassOne.chickenStock.stock.trade.service.TradeHistoryService;

@RestController
@RequestMapping("/api/trade-histories")
@RequiredArgsConstructor
public class TradeHistoryController {

    private final TradeHistoryService tradeHistoryService;

    @GetMapping
    public ResponseEntity<TradeHistoriesCursorResponse> getTradeHistories(
            @RequestHeader("Authorization") String authorizationHeader,
            @RequestParam(required = false) String cursor,
            @RequestParam(defaultValue = "10") int size) {

        TradeHistoriesCursorResponse response = tradeHistoryService.getTradeHistories(authorizationHeader, cursor, size);
        return ResponseEntity.ok(response);
    }

    @GetMapping("/{memberId}")
    public ResponseEntity<?> getMemberTradeHistories(
            @PathVariable Long memberId,
            @RequestParam(required = false) String cursor,
            @RequestParam(defaultValue = "10") int size) {

        // memberId 유효성 검증 - 1부터 4까지만 허용
        if (memberId < 1 || memberId > 4) {
            return ResponseEntity.badRequest().build();
        }

        TradeHistoriesCursorResponse response = tradeHistoryService.getMemberTradeHistories(memberId, cursor, size);
        return ResponseEntity.ok(response);
    }
}