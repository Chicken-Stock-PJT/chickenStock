package realClassOne.chickenStock.stock.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import realClassOne.chickenStock.stock.dto.common.PendingOrderDTO;
import realClassOne.chickenStock.stock.dto.request.TradeRequestDTO;
import realClassOne.chickenStock.stock.dto.response.OrderCancelResponseDTO;
import realClassOne.chickenStock.stock.dto.response.TradeResponseDTO;
import realClassOne.chickenStock.stock.service.StockTradeService;

import java.util.List;

@RestController
@RequestMapping("/api/stock/trading")
@RequiredArgsConstructor
@Slf4j
public class StockTradingController {

    private final StockTradeService stockTradeService;

    // 주식 매수 주문 API (* 시장가 또는 지정가로 주문 가능)
    @PostMapping("/buy")
    public ResponseEntity<TradeResponseDTO> buyStock(
            @RequestHeader("Authorization") String authorizationHeader,
            @RequestBody TradeRequestDTO request) {

        // 시장가 주문인 경우 가격 설정 없이 처리
        if (Boolean.TRUE.equals(request.getMarketOrder())) {
            request.setPrice(null);
        }

        TradeResponseDTO response = stockTradeService.buyStock(authorizationHeader, request);
        return ResponseEntity.ok(response);
    }

    /**
     * 주식 매도 주문 API
     * 시장가 또는 지정가로 주문 가능
     */
    @PostMapping("/sell")
    public ResponseEntity<TradeResponseDTO> sellStock(
            @RequestHeader("Authorization") String authorizationHeader,
            @RequestBody TradeRequestDTO request) {

        // 시장가 주문인 경우 가격 설정 없이 처리
        if (Boolean.TRUE.equals(request.getMarketOrder())) {
            request.setPrice(null);
        }

        TradeResponseDTO response = stockTradeService.sellStock(authorizationHeader, request);
        return ResponseEntity.ok(response);
    }

    // 보류 중인 주문 조회 API
    @GetMapping("/pending-orders")
    public ResponseEntity<List<PendingOrderDTO>> getPendingOrders(
            @RequestHeader("Authorization") String authorizationHeader) {

        List<PendingOrderDTO> pendingOrders = stockTradeService.getPendingOrdersByMember(authorizationHeader);
        return ResponseEntity.ok(pendingOrders);
    }

    // 주문 취소 API
    @PostMapping("/cancel-order/{orderId}")
    public ResponseEntity<OrderCancelResponseDTO> cancelOrder(
            @RequestHeader("Authorization") String authorizationHeader,
            @PathVariable Long orderId) {

        boolean result = stockTradeService.cancelPendingOrder(authorizationHeader, orderId);

        if (result) {
            return ResponseEntity.ok(OrderCancelResponseDTO.success());
        } else {
            return ResponseEntity.badRequest().body(OrderCancelResponseDTO.fail());
        }
    }
}