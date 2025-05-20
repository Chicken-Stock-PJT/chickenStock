package realClassOne.chickenStock.stock.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.stock.dto.common.PendingOrderDTO;
import realClassOne.chickenStock.stock.dto.request.TradeRequestDTO;
import realClassOne.chickenStock.stock.dto.response.OrderCancelResponseDTO;
import realClassOne.chickenStock.stock.dto.response.TradeResponseDTO;
import realClassOne.chickenStock.stock.service.trade.StockTradeFacadeService;


import java.util.List;

@RestController
@RequestMapping("/api/stock/trading")
@RequiredArgsConstructor
@Slf4j
public class StockTradingController {

    private final StockTradeFacadeService tradeFacadeService;

    @PostMapping("/buy")
    public ResponseEntity<TradeResponseDTO> buyStock(
            @RequestHeader("Authorization") String authorization,
            @RequestBody @Valid TradeRequestDTO request) {
        try {
            TradeResponseDTO response = tradeFacadeService.processBuyOrder(authorization, request);
            return getResponseEntityForTradeResponse(response);
        } catch (Exception e) {
            log.error("매수 처리 중 오류 발생", e);
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(TradeResponseDTO.builder()
                            .status("ERROR")
                            .message("시스템 오류가 발생했습니다.")
                            .build());
        }
    }

    @PostMapping("/sell")
    public ResponseEntity<TradeResponseDTO> sellStock(
            @RequestHeader("Authorization") String authorization,
            @RequestBody @Valid TradeRequestDTO request) {
        try {
            TradeResponseDTO response = tradeFacadeService.processSellOrder(authorization, request);
            return getResponseEntityForTradeResponse(response);
        } catch (Exception e) {
            log.error("매도 처리 중 오류 발생", e);
            return ResponseEntity
                    .status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(TradeResponseDTO.builder()
                            .status("ERROR")
                            .message("시스템 오류가 발생했습니다.")
                            .build());
        }
    }

    @PostMapping("/cancel-order/{orderId}")
    public ResponseEntity<OrderCancelResponseDTO> cancelOrder(
            @RequestHeader("Authorization") String authorizationHeader,
            @PathVariable Long orderId) {
        boolean result = tradeFacadeService.cancelPendingOrder(authorizationHeader, orderId);
        return result ?
                ResponseEntity.ok(OrderCancelResponseDTO.success()) :
                ResponseEntity.badRequest().body(OrderCancelResponseDTO.fail());
    }

    @GetMapping("/pending-orders")
    public ResponseEntity<List<PendingOrderDTO>> getPendingOrders(
            @RequestHeader("Authorization") String authorization) {
        try {
            List<PendingOrderDTO> pendingOrders = tradeFacadeService.getPendingOrdersByMember(authorization);
            return ResponseEntity.ok(pendingOrders);
        } catch (Exception e) {
            log.error("미체결 주문 조회 중 오류 발생", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
        }
    }

    // 응답 상태에 따른 ResponseEntity 생성 헬퍼 메서드
    private ResponseEntity<TradeResponseDTO> getResponseEntityForTradeResponse(TradeResponseDTO response) {
        if ("ERROR".equals(response.getStatus())) {
            return ResponseEntity.badRequest().body(response);
        } else if ("PENDING".equals(response.getStatus())) {
            return ResponseEntity.accepted().body(response);
        } else {
            return ResponseEntity.ok(response);
        }
    }
}