package realClassOne.chickenStock.stock.controller;


import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import realClassOne.chickenStock.stock.dto.common.PendingOrderDTO;
import realClassOne.chickenStock.stock.dto.request.TradeRequestDTO;
import realClassOne.chickenStock.stock.dto.response.TradeResponseDTO;
import realClassOne.chickenStock.stock.service.StockTradeService;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/trading")
@RequiredArgsConstructor
@Slf4j
public class StockTradingController {

    private final StockTradeService stockTradeService;

    /**
     * 주식 매수 주문 API
     * 시장가 또는 지정가로 주문 가능
     */
    @PostMapping("/buy")
    public ResponseEntity<TradeResponseDTO> buyStock(
            @RequestHeader("Member-Id") Long memberId,
            @RequestBody TradeRequestDTO request) {

        log.info("매수 주문 요청 - 회원: {}, 종목: {}, 수량: {}, 시장가여부: {}",
                memberId, request.getStockCode(), request.getQuantity(), request.getMarketOrder());

        // 시장가 주문인 경우 가격 설정 없이 처리
        if (Boolean.TRUE.equals(request.getMarketOrder())) {
            request.setPrice(null);
        }

        TradeResponseDTO response = stockTradeService.buyStock(memberId, request);
        return ResponseEntity.ok(response);
    }

    /**
     * 주식 매도 주문 API
     * 시장가 또는 지정가로 주문 가능
     */
    @PostMapping("/sell")
    public ResponseEntity<TradeResponseDTO> sellStock(
            @RequestHeader("Member-Id") Long memberId,
            @RequestBody TradeRequestDTO request) {

        log.info("매도 주문 요청 - 회원: {}, 종목: {}, 수량: {}, 시장가여부: {}",
                memberId, request.getStockCode(), request.getQuantity(), request.getMarketOrder());

        // 시장가 주문인 경우 가격 설정 없이 처리
        if (Boolean.TRUE.equals(request.getMarketOrder())) {
            request.setPrice(null);
        }

        TradeResponseDTO response = stockTradeService.sellStock(memberId, request);
        return ResponseEntity.ok(response);
    }

    /**
     * 회원 기본금 초기화 API (3억)
     */
    @PostMapping("/initialize-money")
    public ResponseEntity<Object> initializeMemberMoney(
            @RequestHeader("Member-Id") Long memberId) {

        log.info("회원 기본금 초기화 요청 - 회원: {}", memberId);
        stockTradeService.initializeMemberMoney(memberId);

        return ResponseEntity.ok().body(
                Map.of(
                        "status", "success",
                        "message", "회원 기본금이 3억원으로 초기화되었습니다.",
                        "memberId", memberId
                )
        );
    }

    /**
     * 보류 중인 주문 조회 API
     */
    @GetMapping("/pending-orders")
    public ResponseEntity<List<PendingOrderDTO>> getPendingOrders(
            @RequestHeader("Member-Id") Long memberId) {

        List<PendingOrderDTO> pendingOrders = stockTradeService.getPendingOrdersByMember(memberId);
        return ResponseEntity.ok(pendingOrders);
    }

    /**
     * 주문 취소 API
     */
    @DeleteMapping("/cancel-order/{orderId}")
    public ResponseEntity<Object> cancelOrder(
            @RequestHeader("Member-Id") Long memberId,
            @PathVariable Long orderId) {

        boolean result = stockTradeService.cancelPendingOrder(memberId, orderId);

        if (result) {
            return ResponseEntity.ok().body(
                    Map.of(
                            "status", "success",
                            "message", "주문이 성공적으로 취소되었습니다."
                    )
            );
        } else {
            return ResponseEntity.badRequest().body(
                    Map.of(
                            "status", "error",
                            "message", "주문 취소에 실패했습니다. 주문이 존재하지 않거나 이미 처리되었습니다."
                    )
            );
        }
    }
}