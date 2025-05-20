package realClassOne.chickenStock.stock.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.dto.common.PendingOrderDTO;
import realClassOne.chickenStock.stock.dto.request.TradeRequestDTO;
import realClassOne.chickenStock.stock.dto.request.TradeTaskDTO;
import realClassOne.chickenStock.stock.dto.response.OrderCancelResponseDTO;
import realClassOne.chickenStock.stock.dto.response.TradeResponseDTO;
import realClassOne.chickenStock.stock.service.StockTradeService;
import realClassOne.chickenStock.stock.service.trade.TradeQueueService;
import realClassOne.chickenStock.stock.service.trade.TradingSecurityService;

import java.util.List;

@RestController
@RequestMapping("/api/stock/trading")
@RequiredArgsConstructor
@Slf4j
public class StockTradingController {

    private final StockTradeService stockTradeService;
    private final JwtTokenProvider jwtTokenProvider;
    private final MemberRepository memberRepository;
    private final TradeQueueService tradeQueueService;
    private final TradingSecurityService tradingSecurityService;

    @PostMapping("/buy")
    public ResponseEntity<TradeResponseDTO> buyStock(@RequestHeader("Authorization") String authorization,
                                                     @RequestBody @Valid TradeRequestDTO request) {
        try {
            // 토큰에서 회원 정보 추출
            String token = jwtTokenProvider.resolveToken(authorization);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 비정상 거래 패턴 체크
            boolean isAbnormal = tradingSecurityService.checkAbnormalTradingPattern(
                    memberId, request.getStockCode(), "BUY");

            // 제한된 회원인지 체크
            boolean isRestricted = tradingSecurityService.isRestricted(memberId);

            if (isAbnormal || isRestricted) {
                return ResponseEntity
                        .status(HttpStatus.FORBIDDEN)
                        .body(TradeResponseDTO.builder()
                                .status("ERROR")
                                .message("비정상적인 거래 패턴이 감지되어 제한되었습니다.")
                                .build());
            }

            // 시장가 주문인 경우 즉시 처리
            if (Boolean.TRUE.equals(request.getMarketOrder())) {
                return ResponseEntity.ok(stockTradeService.buyStock(authorization, request));
            }

            // 지정가 주문인 경우 큐에 넣어 비동기 처리
            boolean queued = tradeQueueService.queueTradeRequest(
                    new TradeTaskDTO(member, "BUY", request));

            if (!queued) {
                return ResponseEntity
                        .status(HttpStatus.SERVICE_UNAVAILABLE)
                        .body(TradeResponseDTO.builder()
                                .status("ERROR")
                                .message("시스템이 현재 요청을 처리할 수 없습니다. 잠시 후 다시 시도해주세요.")
                                .build());
            }

            return ResponseEntity.accepted().body(
                    TradeResponseDTO.builder()
                            .status("PENDING")
                            .message("매수 주문이 접수되었습니다.")
                            .stockCode(request.getStockCode())
                            .quantity(request.getQuantity())
                            .unitPrice(request.getPrice())
                            .totalPrice(request.getPrice() * request.getQuantity())
                            .build());

        } catch (CustomException e) {
            return ResponseEntity
                    .status(HttpStatus.BAD_REQUEST)
                    .body(TradeResponseDTO.builder()
                            .status("ERROR")
                            .message(e.getMessage())
                            .build());
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
    public ResponseEntity<TradeResponseDTO> sellStock(@RequestHeader("Authorization") String authorization,
                                                      @RequestBody @Valid TradeRequestDTO request) {
        try {
            // 토큰에서 회원 정보 추출
            String token = jwtTokenProvider.resolveToken(authorization);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 비정상 거래 패턴 체크
            boolean isAbnormal = tradingSecurityService.checkAbnormalTradingPattern(
                    memberId, request.getStockCode(), "SELL");

            // 제한된 회원인지 체크
            boolean isRestricted = tradingSecurityService.isRestricted(memberId);

            if (isAbnormal || isRestricted) {
                return ResponseEntity
                        .status(HttpStatus.FORBIDDEN)
                        .body(TradeResponseDTO.builder()
                                .status("ERROR")
                                .message("비정상적인 거래 패턴이 감지되어 제한되었습니다.")
                                .build());
            }

            // 시장가 주문인 경우 즉시 처리
            if (Boolean.TRUE.equals(request.getMarketOrder())) {
                return ResponseEntity.ok(stockTradeService.sellStock(authorization, request));
            }

            // 지정가 주문인 경우 큐에 넣어 비동기 처리
            boolean queued = tradeQueueService.queueTradeRequest(
                    new TradeTaskDTO(member, "SELL", request));

            if (!queued) {
                return ResponseEntity
                        .status(HttpStatus.SERVICE_UNAVAILABLE)
                        .body(TradeResponseDTO.builder()
                                .status("ERROR")
                                .message("시스템이 현재 요청을 처리할 수 없습니다. 잠시 후 다시 시도해주세요.")
                                .build());
            }

            return ResponseEntity.accepted().body(
                    TradeResponseDTO.builder()
                            .status("PENDING")
                            .message("매도 주문이 접수되었습니다.")
                            .stockCode(request.getStockCode())
                            .quantity(request.getQuantity())
                            .unitPrice(request.getPrice())
                            .totalPrice(request.getPrice() * request.getQuantity())
                            .build());

        } catch (CustomException e) {
            return ResponseEntity
                    .status(HttpStatus.BAD_REQUEST)
                    .body(TradeResponseDTO.builder()
                            .status("ERROR")
                            .message(e.getMessage())
                            .build());
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