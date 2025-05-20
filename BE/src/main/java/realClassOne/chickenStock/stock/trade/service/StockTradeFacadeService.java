package realClassOne.chickenStock.stock.trade.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.dto.common.PendingOrderDTO;
import realClassOne.chickenStock.stock.trade.dto.common.TradeStatusDTO;
import realClassOne.chickenStock.stock.dto.request.TradeRequestDTO;
import realClassOne.chickenStock.stock.trade.dto.request.TradeTaskDTO;
import realClassOne.chickenStock.stock.dto.response.TradeResponseDTO;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class StockTradeFacadeService {

    private final StockTradeService stockTradeService;
    private final TradeQueueService tradeQueueService;
    private final JwtTokenProvider jwtTokenProvider;
    private final MemberRepository memberRepository;
    private final StockDataRepository stockDataRepository;

    /**
     * 매수 주문 처리 - 큐 기반 처리 방식
     */
    public TradeResponseDTO processBuyOrder(String authorization, TradeRequestDTO request) {
        try {
            // 토큰으로부터 회원 정보 추출
            String token = jwtTokenProvider.resolveToken(authorization);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 종목 정보 조회
            StockData stockData = stockDataRepository.findByShortCode(request.getStockCode())
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 요청 유효성 검증
            validateTradeRequest(request);

            // 시장가 주문과 지정가 주문을 구분하여 처리
            if (Boolean.TRUE.equals(request.getMarketOrder())) {
                // 시장가 주문은 큐를 통해 처리
                TradeTaskDTO tradeTask = new TradeTaskDTO(member, request, "BUY");
                tradeTask = tradeQueueService.queueTradeRequest(tradeTask);

                return TradeResponseDTO.builder()
                        .status("QUEUED")
                        .orderId(tradeTask.getOrderId())
                        .stockCode(request.getStockCode())
                        .stockName(stockData.getShortName())
                        .tradeType("BUY")
                        .quantity(request.getQuantity())
                        .unitPrice(null)
                        .message("시장가 매수 주문이 시스템에 접수되었습니다. 시간 순서에 따라 처리됩니다.")
                        .build();
            } else {
                // 지정가 주문은 stockTradeService로 직접 처리
                return stockTradeService.processLimitBuyOrder(member, stockData, request.getQuantity(), request.getPrice());
            }

        } catch (CustomException e) {
            // 서비스 레이어에서 발생한 예외는 그대로 전파
            throw e;
        } catch (Exception e) {
            log.error("매수 주문 처리 중 오류 발생", e);
            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
                    "주문 처리 중 시스템 오류가 발생했습니다.");
        }
    }

    /**
     * 매도 주문 처리 - 큐 기반 처리 방식
     */
    public TradeResponseDTO processSellOrder(String authorization, TradeRequestDTO request) {
        try {
            // 토큰으로부터 회원 정보 추출
            String token = jwtTokenProvider.resolveToken(authorization);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 종목 정보 조회
            StockData stockData = stockDataRepository.findByShortCode(request.getStockCode())
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 요청 유효성 검증
            validateTradeRequest(request);

            // 시장가 주문과 지정가 주문을 구분하여 처리
            if (Boolean.TRUE.equals(request.getMarketOrder())) {
                // 시장가 주문은 큐를 통해 처리
                TradeTaskDTO tradeTask = new TradeTaskDTO(member, request, "SELL");
                tradeTask = tradeQueueService.queueTradeRequest(tradeTask);

                return TradeResponseDTO.builder()
                        .status("QUEUED")
                        .orderId(tradeTask.getOrderId())
                        .stockCode(request.getStockCode())
                        .stockName(stockData.getShortName())
                        .tradeType("SELL")
                        .quantity(request.getQuantity())
                        .unitPrice(null)
                        .message("시장가 매도 주문이 시스템에 접수되었습니다. 시간 순서에 따라 처리됩니다.")
                        .build();
            } else {
                // 지정가 주문은 stockTradeService로 직접 처리
                return stockTradeService.processLimitSellOrder(member, stockData, request.getQuantity(), request.getPrice());
            }

        } catch (CustomException e) {
            // 서비스 레이어에서 발생한 예외는 그대로 전파
            throw e;
        } catch (Exception e) {
            log.error("매도 주문 처리 중 오류 발생", e);
            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
                    "주문 처리 중 시스템 오류가 발생했습니다.");
        }
    }

    /**
     * 주문 상태 조회
     */
    public TradeStatusDTO getOrderStatus(String authorization, Long orderId) {
        try {
            // 주문 상태 조회 로직
            // (구현 필요)
            return null;
        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("주문 상태 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.ORDER_INQUIRY_FAILED);
        }
    }

    /**
     * 미체결 주문 조회
     */
    public List<PendingOrderDTO> getPendingOrdersByMember(String authorization) {
        try {
            String token = jwtTokenProvider.resolveToken(authorization);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
            return stockTradeService.getPendingOrdersByMember(authorization);
        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("미체결 주문 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.ORDER_INQUIRY_FAILED);
        }
    }

    /**
     * 주문 취소 처리
     */
    public boolean cancelPendingOrder(String authorization, Long orderId) {
        try {
            return stockTradeService.cancelPendingOrder(authorization, orderId);
        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("주문 취소 처리 중 오류 발생", e);
            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED);
        }
    }

    /**
     * 주문 상태 조회
     */
    public TradeStatusDTO getOrderStatus(String authorization, String requestId) {
        try {
            // 주문 상태 조회 로직
            // (구현 필요)
            return null;
        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("주문 상태 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.ORDER_INQUIRY_FAILED);
        }
    }

    // 요청 유효성 검증
    private void validateTradeRequest(TradeRequestDTO request) {
        if (request.getStockCode() == null || request.getStockCode().trim().isEmpty()) {
            throw new CustomException(StockErrorCode.INVALID_REQUEST, "종목 코드가 필요합니다.");
        }

        if (request.getQuantity() == null || request.getQuantity() <= 0) {
            throw new CustomException(StockErrorCode.INVALID_QUANTITY, "유효한 수량이 필요합니다.");
        }

        // 지정가 주문인 경우 가격 검증
        if (Boolean.FALSE.equals(request.getMarketOrder()) && (request.getPrice() == null || request.getPrice() <= 0)) {
            throw new CustomException(StockErrorCode.INVALID_PRICE, "지정가 주문에는 가격이 필요합니다.");
        }
    }
}