package realClassOne.chickenStock.stock.service.trade;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.event.OrderExecutionEvent;
import realClassOne.chickenStock.stock.repository.PendingOrderRepository;

@Service
@RequiredArgsConstructor
@Slf4j
public class TradeEventService {

    private final ApplicationEventPublisher eventPublisher;
    private final PendingOrderRepository pendingOrderRepository;

    public void requestOrderExecution(PendingOrder order, Long currentPrice) {
        try {
            // 약간의 지연을 주어 원본 트랜잭션이 완료될 시간을 확보
            Thread.sleep(100);

            // 최신 상태 확인을 위해 주문 재조회
            PendingOrder freshOrder = pendingOrderRepository.findById(order.getOrderId())
                    .orElse(null);

            if (freshOrder == null || freshOrder.getStatus() != PendingOrder.OrderStatus.PENDING) {
                log.info("주문 ID {}는 이미 처리되었거나 존재하지 않아 이벤트 발행 취소", order.getOrderId());
                return;
            }

            log.info("주문 체결 이벤트 발행 - 주문ID: {}, 가격: {}", freshOrder.getOrderId(), currentPrice);
            eventPublisher.publishEvent(new OrderExecutionEvent(freshOrder, currentPrice));
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            log.error("주문 이벤트 발행 중 오류", e);
        }
    }
}