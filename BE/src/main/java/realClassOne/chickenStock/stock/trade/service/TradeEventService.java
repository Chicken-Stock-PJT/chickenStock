package realClassOne.chickenStock.stock.trade.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.event.OrderExecutionEvent;
import realClassOne.chickenStock.stock.event.TradeEvent;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

@Service
@RequiredArgsConstructor
@Slf4j
public class TradeEventService {

    private final ApplicationEventPublisher eventPublisher;
    private final RedisTemplate<String, Object> redisTemplate;

    /**
     * 주문 이벤트 발행
     */
    public void publishTradeEvent(TradeEvent event) {
        try {
            // 이벤트 발행
            eventPublisher.publishEvent(event);

            // 주문 관련 이벤트는 상태 저장
            if (event.getOrderId() != null) {
                saveOrderStatus(event);
            }

        } catch (Exception e) {
            log.error("이벤트 발행 중 오류: {}", e.getMessage(), e);
        }
    }

    /**
     * 주문 상태 저장
     */
    private void saveOrderStatus(TradeEvent event) {
        if (event.getMember() == null) return;

        try {
            String key = "order:status:" + event.getMember().getMemberId() + ":" + event.getOrderId();
            Map<String, Object> status = new HashMap<>();
            status.put("orderId", event.getOrderId());
            status.put("stockCode", event.getStockCode());
            status.put("type", event.getType().name());
            status.put("message", event.getMessage());
            status.put("timestamp", event.getTimestamp());

            if (event.getPrice() != null) {
                status.put("price", event.getPrice());
            }

            // 24시간 유효기간
            redisTemplate.opsForHash().putAll(key, status);
            redisTemplate.expire(key, 24, TimeUnit.HOURS);

        } catch (Exception e) {
            log.error("주문 상태 저장 중 오류: {}", e.getMessage(), e);
        }
    }

//    /**
//     * 주문 상태 조회
//     */
//    public Map<String, Object> getOrderStatus(Long memberId, Long orderId) {
//        try {
//            String key = "order:status:" + memberId + ":" + orderId;
//            return redisTemplate.opsForHash().entries(key);
//        } catch (Exception e) {
//            log.error("주문 상태 조회 중 오류: {}", e.getMessage(), e);
//            return null;
//        }
//    }

    /**
     * 지정가 주문 체결 실행 요청
     * PendingOrder와 현재가를 받아 주문 체결 이벤트를 발행합니다.
     */
    public void requestOrderExecution(PendingOrder order, Long currentPrice) {
        try {
            // 이벤트 발행
            OrderExecutionEvent event = new OrderExecutionEvent(order, currentPrice);
            eventPublisher.publishEvent(event);

            log.info("주문 체결 이벤트 발행: 주문ID={}, 종목={}, 가격={}",
                    order.getOrderId(), order.getStockData().getShortCode(), currentPrice);

            // 주문 상태 저장 (옵션)
            saveOrderExecutionRequest(order.getMember().getMemberId(), order.getOrderId(), currentPrice);
        } catch (Exception e) {
            log.error("주문 체결 이벤트 발행 중 오류: {}", e.getMessage(), e);
        }
    }

    /**
     * 주문 체결 요청 상태 저장 (옵션)
     */
    private void saveOrderExecutionRequest(Long memberId, Long orderId, Long price) {
        try {
            String key = "order:execution:" + memberId + ":" + orderId;
            Map<String, Object> status = new HashMap<>();
            status.put("orderId", orderId);
            status.put("price", price);
            status.put("requestTime", System.currentTimeMillis());
            status.put("status", "REQUESTED");

            // 24시간 유효기간
            redisTemplate.opsForHash().putAll(key, status);
            redisTemplate.expire(key, 24, TimeUnit.HOURS);
        } catch (Exception e) {
            log.error("주문 체결 요청 상태 저장 중 오류: {}", e.getMessage(), e);
        }
    }
}