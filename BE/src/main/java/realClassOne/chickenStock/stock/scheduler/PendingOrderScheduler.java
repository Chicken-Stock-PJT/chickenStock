package realClassOne.chickenStock.stock.scheduler;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.stock.service.StockTradeService;

@Slf4j
@Component
@RequiredArgsConstructor
public class PendingOrderScheduler {

    private final StockTradeService stockTradeService;

    // 5초마다 실행되는 지정가 주문 처리 트리거
    @Scheduled(fixedRate = 5000000)
    public void triggerPendingOrderProcessing() {
        log.debug("PendingOrderScheduler 실행 - 지정가 주문 처리 시작");
        stockTradeService.processPendingOrders();
    }
}