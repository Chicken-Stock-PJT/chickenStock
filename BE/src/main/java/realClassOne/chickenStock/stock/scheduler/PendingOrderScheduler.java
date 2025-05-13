package realClassOne.chickenStock.stock.scheduler;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.stock.service.StockTradeService;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.util.concurrent.atomic.AtomicInteger;

@Slf4j
@Component
@RequiredArgsConstructor
public class PendingOrderScheduler {

    private final StockTradeService stockTradeService;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;

    // 실행 횟수 추적
    private final AtomicInteger executionCount = new AtomicInteger(0);

    // 연속 실패 횟수 추적
    private final AtomicInteger consecutiveFailures = new AtomicInteger(0);

    // 3초마다 실행되는 지정가 주문 처리 트리거
    @Scheduled(fixedRate = 3000)
    public void triggerPendingOrderProcessing() {
        try {
            // 웹소켓 연결 상태 확인
            if (!kiwoomWebSocketClient.isConnected()) {
                log.warn("WebSocket이 연결되지 않아 지정가 주문 처리 건너뜀");
                return;
            }

            // 실행 횟수 증가
            int count = executionCount.incrementAndGet();

            // 10번에 한 번만 로그 출력 (로그 스팸 방지)
            if (count % 10 == 0) {
                log.info("PendingOrderScheduler 실행 - 지정가 주문 처리 시작 (실행 횟수: {})", count);
            } else {
                log.debug("PendingOrderScheduler 실행 - 지정가 주문 처리 시작");
            }

            stockTradeService.processPendingOrders();

            // 성공적으로 실행되면 실패 카운터 리셋
            consecutiveFailures.set(0);

        } catch (Exception e) {
            int failures = consecutiveFailures.incrementAndGet();

            // 연속 실패가 5회 이상이면 경고 로그
            if (failures >= 5) {
                log.error("지정가 주문 처리 중 연속 {}회 실패", failures, e);
            } else {
                log.warn("지정가 주문 처리 중 오류 발생 (실패 횟수: {})", failures, e);
            }

            // 너무 많은 실패 시 스케줄러 일시 정지 고려
            if (failures >= 10) {
                log.error("연속 실패 10회 이상. 시스템 점검이 필요합니다.");
                // 여기에 알림 발송 로직 추가 가능
            }
        }
    }

    // 매일 자정에 카운터 리셋
    @Scheduled(cron = "0 0 0 * * *")
    public void resetCounters() {
        int totalExecutions = executionCount.getAndSet(0);
        log.info("일일 지정가 주문 처리 스케줄러 통계: 총 {}회 실행", totalExecutions);
    }
}