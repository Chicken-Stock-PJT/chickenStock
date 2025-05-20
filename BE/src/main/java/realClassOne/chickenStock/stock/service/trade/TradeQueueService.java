package realClassOne.chickenStock.stock.service.trade;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.stock.dto.request.TradeTaskDTO;
import realClassOne.chickenStock.stock.service.StockTradeService;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

@Service
@RequiredArgsConstructor
@Slf4j
public class TradeQueueService {

    private final BlockingQueue<TradeTaskDTO> tradeQueue = new LinkedBlockingQueue<>(500);
    private final StockTradeService stockTradeService;
    private final ThreadPoolTaskExecutor tradeExecutor;
    private final TradingSecurityService tradingSecurityService;

    @PostConstruct
    public void init() {
        // 트레이드 작업 처리 스레드 시작
        startProcessingThreads();
    }

    public boolean queueTradeRequest(TradeTaskDTO task) {
        // 큐가 가득 찼을 때 즉시 실패 (back pressure)
        return tradeQueue.offer(task);
    }

    private void startProcessingThreads() {
        // 최대 5개의 처리 스레드
        for (int i = 0; i < 5; i++) {
            tradeExecutor.submit(this::processTradeQueue);
        }
    }

    // 트레이드 작업 처리 메서드 수정
    private void processTradeQueue() {
        while (!Thread.currentThread().isInterrupted()) {
            try {
                TradeTaskDTO task = tradeQueue.poll(100, TimeUnit.MILLISECONDS);
                if (task == null) continue;

                // 비정상 거래 패턴 체크
                boolean isAbnormal = tradingSecurityService.checkAbnormalTradingPattern(
                        task.getMember().getMemberId(),
                        task.getRequest().getStockCode(),
                        task.getType());

                // 제한된 회원인지 체크
                boolean isRestricted = tradingSecurityService.isRestricted(
                        task.getMember().getMemberId());

                if (isAbnormal || isRestricted) {
                    log.warn("비정상 거래 패턴으로 인해 요청이 거부됨: 회원={}, 종목={}, 타입={}",
                            task.getMember().getMemberId(),
                            task.getRequest().getStockCode(),
                            task.getType());
                    continue;
                }

                // 정상 거래 처리
                try {
                    if ("BUY".equals(task.getType())) {
                        stockTradeService.buyStock(task.getRequest(), task.getMember());
                    } else if ("SELL".equals(task.getType())) {
                        stockTradeService.sellStock(task.getRequest(), task.getMember());
                    }
                } catch (Exception e) {
                    log.error("트레이드 처리 중 오류 발생: {}", e.getMessage(), e);
                }

            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.info("트레이드 큐 처리 스레드 중단");
                break;
            }
        }
    }
}