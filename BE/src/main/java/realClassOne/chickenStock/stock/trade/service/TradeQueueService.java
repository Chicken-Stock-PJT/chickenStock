package realClassOne.chickenStock.stock.trade.service;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.stock.trade.dto.request.TradeTaskDTO;
import realClassOne.chickenStock.stock.dto.response.TradeResponseDTO;
import realClassOne.chickenStock.stock.event.TradeEvent;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class TradeQueueService {

    // 글로벌 주문 큐 (시간 순서 보장)
    private final BlockingQueue<TradeTaskDTO> globalOrderQueue = new LinkedBlockingQueue<>(1000);

    // 종목별 처리 큐
    private final Map<String, BlockingQueue<TradeTaskDTO>> stockQueues = new ConcurrentHashMap<>();

    // 종목별 처리 스레드
    private final Map<String, Thread> stockProcessingThreads = new ConcurrentHashMap<>();

    // 필요한 의존성 주입
    private final StockTradeService stockTradeService;
    private final StockPriceCacheService stockPriceCacheService;
    private final TradingSecurityService tradingSecurityService;
    private final TradeEventService tradeEventService;
    private final DistributedLockService distributedLockService;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;

    @PostConstruct
    public void init() {
        // 분배 스레드 시작
        startDistributorThread();
    }

    /**
     * 거래 요청을 큐에 넣기 (시간 우선 순서 보장)
     */
    public TradeTaskDTO queueTradeRequest(TradeTaskDTO task) {
        try {
            // 주문 ID 생성
            Long orderId = generateOrderId();
            task.setOrderId(orderId);

            // 시간 기록
            task.setTimestamp(System.currentTimeMillis());

            // 보안 검사 (빠른 거부)
            performSecurityCheck(task);

            // 글로벌 큐에 추가
            if (!globalOrderQueue.offer(task)) {
                throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
                        "시스템이 현재 많이 혼잡합니다. 잠시 후 다시 시도해주세요.");
            }

            // 이벤트 발행 (주문 접수)
            tradeEventService.publishTradeEvent(TradeEvent.orderReceived(task));

            log.info("주문이 글로벌 큐에 추가됨: orderId={}, 종목: {}, 타입: {}",
                    orderId, task.getRequest().getStockCode(), task.getType());

            return task;
        } catch (CustomException e) {
            throw e;
        } catch (Exception e) {
            log.error("주문 큐 추가 중 오류: {}", e.getMessage(), e);
            throw new CustomException(StockErrorCode.TRADE_PROCESSING_FAILED,
                    "주문 처리 중 시스템 오류가 발생했습니다.");
        }
    }

    /**
     * 분배 스레드 시작 (글로벌 큐 -> 종목별 큐)
     */
    private void startDistributorThread() {
        Thread thread = new Thread(() -> {
            log.info("주문 분배 스레드 시작");

            while (!Thread.currentThread().isInterrupted()) {
                try {
                    // 글로벌 큐에서 주문을 하나씩 가져옴
                    TradeTaskDTO task = globalOrderQueue.poll(100, TimeUnit.MILLISECONDS);
                    if (task == null) continue;

                    String stockCode = task.getRequest().getStockCode();

                    // 종목별 큐 생성 또는 가져오기
                    BlockingQueue<TradeTaskDTO> stockQueue = getOrCreateStockQueue(stockCode);

                    // 종목별 큐에 추가
                    if (!stockQueue.offer(task)) {
                        log.error("종목 {} 큐가 가득 차서 주문 거부: {}",
                                stockCode, task.getOrderId());

                        // 실패 이벤트 발행
                        tradeEventService.publishTradeEvent(
                                TradeEvent.orderFailed(task, "종목 큐가 가득 찼습니다. 잠시 후 다시 시도해주세요.")
                        );
                    } else {
                        // 처리 중 이벤트 발행
                        tradeEventService.publishTradeEvent(TradeEvent.orderProcessing(task));
                    }

                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    log.info("주문 분배 스레드 중단");
                    break;
                } catch (Exception e) {
                    log.error("주문 분배 중 오류", e);
                }
            }
        });

        thread.setName("trade-distributor");
        thread.start();
    }

    /**
     * 종목별 처리 큐 가져오기 (없으면 생성)
     */
    private synchronized BlockingQueue<TradeTaskDTO> getOrCreateStockQueue(String stockCode) {
        if (!stockQueues.containsKey(stockCode)) {
            log.info("종목 {} 전용 처리 큐 및 스레드 생성", stockCode);

            // 큐 생성
            BlockingQueue<TradeTaskDTO> newQueue = new LinkedBlockingQueue<>(100);
            stockQueues.put(stockCode, newQueue);

            // 처리 스레드 생성 및 시작
            Thread processingThread = new Thread(() -> processStockQueue(stockCode, newQueue));
            processingThread.setName("trade-processor-" + stockCode);
            processingThread.start();

            stockProcessingThreads.put(stockCode, processingThread);
        }

        return stockQueues.get(stockCode);
    }

    private void processStockQueue(String stockCode, BlockingQueue<TradeTaskDTO> queue) {
        while (!Thread.currentThread().isInterrupted()) {
            try {
                // 종목별 큐에서 주문 가져오기
                TradeTaskDTO task = queue.poll(100, TimeUnit.MILLISECONDS);
                if (task == null) continue;

                // 1. 캐시에서 가격 조회 시도 (3초 TTL)
                Long cachedPrice = stockPriceCacheService.getFromCacheOnly(stockCode);

                if (cachedPrice != null) {
                    // 캐시된 가격으로 처리
                    log.debug("캐시된 가격({}원)으로 처리: {}", cachedPrice, stockCode);
                    executeTradeWithPrice(task, cachedPrice);
                }
                // 2. 이미 웹소켓 구독 중인 종목인 경우
                else if (kiwoomWebSocketClient.isSubscribed(stockCode)) {
                    // 웹소켓 실시간 데이터에서 가격 조회
                    Long websocketPrice = kiwoomWebSocketClient.getLatestPrice(stockCode);

                    if (websocketPrice != null) {
                        log.debug("웹소켓 가격({}원)으로 처리: {}", websocketPrice, stockCode);
                        // 웹소켓에서 얻은 가격으로 처리 및 캐시 업데이트
                        stockPriceCacheService.updatePriceAndNotify(stockCode, websocketPrice);
                        executeTradeWithPrice(task, websocketPrice);
                    } else {
                        // 웹소켓에서 가격을 얻지 못한 경우 REST API 사용
                        log.warn("구독 중인데 웹소켓에서 가격 조회 실패, API로 시도: {}", stockCode);
                        fetchPriceFromApiAndProcess(task, stockCode);
                    }
                }
                // 3. 구독 중이지 않은 종목은 바로 REST API 사용
                else {
                    log.debug("구독 중이지 않은 종목, API로 가격 조회: {}", stockCode);
                    fetchPriceFromApiAndProcess(task, stockCode);
                }
            } catch (Exception e) {
                log.error("주문 처리 중 오류: {}", e.getMessage(), e);
            }
        }
    }

    // REST API를 통한 가격 조회 및 처리 메서드
    private void fetchPriceFromApiAndProcess(TradeTaskDTO task, String stockCode) {
        try {
            Long apiPrice = stockTradeService.getCurrentStockPriceWithRetry(stockCode);

            if (apiPrice != null) {
                log.debug("API 가격({}원)으로 처리: {}", apiPrice, stockCode);
                // 가격을 캐시에 저장
                stockPriceCacheService.updatePriceAndNotify(stockCode, apiPrice);
                // 주문 처리
                executeTradeWithPrice(task, apiPrice);
            } else {
                // API에서도 가격을 얻지 못한 경우
                log.error("API에서도 가격 조회 실패: {}", stockCode);
                tradeEventService.publishTradeEvent(
                        TradeEvent.orderFailed(task, "현재 종목 가격을 조회할 수 없습니다.")
                );
            }
        } catch (Exception e) {
            log.error("API 가격 조회 중 오류: {}", e.getMessage(), e);
            tradeEventService.publishTradeEvent(
                    TradeEvent.orderFailed(task, "가격 조회 중 오류가 발생했습니다: " + e.getMessage())
            );
        }
    }

    /**
     * 알려진 가격으로 거래 실행
     */
    private void executeTradeWithPrice(TradeTaskDTO task, Long currentPrice) {
        try {
            // 분산락 획득 시도
            String lockName = "trade:" + task.getRequest().getStockCode();
            String ownerId = "thread-" + Thread.currentThread().getId() + "-orderId-" + task.getOrderId();
            boolean lockAcquired = distributedLockService.tryLockWithRetry(lockName, ownerId, 3);

            if (!lockAcquired) {
                log.warn("종목 {} 락 획득 실패로 재시도 예정: orderId={}",
                        task.getRequest().getStockCode(), task.getOrderId());

                // 요청을 다시 글로벌 큐에 넣어 재시도
                globalOrderQueue.offer(task);
                return;
            }

            try {
                TradeResponseDTO response = null;

                if ("BUY".equals(task.getType())) {
                    response = stockTradeService.executeBuyWithPrice(
                            task.getMember(), task.getRequest(), currentPrice);
                } else {
                    response = stockTradeService.executeSellWithPrice(
                            task.getMember(), task.getRequest(), currentPrice);
                }

                // 주문 성공 처리
                if (response != null && ("SUCCESS".equals(response.getStatus()) || "COMPLETED".equals(response.getStatus()))) {
                    tradeEventService.publishTradeEvent(
                            TradeEvent.orderExecuted(task, currentPrice)
                    );
                } else {
                    // 실패 처리
                    tradeEventService.publishTradeEvent(
                            TradeEvent.orderFailed(task, response != null ? response.getMessage() : "주문 처리 실패")
                    );
                }

            } finally {
                distributedLockService.unlock(lockName, ownerId);
            }
        } catch (Exception e) {
            log.error("주문 실행 중 오류: {}", e.getMessage(), e);

            // 실패 이벤트 발행
            tradeEventService.publishTradeEvent(
                    TradeEvent.orderFailed(task, "주문 처리 중 오류: " + e.getMessage())
            );
        }
    }

    /**
     * 주문 ID 생성 (시퀀스 또는 UUID 기반)
     */
    private Long generateOrderId() {
        // 데이터베이스 시퀀스를 사용하는 방법 (구현 필요)
        // 또는 임시로 UUID의 상위 48비트 사용
        return Math.abs(UUID.randomUUID().getMostSignificantBits());
    }

    /**
     * 보안 검사 수행 (빠른 거부)
     */
    private void performSecurityCheck(TradeTaskDTO task) {
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

            throw new CustomException(StockErrorCode.TRADE_RESTRICTED,
                    "비정상적인 거래 패턴이 감지되어 제한되었습니다.");
        }
    }
}