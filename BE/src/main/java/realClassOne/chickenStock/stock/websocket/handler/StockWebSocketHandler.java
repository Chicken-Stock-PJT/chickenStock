package realClassOne.chickenStock.stock.websocket.handler;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.io.IOException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.locks.ReentrantLock;

@Component
@Slf4j
@RequiredArgsConstructor
public class StockWebSocketHandler extends TextWebSocketHandler implements KiwoomWebSocketClient.StockDataListener {

    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final ObjectMapper objectMapper;

    private static final DateTimeFormatter formatter =
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
                    .withZone(ZoneId.of("Asia/Seoul")); // 🌐 [KST 포맷 정의]

    // 세션 관리용 맵 - 동시성을 위해 ConcurrentHashMap 사용
    private final Map<String, WebSocketSession> sessions = new ConcurrentHashMap<>();

    // 각 세션별 구독 종목 관리 - 동시성 보장
    private final Map<String, Set<String>> sessionSubscriptions = new ConcurrentHashMap<>();

    // 종목별 구독자 수 관리
    private final Map<String, Integer> stockCodeSubscriberCount = new ConcurrentHashMap<>();

    // 세션별 메시지 큐 - 메시지 전송 동시성 문제 해결
    private final Map<String, BlockingQueue<String>> messageQueues = new ConcurrentHashMap<>();

    // 세션별 메시지 처리 스레드
    private final Map<String, Thread> sessionWorkers = new ConcurrentHashMap<>();

    // 구독 및 구독 해제 작업에 대한 락
    private final ReentrantLock subscriptionLock = new ReentrantLock();

    // 실패한 세션 메시지 관리를 위한 재시도 큐
    private final BlockingQueue<RetryMessage> retryQueue = new LinkedBlockingQueue<>();

    // 구독/구독 해제 작업 큐
    private final BlockingQueue<SubscriptionTask> subscriptionTaskQueue = new LinkedBlockingQueue<>();

    // 재시도 메시지 클래스
    private static class RetryMessage {
        final String sessionId;
        final String message;
        final int attempts;
        final long nextRetryTime;

        RetryMessage(String sessionId, String message, int attempts) {
            this.sessionId = sessionId;
            this.message = message;
            this.attempts = attempts;
            this.nextRetryTime = System.currentTimeMillis() + (1000L * Math.min(30, attempts * 2)); // 백오프 전략
        }
    }

    // 구독 작업 클래스
    private static class SubscriptionTask {
        enum TaskType { SUBSCRIBE, UNSUBSCRIBE }

        final String sessionId;
        final String stockCode;
        final TaskType type;
        final CompletableFuture<Boolean> result;

        SubscriptionTask(String sessionId, String stockCode, TaskType type) {
            this.sessionId = sessionId;
            this.stockCode = stockCode;
            this.type = type;
            this.result = new CompletableFuture<>();
        }
    }

    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        String sessionId = session.getId();
        log.info("클라이언트 연결: {}", sessionId);

        // 세션 등록
        sessions.put(sessionId, session);
        sessionSubscriptions.put(sessionId, ConcurrentHashMap.newKeySet());

        // 메시지 큐 생성
        BlockingQueue<String> messageQueue = new LinkedBlockingQueue<>();
        messageQueues.put(sessionId, messageQueue);

        // 메시지 처리 스레드 시작
        Thread worker = new Thread(() -> processSessionMessages(sessionId));
        worker.setName("ws-worker-" + sessionId);
        worker.setDaemon(true);
        worker.start();
        sessionWorkers.put(sessionId, worker);

        // KiwoomWebSocketClient에 리스너로 등록 (첫 번째 클라이언트 연결시에만)
        if (sessions.size() == 1) {
            kiwoomWebSocketClient.addListener(this);
        }

        try {
            // 클라이언트에게 연결 성공 메시지 전송
            ObjectNode connectMessage = objectMapper.createObjectNode();
            connectMessage.put("type", "connected");
            connectMessage.put("message", "실시간 주식 데이터 서버에 연결되었습니다");

            // 큐에 메시지 추가
            messageQueue.offer(objectMapper.writeValueAsString(connectMessage));
        } catch (Exception e) {
            log.error("연결 메시지 생성 실패", e);
        }
    }

    // 메시지 처리 스레드 실행 메서드
    private void processSessionMessages(String sessionId) {
        BlockingQueue<String> queue = messageQueues.get(sessionId);
        int failCount = 0;

        while (!Thread.currentThread().isInterrupted()) {
            try {
                String message = queue.poll(100, TimeUnit.MILLISECONDS); // 폴링 방식으로 변경하여 인터럽트 대응 개선
                if (message == null) continue; // 메시지가 없으면 다음 반복으로

                WebSocketSession session = sessions.get(sessionId);
                if (session == null || !session.isOpen()) {
                    // 세션이 없거나 닫혀있으면 스레드 종료
                    log.warn("세션 {} 이 닫혔거나 없어 메시지 처리 스레드 종료", sessionId);
                    break;
                }

                try {
                    synchronized (session) { // 세션별 동기화
                        session.sendMessage(new TextMessage(message));
                    }
                    failCount = 0; // 성공시 실패 카운트 리셋
                } catch (IOException e) {
                    failCount++;
                    log.error("메시지 전송 중 오류 발생({}번째 실패): {}", failCount, sessionId, e);

                    if (failCount <= 3) { // 일정 횟수만 재시도
                        // 실패한 메시지 재시도 큐에 추가
                        retryQueue.offer(new RetryMessage(sessionId, message, failCount));
                    }

                    // 연속 실패 시 세션 상태 확인
                    if (failCount >= 5) {
                        log.warn("세션 {} 연속 5회 전송 실패, 세션 상태 확인 필요", sessionId);
                        // 세션이 문제가 있다면 스레드 종료
                        if (!verifySessionStatus(session)) {
                            break;
                        }
                    }
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.info("세션 {} 메시지 처리 스레드 중단됨", sessionId);
                break;
            } catch (Exception e) {
                log.error("메시지 처리 중 예기치 않은 오류 발생: {}", sessionId, e);
            }
        }

        // 스레드 종료 시 자원 정리
        log.info("세션 {} 메시지 처리 스레드 종료", sessionId);
    }

    // 세션 상태 확인
    private boolean verifySessionStatus(WebSocketSession session) {
        try {
            return session != null && session.isOpen();
        } catch (Exception e) {
            log.error("세션 상태 확인 중 오류", e);
            return false;
        }
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        try {
            // 클라이언트로부터 메시지 수신 처리
            JsonNode requestJson = objectMapper.readTree(message.getPayload());
            String action = requestJson.get("action").asText();
            String sessionId = session.getId();

            // list 작업은 stockCode가 필요하지 않으므로 별도 처리
            if ("list".equals(action)) {
                // (기존 코드 유지)
                return;
            }

            // 나머지 작업은 stockCode 필요
            String stockCode = requestJson.has("stockCode") ? requestJson.get("stockCode").asText() : null;

            if (stockCode == null || stockCode.trim().isEmpty()) {
                sendInfoMessage(sessionId, "종목 코드가 유효하지 않습니다.");
                return;
            }

            stockCode = stockCode.trim();

            if ("subscribe".equals(action)) {
                // 구독 요청을 작업 큐에 추가하고 결과 대기
                SubscriptionTask task = new SubscriptionTask(sessionId, stockCode, SubscriptionTask.TaskType.SUBSCRIBE);
                subscriptionTaskQueue.offer(task);

                boolean success = task.result.get(10, TimeUnit.SECONDS); // 타임아웃 10초

                if (success) {
                    sendSuccessMessage(sessionId, "구독", stockCode);
                } else {
                    sendErrorMessage(sessionId, "종목 등록에 실패했습니다: " + stockCode);
                }
            } else if ("unsubscribe".equals(action)) {
                // 구독 해제 요청을 작업 큐에 추가하고 결과 대기
                SubscriptionTask task = new SubscriptionTask(sessionId, stockCode, SubscriptionTask.TaskType.UNSUBSCRIBE);
                subscriptionTaskQueue.offer(task);

                boolean success = task.result.get(10, TimeUnit.SECONDS); // 타임아웃 10초

                if (success) {
                    sendSuccessMessage(sessionId, "구독 해제", stockCode);
                } else {
                    sendErrorMessage(sessionId, "종목 해제에 실패했습니다: " + stockCode);
                }
            } else {
                // 알 수 없는 작업 요청
                sendErrorMessage(sessionId, "지원하지 않는 작업입니다: " + action);
            }
        } catch (Exception e) {
            log.error("클라이언트 메시지 처리 중 오류 발생", e);
            try {
                sendErrorMessage(session.getId(), "메시지 처리 중 오류가 발생했습니다.");
            } catch (Exception ex) {
                log.error("오류 메시지 전송 실패", ex);
            }
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) {
        String sessionId = session.getId();
        log.info("클라이언트 연결 종료: {}", sessionId);

        // 해당 세션의 모든 구독 종목 해제
        Set<String> subscribedStocks = sessionSubscriptions.get(sessionId);
        if (subscribedStocks != null && !subscribedStocks.isEmpty()) {
            // 복사본 생성하여 ConcurrentModificationException 방지
            Set<String> stocksToUnsubscribe = new HashSet<>(subscribedStocks);

            // 모든 구독 종목에 대해 구독자 수 즉시 차감 및 비동기 해제 작업 추가
            for (String stockCode : stocksToUnsubscribe) {
                try {
                    // 구독자 수 차감 (동기 작업)
                    subscriptionLock.lock();
                    try {
                        // 구독자 수 감소 - 먼저 직접 차감
                        stockCodeSubscriberCount.computeIfPresent(stockCode, (k, v) -> {
                            int newCount = v - 1;
                            log.info("세션 종료로 인한 종목 {} 구독자 수 즉시 차감: {}명 -> {}명",
                                    stockCode, v, newCount > 0 ? newCount : 0);
                            return newCount > 0 ? newCount : null;
                        });
                    } finally {
                        subscriptionLock.unlock();
                    }

                    // 비동기 구독 해제 작업도 추가 (API 호출 처리용)
                    SubscriptionTask task = new SubscriptionTask(sessionId, stockCode, SubscriptionTask.TaskType.UNSUBSCRIBE);
                    subscriptionTaskQueue.offer(task);

                    // 세션 구독 목록에서 제거 (메모리 상태 정리)
                    subscribedStocks.remove(stockCode);

                    log.info("세션 종료로 인한 종목 {} 구독 해제 요청 추가", stockCode);
                } catch (Exception e) {
                    log.error("세션 종료 시 종목 {} 구독 해제 중 오류 발생", stockCode, e);
                }
            }
        }

        // 메시지 처리 스레드 중지
        Thread worker = sessionWorkers.remove(sessionId);
        if (worker != null) {
            worker.interrupt();
        }

        // 메시지 큐 제거
        messageQueues.remove(sessionId);

        // 세션 정보 제거
        sessions.remove(sessionId);
        sessionSubscriptions.remove(sessionId);

        // 연결된 클라이언트가 없으면 리스너 제거
        if (sessions.isEmpty()) {
            kiwoomWebSocketClient.removeListener(this);
            log.info("모든 클라이언트 연결 종료됨, 리스너 제거");
        }
    }

    private void sendSuccessMessage(String sessionId, String action, String stockCode) {
        try {
            ObjectNode message = objectMapper.createObjectNode();
            message.put("type", "success");
            message.put("action", action);
            message.put("stockCode", stockCode);

            // 메시지 큐에 추가
            BlockingQueue<String> queue = messageQueues.get(sessionId);
            if (queue != null) {
                queue.offer(objectMapper.writeValueAsString(message));
            }
        } catch (Exception e) {
            log.error("성공 메시지 생성 실패", e);
        }
    }

    private void sendErrorMessage(String sessionId, String errorMessage) {
        try {
            ObjectNode message = objectMapper.createObjectNode();
            message.put("type", "error");
            message.put("message", errorMessage);

            // 메시지 큐에 추가
            BlockingQueue<String> queue = messageQueues.get(sessionId);
            if (queue != null) {
                queue.offer(objectMapper.writeValueAsString(message));
            }
        } catch (Exception e) {
            log.error("오류 메시지 생성 실패", e);
        }
    }

    private void sendInfoMessage(String sessionId, String infoMessage) {
        try {
            ObjectNode message = objectMapper.createObjectNode();
            message.put("type", "info");
            message.put("message", infoMessage);

            // 메시지 큐에 추가
            BlockingQueue<String> queue = messageQueues.get(sessionId);
            if (queue != null) {
                queue.offer(objectMapper.writeValueAsString(message));
            }
        } catch (Exception e) {
            log.error("정보 메시지 생성 실패", e);
        }
    }

    // 기존 onStockPriceUpdate 메서드를 확장하여 0B(주식체결) 트랜잭션에서 체결 정보 추출
    @Override
    public void onStockPriceUpdate(String stockCode, JsonNode data) {
        try {
            // 종목 코드에서 _AL 접미사 제거
            String originalStockCode = stockCode.replace("_AL", "");

            // 주식체결 데이터 (0B)에서 필요한 정보만 추출
            ObjectNode messageNode = objectMapper.createObjectNode();
            messageNode.put("type", "stockPrice");
            messageNode.put("stockCode", originalStockCode);  // 원본 종목 코드 사용
            messageNode.put("currentPrice", data.get("10").asText());      // 현재가
            messageNode.put("priceChange", data.get("11").asText());       // 전일대비
            messageNode.put("changeRate", data.get("12").asText());        // 등락율
            messageNode.put("timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter)); // 🌐 [KST 적용]

            String message = objectMapper.writeValueAsString(messageNode);

            // 해당 종목을 구독 중인 세션에만 전송
            broadcastToSubscribers(originalStockCode, message);

            // 추가: 체결 정보가 있을 경우 체결 데이터 추출 및 전송
            if (data.has("15") && !data.get("15").asText().isEmpty()) {
                String volume = data.get("15").asText();
                String tradeType = "UNKNOWN";
                int quantity = 0;

                // 키움 API는 거래량 필드에 +는 매수체결, -는 매도체결로 표시
                if (volume.startsWith("+")) {
                    tradeType = "BUY";
                    quantity = Integer.parseInt(volume.substring(1));
                } else if (volume.startsWith("-")) {
                    tradeType = "SELL";
                    quantity = Integer.parseInt(volume.substring(1));
                } else {
                    // 부호가 없는 경우 숫자만 있다면 파싱 시도
                    try {
                        quantity = Integer.parseInt(volume);
                    } catch (NumberFormatException e) {
                        log.warn("거래량 파싱 실패: {}", volume);
                    }
                }

                // 유효한 체결량이 있는 경우에만 체결 정보 전송
                if (quantity > 0) {
                    // 현재가 파싱
                    String priceStr = data.get("10").asText().replace(",", "").replace("+", "").replace("-", "").trim();
                    Long price = null;
                    try {
                        price = Long.parseLong(priceStr);
                    } catch (NumberFormatException e) {
                        log.warn("현재가 파싱 실패: {}", priceStr);
                    }

                    if (price != null) {
                        Long totalAmount = price * quantity;
                        String timestamp = ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter); // 🌐 [KST 적용]

                        // 체결 정보 전송
                        broadcastTradeExecution(originalStockCode, tradeType, quantity, price, totalAmount, timestamp);
                    }
                }
            }
        } catch (Exception e) {
            log.error("주식체결 데이터 처리 중 오류 발생", e);
        }
    }

    @Override
    public void onStockBidAskUpdate(String stockCode, JsonNode data) {
        try {
            // 종목 코드에서 _AL 접미사 제거
            String originalStockCode = stockCode.replace("_AL", "");

            // 로깅용
            String timestamp = data.get("21").asText();         // 호가시간

            // 주식호가잔량 데이터 (0D)에서 필요한 정보만 추출
            ObjectNode messageNode = objectMapper.createObjectNode();
            messageNode.put("type", "stockBidAsk");
            messageNode.put("stockCode", originalStockCode);  // 원본 종목 코드 사용
            messageNode.put("timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter)); // 🌐 [KST 적용]  // 호가시간

            // 매도호가 및 수량 (상위 8개)
            ObjectNode askPrices = objectMapper.createObjectNode();
            ObjectNode askVolumes = objectMapper.createObjectNode();
            for (int i = 1; i <= 8; i++) {
                String askPriceKey = String.format("4%d", i);
                String askVolumeKey = String.format("6%d", i);
                askPrices.put(String.valueOf(i), data.get(askPriceKey).asText());
                askVolumes.put(String.valueOf(i), data.get(askVolumeKey).asText());
            }

            // 매수호가 및 수량 (상위 8개)
            ObjectNode bidPrices = objectMapper.createObjectNode();
            ObjectNode bidVolumes = objectMapper.createObjectNode();
            for (int i = 1; i <= 8; i++) {
                String bidPriceKey = String.format("5%d", i);
                String bidVolumeKey = String.format("7%d", i);
                bidPrices.put(String.valueOf(i), data.get(bidPriceKey).asText());
                bidVolumes.put(String.valueOf(i), data.get(bidVolumeKey).asText());
            }

            messageNode.set("askPrices", askPrices);
            messageNode.set("askVolumes", askVolumes);
            messageNode.set("bidPrices", bidPrices);
            messageNode.set("bidVolumes", bidVolumes);

            String message = objectMapper.writeValueAsString(messageNode);

            // 해당 종목을 구독 중인 세션에만 전송 (원본 종목 코드 사용)
            broadcastToSubscribers(originalStockCode, message);
        } catch (Exception e) {
            log.error("주식호가잔량 데이터 처리 중 오류 발생", e);
        }
    }

    // 특정 종목 구독자들에게만 메시지 전송 (메시지 큐 방식으로 변경)
    private void broadcastToSubscribers(String stockCode, String message) {
        sessionSubscriptions.forEach((sessionId, subscribedStocks) -> {
            if (subscribedStocks.contains(stockCode)) {
                BlockingQueue<String> queue = messageQueues.get(sessionId);
                if (queue != null) {
                    // 큐에 메시지 추가 (스레드에서 처리)
                    boolean offered = queue.offer(message);
                    if (!offered) {
                        log.warn("세션 {} 메시지 큐가 가득 찼습니다. 메시지 드롭: {}", sessionId, stockCode);
                    }
                }
            }
        });
    }

    // 구독 처리 스레드 - 애플리케이션 시작 시 실행
    @PostConstruct
    public void startSubscriptionProcessor() {
        Thread processor = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    SubscriptionTask task = subscriptionTaskQueue.poll(100, TimeUnit.MILLISECONDS);
                    if (task == null) continue;

                    subscriptionLock.lock();
                    try {
                        boolean success = false;

                        if (task.type == SubscriptionTask.TaskType.SUBSCRIBE) {
                            success = handleSubscribe(task.sessionId, task.stockCode);
                        } else if (task.type == SubscriptionTask.TaskType.UNSUBSCRIBE) {
                            success = handleUnsubscribe(task.sessionId, task.stockCode);
                        }

                        // 결과 설정
                        task.result.complete(success);
                    } finally {
                        subscriptionLock.unlock();
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    log.info("구독 처리 스레드 중단됨");
                    break;
                } catch (Exception e) {
                    log.error("구독 처리 중 예기치 않은 오류 발생", e);
                }
            }
        });
        processor.setName("subscription-processor");
        processor.setDaemon(true);
        processor.start();

        // 메시지 재시도 처리 스레드도 시작
        startRetryProcessor();
    }

    // 메시지 재시도 처리 스레드
    private void startRetryProcessor() {
        Thread retryProcessor = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    RetryMessage retryMessage = retryQueue.poll(100, TimeUnit.MILLISECONDS);
                    if (retryMessage == null) continue;

                    // 재시도 시간이 되었는지 확인
                    if (System.currentTimeMillis() < retryMessage.nextRetryTime) {
                        retryQueue.offer(retryMessage); // 아직 재시도 시간이 아니면 다시 큐에 넣음
                        Thread.sleep(10); // 짧은 딜레이
                        continue;
                    }

                    String sessionId = retryMessage.sessionId;
                    WebSocketSession session = sessions.get(sessionId);

                    if (session != null && session.isOpen()) {
                        try {
                            synchronized (session) {
                                session.sendMessage(new TextMessage(retryMessage.message));
                            }
                            log.info("메시지 재전송 성공: 세션={}, 시도={}회", sessionId, retryMessage.attempts);
                        } catch (IOException e) {
                            log.warn("메시지 재전송 실패: 세션={}, 시도={}회", sessionId, retryMessage.attempts, e);

                            // 최대 5번까지만 재시도
                            if (retryMessage.attempts < 5) {
                                retryQueue.offer(new RetryMessage(sessionId, retryMessage.message, retryMessage.attempts + 1));
                            } else {
                                log.error("최대 재시도 횟수 초과, 메시지 폐기: 세션={}", sessionId);
                            }
                        }
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    log.info("재시도 처리 스레드 중단됨");
                    break;
                } catch (Exception e) {
                    log.error("메시지 재시도 중 예기치 않은 오류 발생", e);
                }
            }
        });
        retryProcessor.setName("retry-processor");
        retryProcessor.setDaemon(true);
        retryProcessor.start();
    }

    // 구독 처리 로직
    private boolean handleSubscribe(String sessionId, String stockCode) {
        log.info("클라이언트 {} 종목 구독 요청 처리: {}", sessionId, stockCode);

        try {
            Set<String> subscribedStocks = sessionSubscriptions.get(sessionId);
            if (subscribedStocks == null) {
                log.warn("세션 {}이 더 이상 유효하지 않음", sessionId);
                return false;
            }

            if (subscribedStocks.contains(stockCode)) {
                log.info("이미 구독 중인 종목: 세션={}, 종목={}", sessionId, stockCode);
                return true; // 이미 구독 중이면 성공으로 간주
            }

            // 키움 API를 통해 종목 구독
            boolean success = kiwoomWebSocketClient.subscribeStockWithPurpose(stockCode, "WEBSOCKET_" + sessionId);

            if (success) {
                // 세션의 구독 목록에 추가
                subscribedStocks.add(stockCode);

                // 구독자 수 증가
                stockCodeSubscriberCount.merge(stockCode, 1, Integer::sum);

                log.info("종목 {} 구독 성공, 현재 구독자 수: {}", stockCode, stockCodeSubscriberCount.get(stockCode));
                return true;
            } else {
                log.error("종목 {} 구독 실패", stockCode);
                return false;
            }
        } catch (Exception e) {
            log.error("구독 처리 중 오류 발생: 세션={}, 종목={}", sessionId, stockCode, e);
            return false;
        }
    }

    // 구독 해제 처리 로직
    private boolean handleUnsubscribe(String sessionId, String stockCode) {
        log.info("클라이언트 {} 종목 구독 해제 요청 처리: {}", sessionId, stockCode);

        try {
            Set<String> subscribedStocks = sessionSubscriptions.get(sessionId);
            boolean wasSubscribed = false;

            // 세션이 이미 없는 경우도 고려
            if (subscribedStocks == null) {
                log.warn("세션 {}이 더 이상 유효하지 않음", sessionId);
                // 세션이 없어도 구독자 수는 확인해서 차감 필요
                wasSubscribed = true;
            } else {
                wasSubscribed = subscribedStocks.contains(stockCode);
                // 세션이 있고 구독 중이면 세션 구독 목록에서 제거
                if (wasSubscribed) {
                    subscribedStocks.remove(stockCode);
                }
            }

            if (wasSubscribed) {
                // 구독자 수 감소 (세션 상태와 무관하게 진행)
                stockCodeSubscriberCount.computeIfPresent(stockCode, (k, v) -> {
                    int newCount = v - 1;
                    log.info("종목 {} 구독자 수 감소: {}명 -> {}명", stockCode, v, newCount);
                    return newCount > 0 ? newCount : null;
                });

                // 키움 API를 통해 종목 구독 해제 (필요한 경우에만)
                if (stockCodeSubscriberCount.getOrDefault(stockCode, 0) == 0) {
                    boolean success = kiwoomWebSocketClient.unsubscribeStockForPurpose(stockCode, "WEBSOCKET_" + sessionId);
                    log.info("종목 {} 구독 해제 완료, 성공: {}, 남은 구독자 수: 0", stockCode, success);
                    return success;
                }

                log.info("종목 {} 구독 해제 성공, 남은 구독자 수: {}",
                        stockCode, stockCodeSubscriberCount.getOrDefault(stockCode, 0));
                return true;
            } else {
                log.info("구독 중이 아닌 종목: 세션={}, 종목={}", sessionId, stockCode);
                return true; // 구독 중이 아니면 이미 해제된 것으로 간주
            }
        } catch (Exception e) {
            log.error("구독 해제 처리 중 오류 발생: 세션={}, 종목={}", sessionId, stockCode, e);
            return false;
        }
    }

    // 주기적으로 연결이 끊어진 세션 정리
    @Scheduled(fixedRate = 60000) // 1분마다 실행
    public void cleanupDeadSessions() {
        log.debug("연결이 끊어진 세션 정리 시작");

        Set<String> deadSessions = new HashSet<>();

        // 연결이 끊어진 세션 식별
        sessions.forEach((sessionId, session) -> {
            if (!verifySessionStatus(session)) {
                deadSessions.add(sessionId);
            }
        });

        // 연결이 끊어진 세션 정리
        for (String sessionId : deadSessions) {
            log.info("연결이 끊어진 세션 정리: {}", sessionId);

            // 해당 세션의 모든 구독 해제
            Set<String> subscribedStocks = sessionSubscriptions.get(sessionId);
            if (subscribedStocks != null && !subscribedStocks.isEmpty()) {
                // 복사본 생성하여 ConcurrentModificationException 방지
                Set<String> stocksToUnsubscribe = new HashSet<>(subscribedStocks);

                for (String stockCode : stocksToUnsubscribe) {
                    try {
                        // 구독자 수 즉시 차감
                        subscriptionLock.lock();
                        try {
                            stockCodeSubscriberCount.computeIfPresent(stockCode, (k, v) -> {
                                int newCount = v - 1;
                                log.info("죽은 세션 정리로 인한 종목 {} 구독자 수 차감: {}명 -> {}명",
                                        stockCode, v, newCount > 0 ? newCount : 0);
                                return newCount > 0 ? newCount : null;
                            });
                        } finally {
                            subscriptionLock.unlock();
                        }

                        // 구독 해제 처리
                        kiwoomWebSocketClient.unsubscribeStockForPurpose(stockCode, "WEBSOCKET_" + sessionId);
                        log.info("죽은 세션 정리로 인한 종목 {} 구독 해제 완료", stockCode);

                        // 세션 구독 목록에서 제거
                        subscribedStocks.remove(stockCode);
                    } catch (Exception e) {
                        log.error("죽은 세션 정리 중 종목 {} 구독 해제 오류 발생", stockCode, e);
                    }
                }
            }

            // 메시지 처리 스레드 중지
            Thread worker = sessionWorkers.remove(sessionId);
            if (worker != null) {
                worker.interrupt();
            }

            // 메시지 큐 제거
            messageQueues.remove(sessionId);

            // 세션 정보 제거
            sessions.remove(sessionId);
            sessionSubscriptions.remove(sessionId);

            log.info("죽은 세션 {} 정리 완료", sessionId);
        }

        // 모든 세션이 종료되면 리스너 제거
        if (sessions.isEmpty() && !deadSessions.isEmpty()) {
            kiwoomWebSocketClient.removeListener(this);
            log.info("모든 세션 종료됨, 리스너 제거");
        }

        if (!deadSessions.isEmpty()) {
            log.info("연결이 끊어진 세션 정리 완료: {} 세션 제거됨", deadSessions.size());
        } else {
            log.debug("연결이 끊어진 세션 정리 완료: 제거할 세션 없음");
        }
    }

    // 체결 정보 전송 메서드
    public void broadcastTradeExecution(String stockCode, String tradeType, int quantity,
                                        Long price, Long totalAmount, String timestamp) {
        try {
            ObjectNode messageNode = objectMapper.createObjectNode();
            messageNode.put("type", "tradeExecution");
            messageNode.put("stockCode", stockCode);
            messageNode.put("tradeType", tradeType);  // "BUY" 또는 "SELL"
            messageNode.put("quantity", quantity);
            messageNode.put("price", price);
            messageNode.put("totalAmount", totalAmount);
            messageNode.put("timestamp", timestamp);

            String message = objectMapper.writeValueAsString(messageNode);

            // 해당 종목을 구독 중인 세션에만 전송
            broadcastToSubscribers(stockCode, message);

        } catch (Exception e) {
            log.error("체결 정보 메시지 생성 중 오류 발생", e);
        }
    }

}