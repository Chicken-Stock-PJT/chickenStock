package realClassOne.chickenStock.stock.websocket.client;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import jakarta.annotation.PreDestroy;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import jakarta.annotation.PostConstruct;
import realClassOne.chickenStock.stock.service.KiwoomAuthService;
import realClassOne.chickenStock.stock.websocket.handler.StockWebSocketHandler;

import java.net.URI;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantReadWriteLock;

@Component
@Slf4j
@RequiredArgsConstructor
public class KiwoomWebSocketClient {

    @Value("${kiwoom.websocket.url}")
    private String websocketUrl;

    private final KiwoomAuthService authService;
    private final ObjectMapper objectMapper;
    private final List<StockDataListener> listeners = new CopyOnWriteArrayList<>();
    private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();

    private WebSocketClient client;
    private final AtomicBoolean connected = new AtomicBoolean(false);
    private final AtomicBoolean reconnecting = new AtomicBoolean(false);
    private final AtomicBoolean tokenRefreshing = new AtomicBoolean(false);

    // 구독된 종목 코드 관리 - 동시성 안전한 컬렉션 사용
    private final Set<String> subscribedStockCodes = ConcurrentHashMap.newKeySet();

    // 종목별 구독자 수 관리
    private final Map<String, Integer> stockCodeSubscriberCount = new ConcurrentHashMap<>();

    // 종목별 최신 가격 데이터 캐시
    private final Map<String, JsonNode> latestPriceDataCache = new ConcurrentHashMap<>();

    // 목적에 따른 구독 진행 (세션별 구독 관리)
    private final Map<String, Set<String>> stockSubscriptionPurposes = new ConcurrentHashMap<>();
    private final ReentrantReadWriteLock subscriptionLock = new ReentrantReadWriteLock();

    // 메시지 송신 큐
    private final BlockingQueue<String> messageQueue = new LinkedBlockingQueue<>();

    // 재연결 시도 카운터
    private int reconnectAttempts = 0;

    // API 호출 타임아웃 설정
    private static final int API_TIMEOUT_SECONDS = 10;

    // 종목 코드 변환 및 정규화 유틸리티
    private final Map<String, String> normalizedCodeCache = new ConcurrentHashMap<>();

    // 인터페이스 정의
    public interface StockDataListener {
        void onStockPriceUpdate(String stockCode, JsonNode data);

        void onStockBidAskUpdate(String stockCode, JsonNode data);
    }

    public void addListener(StockDataListener listener) {
        listeners.add(listener);
        log.info("리스너 추가: 현재 리스너 수={}", listeners.size());
    }

    public void removeListener(StockDataListener listener) {
        listeners.remove(listener);
        log.info("리스너 제거: 현재 리스너 수={}", listeners.size());
    }

    // 최신 가격 데이터 반환 메서드
    public JsonNode getLatestStockPriceData(String stockCode) {
        if (stockCode == null) return null;

        String normalized = normalizeStockCode(stockCode);

        // 정규화된 코드로 조회
        JsonNode data = latestPriceDataCache.get(normalized);
        if (data != null) {
            return data;
        }

        // 원본 코드로 조회
        data = latestPriceDataCache.get(stockCode);
        if (data != null) {
            return data;
        }

        // _AL 버전으로 조회
        String alVersion = stockCode + "_AL";
        data = latestPriceDataCache.get(alVersion);
        if (data != null) {
            return data;
        }

        // _AL 버전 제거 조회
        if (stockCode.endsWith("_AL")) {
            String withoutAl = stockCode.replace("_AL", "");
            return latestPriceDataCache.get(withoutAl);
        }

        return null;
    }

    // 종목 코드 정규화 (캐싱 적용)
    private String normalizeStockCode(String stockCode) {
        if (stockCode == null) return null;

        return normalizedCodeCache.computeIfAbsent(stockCode, code -> {
            String trimmed = code.trim();
            if (trimmed.endsWith("_AL")) {
                return trimmed;
            }
            return trimmed;
        });
    }

    // 실시간 데이터 처리 및 캐싱
    public void processStockPriceUpdate(String stockCode, JsonNode data) {
        try {
            // 코드 정규화
            String normalizedCode = normalizeStockCode(stockCode);

            // 로그 수준을 DEBUG로 조정하여 성능 향상
            if (log.isDebugEnabled()) {
                log.debug("[실시간가격] 종목: {}, 현재가: {}, 등락률: {}%",
                        normalizedCode,
                        data.get("10").asText(),
                        data.get("12").asText());
            }

            // 데이터 캐싱 (여러 코드 형태 지원)
            latestPriceDataCache.put(normalizedCode, data);
            if (normalizedCode.endsWith("_AL")) {
                latestPriceDataCache.put(normalizedCode.replace("_AL", ""), data);
            } else {
                latestPriceDataCache.put(normalizedCode + "_AL", data);
            }

            // 데이터 처리 - 별도 스레드 생성하지 않고 직접 처리
            notifyStockPriceUpdate(normalizedCode, data);
        } catch (Exception e) {
            log.error("실시간 데이터 처리 중 오류 발생: {}", stockCode, e);
        }
    }

    @PostConstruct
    public void init() {
        connect();

        // 메시지 처리 스레드 시작
        startMessageProcessingThread();

        // 주기적인 연결 상태 확인 스케줄링
        scheduler.scheduleAtFixedRate(this::checkConnectionStatus, 30, 30, TimeUnit.SECONDS);

        // 주기적인 토큰 갱신 스케줄링
        scheduler.scheduleAtFixedRate(this::refreshTokenIfNeeded, 1, 1, TimeUnit.HOURS);
    }

    private void startMessageProcessingThread() {
        Thread messageProcessor = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    String message = messageQueue.poll(100, TimeUnit.MILLISECONDS);
                    if (message == null) continue;

                    if (isConnected()) {
                        client.send(message);
                    } else {
                        // 연결이 끊어진 경우 메시지 다시 큐에 넣음 (최대 100개 메시지까지만)
                        if (messageQueue.size() < 100) {
                            messageQueue.offer(message);
                        }

                        // 연결이 끊어진 상태라면 재연결 시도
                        if (!reconnecting.get()) {
                            reconnect();
                        }

                        // 잠시 대기
                        Thread.sleep(500);
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    log.info("메시지 처리 스레드 중단됨");
                    break;
                } catch (Exception e) {
                    log.error("메시지 처리 중 오류 발생", e);
                }
            }
        });
        messageProcessor.setName("kiwoom-message-processor");
        messageProcessor.setDaemon(true);
        messageProcessor.start();
    }

    // 연결 상태 확인 및 필요시 재연결
    private void checkConnectionStatus() {
        if (!isConnected() && !reconnecting.get()) {
            log.warn("연결 상태 확인: 연결이 끊어져 있음, 재연결 시도");
            reconnect();
        }
    }

    // 토큰 갱신 필요 시 갱신
    private void refreshTokenIfNeeded() {
        try {
            if (tokenRefreshing.compareAndSet(false, true)) {
                try {
                    authService.fetchNewAccessToken();

                    // 토큰 갱신 후 재로그인 필요
                    if (isConnected()) {
                        login();
                    }
                } finally {
                    tokenRefreshing.set(false);
                }
            }
        } catch (Exception e) {
            log.error("토큰 갱신 중 오류 발생", e);
        }
    }

    public void connect() {
        try {
            if (isConnected()) {
                log.info("WebSocket 이미 연결 중입니다.");
                return;
            }

            if (reconnecting.compareAndSet(false, true)) {
                try {
                    log.info("키움증권 WebSocket 서버 연결 시도 중...");

                    client = new WebSocketClient(new URI(websocketUrl)) {
                        @Override
                        public void onOpen(ServerHandshake handshake) {
                            log.info("키움증권 WebSocket 서버 연결 성공");
                            connected.set(true);
                            reconnectAttempts = 0;
                            login();
                        }

                        @Override
                        public void onMessage(String message) {
                            try {
                                JsonNode response = objectMapper.readTree(message);
                                String trnm = response.has("trnm") ? response.get("trnm").asText() : "";

                                if ("LOGIN".equals(trnm)) {
                                    if ("0".equals(response.get("return_code").asText())) {
                                        log.info("키움증권 WebSocket 로그인 성공");

                                        // 로그인 성공 후 기존 구독된 종목 재등록
                                        if (!subscribedStockCodes.isEmpty()) {
                                            reregisterAllStocks();
                                        }
                                    } else {
                                        log.error("키움증권 WebSocket 로그인 실패: {}", response.get("return_msg").asText());
                                        reconnect();
                                    }
                                } else if ("PING".equals(trnm)) {
                                    // PING 메시지에 그대로 응답
                                    messageQueue.offer(message);
                                } else if ("REAL".equals(trnm)) {
                                    processRealTimeData(response);
                                }

                                if (!"PING".equals(trnm) && log.isDebugEnabled()) {
                                    log.debug("키움증권 WebSocket 메시지 수신: {}", message);
                                }
                            } catch (Exception e) {
                                log.error("WebSocket 메시지 처리 중 오류 발생", e);
                            }
                        }

                        @Override
                        public void onClose(int code, String reason, boolean remote) {
                            log.info("키움증권 WebSocket 연결 종료: code={}, reason={}, remote={}", code, reason, remote);
                            connected.set(false);
                            reconnect();
                        }

                        @Override
                        public void onError(Exception ex) {
                            log.error("키움증권 WebSocket 오류 발생", ex);
                            connected.set(false);
                            reconnect();
                        }
                    };

                    client.connect();
                } finally {
                    reconnecting.set(false);
                }
            }
        } catch (Exception e) {
            log.error("키움증권 WebSocket 연결 시도 중 오류 발생", e);
            connected.set(false);
            reconnect();
        }
    }

    private void login() {
        try {
            ObjectNode loginMessage = objectMapper.createObjectNode();
            loginMessage.put("trnm", "LOGIN");
            loginMessage.put("token", authService.getAccessToken());

            String loginMessageStr = objectMapper.writeValueAsString(loginMessage);
            messageQueue.offer(loginMessageStr);
            log.info("키움증권 WebSocket 로그인 요청 전송");
        } catch (Exception e) {
            log.error("키움증권 WebSocket 로그인 요청 전송 중 오류 발생", e);
        }
    }

    // 종목 구독 메서드 - 동시성 보장
    public boolean subscribeStock(String stockCode) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
            log.error("유효하지 않은 종목 코드: {}", stockCode);
            return false;
        }

        String normalizedCode = normalizeStockCode(stockCode);

        subscriptionLock.writeLock().lock();
        try {
            // 구독자 수 증가
            int count = stockCodeSubscriberCount.getOrDefault(normalizedCode, 0) + 1;
            stockCodeSubscriberCount.put(normalizedCode, count);

            // 새로운 종목 구독인 경우 실시간 데이터 등록
            if (count == 1) {
                subscribedStockCodes.add(normalizedCode);

                if (isConnected()) {
                    // 실시간 데이터 등록 요청
                    CompletableFuture<Boolean> future = new CompletableFuture<>();

                    // 비동기 등록 요청 (메시지 큐 사용)
                    registerRealTimeDataAsync("0B", Collections.singletonList(normalizedCode), future);
                    registerRealTimeDataAsync("0D", Collections.singletonList(normalizedCode), null);

                    try {
                        // 타임아웃 설정하여 결과 대기
                        boolean success = future.get(API_TIMEOUT_SECONDS, TimeUnit.SECONDS);
                        if (success) {
                            log.info("종목 구독 성공: {}", normalizedCode);
                            return true;
                        } else {
                            // 실패 시 구독 상태 롤백
                            subscribedStockCodes.remove(normalizedCode);
                            stockCodeSubscriberCount.remove(normalizedCode);
                            log.error("종목 구독 실패: {}", normalizedCode);
                            return false;
                        }
                    } catch (TimeoutException e) {
                        log.error("종목 구독 타임아웃: {}", normalizedCode);
                        // 타임아웃 발생 시도 성공으로 처리 (나중에 재연결 시 재등록됨)
                        return true;
                    } catch (Exception e) {
                        log.error("종목 구독 중 오류 발생: {}", normalizedCode, e);
                        // 오류 발생해도 내부 상태는 구독 중으로 유지 (재연결 시 재등록됨)
                        return true;
                    }
                } else {
                    log.warn("WebSocket 연결이 없어 나중에 등록됩니다: {}", normalizedCode);
                    // 연결이 없어도 내부 상태는 구독 중으로 유지 (연결 시 등록됨)
                    return true;
                }
            } else {
                log.info("종목 구독자 수 증가: {} ({}명)", normalizedCode, count);
                return true;
            }
        } finally {
            subscriptionLock.writeLock().unlock();
        }
    }

    // 비동기 실시간 데이터 등록 요청
    private void registerRealTimeDataAsync(String type, List<String> stockCodes, CompletableFuture<Boolean> future) {
        try {
            if (stockCodes == null || stockCodes.isEmpty()) {
                if (future != null) future.complete(false);
                return;
            }

            ObjectNode registerMessage = objectMapper.createObjectNode();
            registerMessage.put("trnm", "REG");
            registerMessage.put("grp_no", "1");
            registerMessage.put("refresh", "1");

            ArrayNode dataArray = objectMapper.createArrayNode();
            ObjectNode dataObject = objectMapper.createObjectNode();

            ArrayNode itemArray = objectMapper.createArrayNode();
            stockCodes.forEach(code -> {
                String formattedCode = convertStockCode(code);
                itemArray.add(formattedCode);
            });

            ArrayNode typeArray = objectMapper.createArrayNode();
            typeArray.add(type);

            dataObject.set("item", itemArray);
            dataObject.set("type", typeArray);
            dataArray.add(dataObject);

            registerMessage.set("data", dataArray);

            String registerMessageStr = objectMapper.writeValueAsString(registerMessage);
            messageQueue.offer(registerMessageStr);

            log.info("실시간 데이터 등록 요청 전송: type={}, stocks={}", type, stockCodes);

            // 현재는 간단하게 항상 성공으로 처리 (실제로는 응답 처리 필요)
            if (future != null) {
                // 실제 구현에서는 응답 매핑 필요
                future.complete(true);
            }
        } catch (Exception e) {
            log.error("실시간 데이터 등록 요청 생성 중 오류 발생", e);
            if (future != null) {
                future.complete(false);
            }
        }
    }

    // 종목 구독 해제 메서드
    public boolean unsubscribeStock(String stockCode) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
            log.error("유효하지 않은 종목 코드: {}", stockCode);
            return false;
        }

        String normalizedCode = normalizeStockCode(stockCode);

        subscriptionLock.writeLock().lock();
        try {
            if (!stockCodeSubscriberCount.containsKey(normalizedCode)) {
                log.warn("구독되지 않은 종목 해제 요청: {}", normalizedCode);
                return true; // 이미 구독되지 않은 상태이므로 성공으로 간주
            }

            int count = stockCodeSubscriberCount.getOrDefault(normalizedCode, 0) - 1;

            if (count <= 0) {
                stockCodeSubscriberCount.remove(normalizedCode);
                subscribedStockCodes.remove(normalizedCode);

                if (isConnected()) {
                    // 실시간 데이터 구독 해제 요청
                    CompletableFuture<Boolean> future = new CompletableFuture<>();

                    // 비동기 구독 해제 요청 (메시지 큐 사용)
                    unregisterRealTimeDataAsync("0B", Collections.singletonList(normalizedCode), future);
                    unregisterRealTimeDataAsync("0D", Collections.singletonList(normalizedCode), null);

                    try {
                        // 타임아웃 설정하여 결과 대기
                        boolean success = future.get(API_TIMEOUT_SECONDS, TimeUnit.SECONDS);
                        log.info("종목 {} 구독 해제 {}: {}", normalizedCode, success ? "성공" : "실패", normalizedCode);

                        // 모든 구독이 해제되었는지 확인하고 필요시 연결 종료
                        disconnectIfNoSubscriptions();

                        return success;
                    } catch (TimeoutException e) {
                        log.error("종목 {} 구독 해제 타임아웃: {}", normalizedCode);
                        // 타임아웃 발생해도 내부 상태는 해제됨

                        // 모든 구독이 해제되었는지 확인하고 필요시 연결 종료
                        disconnectIfNoSubscriptions();

                        return true;
                    } catch (Exception e) {
                        log.error("종목 {} 구독 해제 중 오류 발생: {}", normalizedCode, e);
                        // 오류 발생해도 내부 상태는 해제됨

                        // 모든 구독이 해제되었는지 확인하고 필요시 연결 종료
                        disconnectIfNoSubscriptions();

                        return true;
                    }
                } else {
                    log.warn("WebSocket 연결이 없어 구독 해제 요청을 보내지 못함: {}", normalizedCode);
                    // 연결이 없어도 내부 상태는 해제됨

                    // 모든 구독이 해제되었는지 확인하고 필요시 연결 종료
                    disconnectIfNoSubscriptions();

                    return true;
                }
            } else {
                stockCodeSubscriberCount.put(normalizedCode, count);
                log.info("종목 구독자 수 감소: {} ({}명)", normalizedCode, count);
                return true;
            }
        } finally {
            subscriptionLock.writeLock().unlock();
        }
    }

    // 비동기 실시간 데이터 구독 해제 요청
    private void unregisterRealTimeDataAsync(String type, List<String> stockCodes, CompletableFuture<Boolean> future) {
        try {
            if (stockCodes == null || stockCodes.isEmpty()) {
                if (future != null) future.complete(false);
                return;
            }

            ObjectNode unregisterMessage = objectMapper.createObjectNode();
            unregisterMessage.put("trnm", "REMOVE");
            unregisterMessage.put("grp_no", "1");

            ArrayNode dataArray = objectMapper.createArrayNode();
            ObjectNode dataObject = objectMapper.createObjectNode();

            ArrayNode itemArray = objectMapper.createArrayNode();
            stockCodes.forEach(code -> {
                String formattedCode = convertStockCode(code);
                itemArray.add(formattedCode);
            });

            ArrayNode typeArray = objectMapper.createArrayNode();
            typeArray.add(type);

            dataObject.set("item", itemArray);
            dataObject.set("type", typeArray);
            dataArray.add(dataObject);

            unregisterMessage.set("data", dataArray);

            String unregisterMessageStr = objectMapper.writeValueAsString(unregisterMessage);
            messageQueue.offer(unregisterMessageStr);

            log.info("실시간 데이터 해제 요청 전송: type={}, stocks={}", type, stockCodes);

            // 현재는 간단하게 항상 성공으로 처리 (실제로는 응답 처리 필요)
            if (future != null) {
                // 실제 구현에서는 응답 매핑 필요
                future.complete(true);
            }
        } catch (Exception e) {
            log.error("실시간 데이터 해제 요청 생성 중 오류 발생", e);
            if (future != null) {
                future.complete(false);
            }
        }
    }

    // 종목 코드 변환 (_AL 접미사 추가)
    private String convertStockCode(String stockCode) {
        if (stockCode == null) return null;

        String normalized = normalizeStockCode(stockCode);
        if (normalized.endsWith("_AL")) {
            return normalized;
        }
        return normalized + "_AL";
    }

    // 연결 재시작시 모든 구독 종목 재등록
    private void reregisterAllStocks() {
        subscriptionLock.readLock().lock();
        try {
            if (subscribedStockCodes.isEmpty()) {
                return;
            }

            List<String> stockCodes = new ArrayList<>(subscribedStockCodes);
            log.info("기존 구독 종목 재등록: {} 종목", stockCodes.size());

            // 최대 100개씩 나누어 등록 (API 한계 고려)
            int batchSize = 100;
            for (int i = 0; i < stockCodes.size(); i += batchSize) {
                List<String> batch = stockCodes.subList(i, Math.min(i + batchSize, stockCodes.size()));
                registerRealTimeDataAsync("0B", batch, null); // 주식체결
                registerRealTimeDataAsync("0D", batch, null); // 주식호가잔량
            }
        } finally {
            subscriptionLock.readLock().unlock();
        }
    }

    // 포트폴리오용 연결 재시작시 구독 종목 재등록 (주식체결만 구독)
    public void reregisterPortfolioStocks(String purpose) {
        subscriptionLock.readLock().lock();
        try {
            // 해당 목적에 맞는 종목만 필터링
            Set<String> stockCodes = getStocksWithPurpose(purpose);

            if (stockCodes.isEmpty()) {
                log.info("목적 '{}' 에 해당하는 구독 종목이 없습니다.", purpose);
                return;
            }

            List<String> stockCodesList = new ArrayList<>(stockCodes);
            log.info("포트폴리오 구독 종목 재등록: {} 종목", stockCodesList.size());

            // 최대 100개씩 나누어 등록 (API 한계 고려)
            int batchSize = 100;
            for (int i = 0; i < stockCodesList.size(); i += batchSize) {
                List<String> batch = stockCodesList.subList(i, Math.min(i + batchSize, stockCodesList.size()));
                registerRealTimeDataAsync("0B", batch, null); // 주식체결만 구독
            }
        } finally {
            subscriptionLock.readLock().unlock();
        }
    }

    // 데이터 처리 메서드 (스레드 생성 제거)
    private void processRealTimeData(JsonNode response) {
        try {
            JsonNode dataArray = response.get("data");
            if (dataArray != null && dataArray.isArray()) {
                for (JsonNode dataItem : dataArray) {
                    String type = dataItem.get("type").asText();
                    String stockCode = dataItem.get("item").asText();
                    JsonNode values = dataItem.get("values");

                    // 종목 코드 정규화
                    String normalizedCode = normalizeStockCode(stockCode);

                    // 데이터 유형에 따른 처리
                    if ("0B".equals(type)) {
                        // 두 가지 형태 모두 캐시에 저장 (원본 코드와 _AL 붙은 코드)
                        if (normalizedCode.endsWith("_AL")) {
                            latestPriceDataCache.put(normalizedCode, values);
                            latestPriceDataCache.put(normalizedCode.replace("_AL", ""), values);
                        } else {
                            latestPriceDataCache.put(normalizedCode, values);
                            latestPriceDataCache.put(normalizedCode + "_AL", values);
                        }

                        // 주식체결 데이터 처리 - 별도 스레드 생성하지 않고 직접 처리
                        notifyStockPriceUpdate(normalizedCode, values);
                    } else if ("0D".equals(type)) {
                        // 주식호가잔량 데이터 처리
                        notifyStockBidAskUpdate(normalizedCode, values);
                    }
                }
            }
        } catch (Exception e) {
            log.error("실시간 데이터 처리 중 오류 발생", e);
        }
    }

    // 리스너 알림 메서드 (별도 스레드 생성 없이 직접 처리)
    private void notifyStockPriceUpdate(String stockCode, JsonNode data) {
        for (StockDataListener listener : listeners) {
            try {
                listener.onStockPriceUpdate(stockCode, data);
            } catch (Exception e) {
                log.error("리스너 알림 중 오류 발생 (주식체결): {}", e.getMessage());
            }
        }
    }

    private void notifyStockBidAskUpdate(String stockCode, JsonNode data) {
        for (StockDataListener listener : listeners) {
            try {
                listener.onStockBidAskUpdate(stockCode, data);
            } catch (Exception e) {
                log.error("리스너 알림 중 오류 발생 (주식호가): {}", e.getMessage());
            }
        }
    }

    // 지수 백오프 방식의 재연결 메서드
    private void reconnect() {
        if (reconnecting.compareAndSet(false, true)) {
            try {
                if (client != null) {
                    try {
                        client.close();
                    } catch (Exception e) {
                        log.warn("WebSocket 닫기 중 오류 발생", e);
                    }
                }

                connected.set(false);

                // 지수 백오프 (최대 60초)
                int delay = Math.min(60, (int) Math.pow(2, reconnectAttempts));
                reconnectAttempts++;

                log.info("키움증권 WebSocket {}초 후 재연결 시도 (시도 횟수: {})", delay, reconnectAttempts);

                // 별도 스레드에서 지연 후 재연결
                scheduler.schedule(() -> {
                    try {
                        log.info("재연결 시도 중...");
                        connect();
                    } finally {
                        reconnecting.set(false);
                    }
                }, delay, TimeUnit.SECONDS);
            } catch (Exception e) {
                log.error("재연결 스케줄링 중 오류 발생", e);
                reconnecting.set(false);
            }
        }
    }

    // 목적을 지정하여 구독하는 메서드
    public boolean subscribeStockWithPurpose(String stockCode, String purpose) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
            log.error("유효하지 않은 종목 코드: {}", stockCode);
            return false;
        }

        String normalizedCode = normalizeStockCode(stockCode);

        subscriptionLock.writeLock().lock();
        try {
            // 목적 추가
            Set<String> purposes = stockSubscriptionPurposes.computeIfAbsent(normalizedCode, k -> ConcurrentHashMap.newKeySet());
            purposes.add(purpose);

            log.info("종목 {} 구독 목적 추가: {}, 총 목적 수: {}", normalizedCode, purpose, purposes.size());

            // 실제 구독 처리
            return subscribeStock(normalizedCode);
        } finally {
            subscriptionLock.writeLock().unlock();
        }
    }

    // 특정 목적의 구독을 해제하는 메서드
    public boolean unsubscribeStockForPurpose(String stockCode, String purpose) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
            log.error("유효하지 않은 종목 코드: {}", stockCode);
            return false;
        }

        String normalizedCode = normalizeStockCode(stockCode);

        subscriptionLock.writeLock().lock();
        try {
            // 목적 제거
            Set<String> purposes = stockSubscriptionPurposes.get(normalizedCode);
            if (purposes == null) {
                log.warn("종목 {} 구독 목적 목록이 없음", normalizedCode);
                return true; // 이미 구독되지 않은 상태이므로 성공으로 간주
            }

            boolean removed = purposes.remove(purpose);
            log.info("종목 {} 구독 목적 제거: {}, 성공: {}, 남은 목적 수: {}",
                    normalizedCode, purpose, removed, purposes.size());

            if (!removed) {
                log.warn("종목 {} 구독 목적 {} 제거 실패: 해당 목적이 존재하지 않음", normalizedCode, purpose);
                return true; // 이미 해당 목적으로 구독되지 않았으므로 성공으로 간주
            }

            // 모든 목적이 제거되면 실제 구독 해제
            if (purposes.isEmpty()) {
                stockSubscriptionPurposes.remove(normalizedCode);
                log.info("종목 {} 모든 구독 목적 제거됨, 구독 해제", normalizedCode);

                // 이미 구독자 수가 0이 아니면 차감
                if (stockCodeSubscriberCount.containsKey(normalizedCode)) {
                    int currentCount = stockCodeSubscriberCount.get(normalizedCode);
                    if (currentCount > 0) {
                        log.warn("종목 {} 목적이 모두 제거되었으나 구독자 수가 {}명 남았습니다. 구독자 수를 0으로 조정합니다.",
                                normalizedCode, currentCount);
                        stockCodeSubscriberCount.remove(normalizedCode);
                    }
                }

                return unsubscribeStock(normalizedCode);
                // unsubscribeStock 내부에서 disconnectIfNoSubscriptions()를 호출하므로 여기서는 호출하지 않음
            }

            // 다른 목적이 남아있으면 구독 유지, 성공 반환
            return true;
        } finally {
            subscriptionLock.writeLock().unlock();
        }
    }

    // WebSocket 연결 상태 확인 (동시성 안전)
    public boolean isConnected() {
        return connected.get() && client != null && client.isOpen();
    }

    // 현재 구독중인 종목 목록 조회 (읽기 전용 뷰 반환)
    public Set<String> getSubscribedStockCodes() {
        subscriptionLock.readLock().lock();
        try {
            return Collections.unmodifiableSet(new HashSet<>(subscribedStockCodes));
        } finally {
            subscriptionLock.readLock().unlock();
        }
    }

    // 특정 종목이 구독 중인지 확인
    public boolean isSubscribed(String stockCode) {
        if (stockCode == null) return false;

        String normalizedCode = normalizeStockCode(stockCode);

        subscriptionLock.readLock().lock();
        try {
            return subscribedStockCodes.contains(normalizedCode) ||
                    subscribedStockCodes.contains(normalizedCode + "_AL") ||
                    (normalizedCode.endsWith("_AL") && subscribedStockCodes.contains(normalizedCode.replace("_AL", "")));
        } finally {
            subscriptionLock.readLock().unlock();
        }
    }

    // 구독자 수 조회
    public int getSubscriberCount(String stockCode) {
        if (stockCode == null) return 0;

        String normalizedCode = normalizeStockCode(stockCode);

        subscriptionLock.readLock().lock();
        try {
            return stockCodeSubscriberCount.getOrDefault(normalizedCode, 0);
        } finally {
            subscriptionLock.readLock().unlock();
        }
    }

    // 특정 목적의 구독 여부 확인
    public boolean hasSubscriptionPurpose(String stockCode, String purposePrefix) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
            return false;
        }

        String normalizedCode = normalizeStockCode(stockCode);

        subscriptionLock.readLock().lock();
        try {
            Set<String> purposes = stockSubscriptionPurposes.get(normalizedCode);
            if (purposes == null || purposes.isEmpty()) {
                return false;
            }

            // 특정 접두사로 시작하는 목적이 있는지 확인
            return purposes.stream().anyMatch(p -> p.startsWith(purposePrefix));
        } finally {
            subscriptionLock.readLock().unlock();
        }
    }

    // 특정 목적으로 구독 중인 종목들 조회
    public Set<String> getStocksWithPurpose(String purposePrefix) {
        subscriptionLock.readLock().lock();
        try {
            Set<String> result = new HashSet<>();

            for (Map.Entry<String, Set<String>> entry : stockSubscriptionPurposes.entrySet()) {
                String stockCode = entry.getKey();
                Set<String> purposes = entry.getValue();

                if (purposes.stream().anyMatch(p -> p.startsWith(purposePrefix))) {
                    result.add(stockCode);
                }
            }

            return Collections.unmodifiableSet(result);
        } finally {
            subscriptionLock.readLock().unlock();
        }
    }

    // 모든 구독 목적 조회
    public Set<String> getSubscriptionPurposes(String stockCode) {
        if (stockCode == null) return Collections.emptySet();

        String normalizedCode = normalizeStockCode(stockCode);

        subscriptionLock.readLock().lock();
        try {
            Set<String> purposes = stockSubscriptionPurposes.get(normalizedCode);
            return purposes != null ? Collections.unmodifiableSet(new HashSet<>(purposes)) : Collections.emptySet();
        } finally {
            subscriptionLock.readLock().unlock();
        }
    }

    // 주기적으로 연결 상태 모니터링 및 구독 상태 점검
    @Scheduled(fixedRate = 300000) // 5분마다 실행
    public void monitorSubscriptionStatus() {
        if (!isConnected()) {
            log.warn("연결이 끊어진 상태에서 구독 상태 점검 불가");
            return;
        }

        subscriptionLock.readLock().lock();
        try {
            if (subscribedStockCodes.isEmpty()) {
                return;
            }

            log.info("구독 상태 점검 중... 현재 {}개 종목 구독 중", subscribedStockCodes.size());

            // 샘플링으로 구독 상태 확인 (전체 종목의 10%만 선택)
            int sampleSize = Math.max(1, subscribedStockCodes.size() / 10);
            List<String> allStocks = new ArrayList<>(subscribedStockCodes);
            Collections.shuffle(allStocks);

            List<String> sampledStocks = allStocks.subList(0, Math.min(sampleSize, allStocks.size()));

            for (String stockCode : sampledStocks) {
                // 여기서 실제 구독 상태 확인 로직 구현 (필요시)
                JsonNode cachedData = getLatestStockPriceData(stockCode);
                if (cachedData == null) {
                    log.warn("종목 {} 구독 중이지만 최신 데이터 없음, 재구독 필요 가능성", stockCode);
                }
            }

            // 구독 상태 점검 후 연결 상태 확인
            disconnectIfNoSubscriptions();
        } finally {
            subscriptionLock.readLock().unlock();
        }
    }

    // 애플리케이션 종료 시 정리 작업
    @PreDestroy
    public void shutdown() {
        try {
            log.info("KiwoomWebSocketClient 종료 중...");

            // 스케줄러 종료
            scheduler.shutdown();
            try {
                if (!scheduler.awaitTermination(5, TimeUnit.SECONDS)) {
                    scheduler.shutdownNow();
                }
            } catch (InterruptedException e) {
                scheduler.shutdownNow();
                Thread.currentThread().interrupt();
            }

            // WebSocket 연결 종료
            if (client != null) {
                client.close();
            }

            log.info("KiwoomWebSocketClient 종료 완료");
        } catch (Exception e) {
            log.error("KiwoomWebSocketClient 종료 중 오류 발생", e);
        }
    }

    // 모든 구독이 해제되었을 때 서버 연결을 끊는 메서드
    private void disconnectIfNoSubscriptions() {
        subscriptionLock.readLock().lock();
        try {
            // 구독 중인 종목이 없는지 확인
            if (subscribedStockCodes.isEmpty()) {
                log.info("구독 중인 종목이 없음. 리스너 수: {}", listeners.size());

                // 추가: 필요한 리스너만 확인하도록 수정
                // StockWebSocketHandler에서 등록한 리스너인지 확인
                boolean hasWebSocketListener = false;
                for (StockDataListener listener : listeners) {
                    if (listener instanceof StockWebSocketHandler) {
                        hasWebSocketListener = true;
                        break;
                    }
                }

                // 웹소켓 리스너가 없거나 리스너가 1개만 있고
                // 그것이 기본 시스템 리스너라면 연결 종료 가능
                if (!hasWebSocketListener &&
                        (listeners.isEmpty() || (listeners.size() == 1 && isSystemListener(listeners.get(0))))) {
                    if (client != null && isConnected()) {
                        try {
                            log.info("웹소켓 리스너가 없고 구독 중인 종목이 없어 키움증권 WebSocket 연결을 종료합니다.");
                            client.close();
                            connected.set(false);
                            log.info("키움증권 WebSocket 연결 종료 완료");
                        } catch (Exception e) {
                            log.error("키움증권 WebSocket 연결 종료 중 오류 발생", e);
                        }
                    }
                } else {
                    if (hasWebSocketListener) {
                        log.info("웹소켓 리스너가 아직 존재하여 연결을 유지합니다.");
                    } else {
                        log.info("시스템 내부 리스너가 존재하여 연결을 유지합니다. 리스너 수: {}", listeners.size());
                    }
                }
            }
        } finally {
            subscriptionLock.readLock().unlock();
        }
    }

    // 시스템 기본 리스너인지 확인하는 헬퍼 메서드
    private boolean isSystemListener(StockDataListener listener) {
        // 시스템에 기본으로 등록되는 리스너 클래스 이름 확인
        String className = listener.getClass().getName();
        return className.contains("StockInfoService") ||
                className.contains("StockMonitorService") ||
                className.contains("KiwoomDataProcessor");
    }

    // 주기적으로 구독자 수와 목적을 확인하고 불일치를 수정
    @Scheduled(fixedRate = 600000) // 10분마다 실행
    public void checkSubscriptionCounts() {
        log.info("구독자 수 상태 점검 시작");

        subscriptionLock.writeLock().lock();
        try {
            // 종목별 구독 목적과 구독자 수 불일치 확인
            Set<String> mismatches = new HashSet<>();

            for (String stockCode : subscribedStockCodes) {
                Set<String> purposes = stockSubscriptionPurposes.get(stockCode);
                int subscriberCount = stockCodeSubscriberCount.getOrDefault(stockCode, 0);

                // 구독자 수는 있는데 목적이 없는 경우
                if ((purposes == null || purposes.isEmpty()) && subscriberCount > 0) {
                    log.warn("종목 {} 구독 불일치: 목적은 없으나 구독자 수 {}명", stockCode, subscriberCount);
                    mismatches.add(stockCode);
                }

                // 구독 목적은 있는데 구독자 수가 없는 경우
                if (purposes != null && !purposes.isEmpty() && subscriberCount <= 0) {
                    log.warn("종목 {} 구독 불일치: 목적 {}개 있으나 구독자 수 0명", stockCode, purposes.size());
                    mismatches.add(stockCode);

                    // 구독자 수 갱신
                    stockCodeSubscriberCount.put(stockCode, purposes.size());
                    log.info("종목 {} 구독자 수 자동 조정: {}명", stockCode, purposes.size());
                }
            }

            // 문제가 있는 종목들 추가 확인 및 정리
            if (!mismatches.isEmpty()) {
                log.info("구독 상태 불일치 종목 {}개 발견, 상태 정리 시작", mismatches.size());

                for (String stockCode : mismatches) {
                    Set<String> purposes = stockSubscriptionPurposes.get(stockCode);

                    // 목적이 없으면 구독자 수 제거
                    if (purposes == null || purposes.isEmpty()) {
                        stockCodeSubscriberCount.remove(stockCode);
                        log.info("종목 {} 구독 목적 없음, 구독자 수 제거", stockCode);

                        // 필요 시 실제 API에서 구독 해제
                        if (isConnected()) {
                            try {
                                unregisterRealTimeDataAsync("0B", Collections.singletonList(stockCode), null);
                                unregisterRealTimeDataAsync("0D", Collections.singletonList(stockCode), null);
                                log.info("종목 {} 실시간 데이터 구독 해제 요청 전송", stockCode);
                            } catch (Exception e) {
                                log.error("종목 {} 구독 해제 중 오류 발생", stockCode, e);
                            }
                        }

                        // 구독 목록에서 제거
                        subscribedStockCodes.remove(stockCode);
                    } else {
                        // 목적이 있으면 구독자 수를 목적 수와 일치시킴
                        stockCodeSubscriberCount.put(stockCode, purposes.size());
                        log.info("종목 {} 구독자 수 조정: {}명", stockCode, purposes.size());
                    }
                }
            }

            // 상태 점검 후 불필요한 연결 종료 확인
            disconnectIfNoSubscriptions();
        } finally {
            subscriptionLock.writeLock().unlock();
        }

        log.info("구독자 수 상태 점검 완료");
    }

    /**
     * 여러 종목을 동시에 구독하는 메서드
     * @param stockCodes 구독할 종목 코드 목록
     * @param purpose 구독 목적 (예: "PORTFOLIO")
     * @return 구독 성공 여부
     */
    public boolean subscribeStocksWithPurpose(List<String> stockCodes, String purpose) {
        if (stockCodes == null || stockCodes.isEmpty()) {
            log.error("구독할 종목 코드가 없습니다.");
            return false;
        }

        subscriptionLock.writeLock().lock();
        try {
            log.info("일괄 종목 구독 시작: {} 종목, 목적: {}", stockCodes.size(), purpose);

            // 구독할 새로운 종목들만 필터링
            List<String> newStockCodes = new ArrayList<>();

            // 각 종목에 목적 추가 및 구독자 수 증가
            for (String stockCode : stockCodes) {
                String normalizedCode = normalizeStockCode(stockCode);

                // 목적 추가
                Set<String> purposes = stockSubscriptionPurposes.computeIfAbsent(normalizedCode, k -> ConcurrentHashMap.newKeySet());
                boolean purposeAdded = purposes.add(purpose);

                // 구독자 수 증가
                int prevCount = stockCodeSubscriberCount.getOrDefault(normalizedCode, 0);
                stockCodeSubscriberCount.put(normalizedCode, prevCount + 1);

                // 내부 구독 목록에 추가
                subscribedStockCodes.add(normalizedCode);

                // 신규 구독인 경우에만 API 요청 목록에 추가
                if (prevCount == 0) {
                    newStockCodes.add(normalizedCode);
                    log.info("종목 {} 신규 구독 등록 예정", normalizedCode);
                } else {
                    log.info("종목 {} 이미 구독 중, 구독자 수만 증가: {} -> {}", normalizedCode, prevCount, prevCount + 1);
                }
            }

            // 실제 API 구독 요청 (신규 종목들만)
            boolean success = true;
            if (!newStockCodes.isEmpty() && isConnected()) {
                // 일괄 실시간 데이터 등록 요청 - 주식체결(0B)만 구독
                CompletableFuture<Boolean> future = new CompletableFuture<>();

                // 배치 처리 (일반적으로 API는 한 번에 처리할 수 있는 종목 수에 제한이 있음)
                int batchSize = 100; // 최대 배치 크기 - 실제 API 제한에 맞게 조정 필요

                List<List<String>> batches = new ArrayList<>();
                for (int i = 0; i < newStockCodes.size(); i += batchSize) {
                    batches.add(newStockCodes.subList(i, Math.min(i + batchSize, newStockCodes.size())));
                }

                for (List<String> batch : batches) {
                    try {
                        // 주식체결(0B) 등록만 수행
                        registerRealTimeDataAsync("0B", batch, future);
                        boolean batchResult = future.get(API_TIMEOUT_SECONDS, TimeUnit.SECONDS);
                        success &= batchResult;

                        log.info("신규 종목 배치 구독 완료 (체결 데이터): {} 종목", batch.size());
                    } catch (Exception e) {
                        log.error("신규 종목 배치 구독 중 오류: {} 종목", batch.size(), e);
                        success = false;
                    }
                }

                if (success) {
                    log.info("모든 신규 종목 일괄 구독 성공: {} 종목", newStockCodes.size());
                } else {
                    log.warn("일부 신규 종목 구독에 실패: {} 종목", newStockCodes.size());
                }
            }

            return success;
        } finally {
            subscriptionLock.writeLock().unlock();
        }
    }

}