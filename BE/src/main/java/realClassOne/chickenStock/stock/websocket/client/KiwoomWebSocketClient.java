package realClassOne.chickenStock.stock.websocket.client;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import jakarta.annotation.PostConstruct;
import realClassOne.chickenStock.stock.service.KiwoomAuthService;

import java.net.URI;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;

@Component
@Slf4j
@RequiredArgsConstructor
public class KiwoomWebSocketClient {

    @Value("${kiwoom.websocket.url}")
    private String websocketUrl;

    private final KiwoomAuthService authService;
    private final ObjectMapper objectMapper;
    private final List<StockDataListener> listeners = new CopyOnWriteArrayList<>();

    private WebSocketClient client;
    private boolean connected = false;

    // 구독된 종목 코드 관리
    private final Set<String> subscribedStockCodes = Collections.synchronizedSet(new HashSet<>());
    // 종목별 구독자 수 관리
    private final Map<String, Integer> stockCodeSubscriberCount = new ConcurrentHashMap<>();

    // 종목별 최신 가격 데이터 캐시
    private final Map<String, JsonNode> latestPriceDataCache = new ConcurrentHashMap<>();

    // 최신 가격 데이터 반환 메서드
    public JsonNode getLatestStockPriceData(String stockCode) {
        return latestPriceDataCache.get(stockCode);
    }

    public interface StockDataListener {
        void onStockPriceUpdate(String stockCode, JsonNode data);
        void onStockBidAskUpdate(String stockCode, JsonNode data);
    }

    public void addListener(StockDataListener listener) {
        listeners.add(listener);
    }

    public void removeListener(StockDataListener listener) {
        listeners.remove(listener);
    }

    // 기존 onStockPriceUpdate 메서드 내부에 추가할 코드 (data를 캐시에 저장)
    public void processStockPriceUpdate(String stockCode, JsonNode data) {
        try {
            // 로그 데이터 (기존 코드)
            log.info("[실시간가격] 종목: {}, 현재가: {}, 전일대비: {}, 등락률: {}%, 체결시간: {}",
                    stockCode,
                    data.get("10").asText(),
                    data.get("11").asText(),
                    data.get("12").asText(),
                    data.get("20").asText());

            // 최신 데이터를 캐시에 저장 (기존 코드)
            latestPriceDataCache.put(stockCode, data);

            // 주식체결 데이터 처리 (기존 코드)
            notifyStockPriceUpdate(stockCode, data);
        } catch (Exception e) {
            log.error("실시간 데이터 처리 중 오류 발생", e);
        }
    }

    @PostConstruct
    public void init() {
        connect();
    }

    public void connect() {
        try {
            // isConnecting() 메서드 사용 대신 연결 상태 체크 변경
            if (client != null && client.isOpen()) {
                log.info("WebSocket 이미 연결 중입니다.");
                return;
            }

            client = new WebSocketClient(new URI(websocketUrl)) {
                @Override
                public void onOpen(ServerHandshake handshake) {
                    log.info("키움증권 WebSocket 서버 연결 성공");
                    login();
                    connected = true;
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
                            send(message);
                        } else if ("REAL".equals(trnm)) {
                            processRealTimeData(response);
                        }

                        if (!"PING".equals(trnm)) {
                            log.debug("키움증권 WebSocket 메시지 수신: {}", message);
                        }
                    } catch (Exception e) {
                        log.error("WebSocket 메시지 처리 중 오류 발생", e);
                    }
                }

                @Override
                public void onClose(int code, String reason, boolean remote) {
                    log.info("키움증권 WebSocket 연결 종료: code={}, reason={}, remote={}", code, reason, remote);
                    connected = false;
                    reconnect();
                }

                @Override
                public void onError(Exception ex) {
                    log.error("키움증권 WebSocket 오류 발생", ex);
                    connected = false;
                    reconnect();
                }
            };

            client.connect();
        } catch (Exception e) {
            log.error("키움증권 WebSocket 연결 시도 중 오류 발생", e);
            reconnect();
        }
    }

    private void login() {
        try {
            ObjectNode loginMessage = objectMapper.createObjectNode();
            loginMessage.put("trnm", "LOGIN");
            loginMessage.put("token", authService.getAccessToken());

            String loginMessageStr = objectMapper.writeValueAsString(loginMessage);
            client.send(loginMessageStr);
            log.info("키움증권 WebSocket 로그인 요청 전송");
        } catch (Exception e) {
            log.error("키움증권 WebSocket 로그인 요청 전송 중 오류 발생", e);
        }
    }

    // 종목 구독 메서드
    public synchronized boolean subscribeStock(String stockCode) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
            log.error("유효하지 않은 종목 코드: {}", stockCode);
            return false;
        }

        stockCode = stockCode.trim();

        // 구독자 수 증가
        int count = stockCodeSubscriberCount.getOrDefault(stockCode, 0) + 1;
        stockCodeSubscriberCount.put(stockCode, count);

        // 새로운 종목 구독인 경우 실시간 데이터 등록
        if (count == 1) {
            subscribedStockCodes.add(stockCode);

            if (isConnected()) {
                registerRealTimeData("0B", List.of(stockCode)); // 주식체결
                registerRealTimeData("0D", List.of(stockCode)); // 주식호가잔량
                log.info("종목 구독 성공: {}", stockCode);
            } else {
                log.warn("WebSocket 연결이 없어 나중에 등록됩니다: {}", stockCode);
            }
        } else {
            log.info("종목 구독자 수 증가: {} ({}명)", stockCode, count);
        }

        return true;
    }

    // 종목 구독 해제 메서드
    public synchronized boolean unsubscribeStock(String stockCode) {
        if (stockCode == null || !stockCodeSubscriberCount.containsKey(stockCode)) {
            return false;
        }

        int count = stockCodeSubscriberCount.getOrDefault(stockCode, 0) - 1;

        if (count <= 0) {
            stockCodeSubscriberCount.remove(stockCode);
            subscribedStockCodes.remove(stockCode);

            if (isConnected()) {
                unregisterRealTimeData("0B", List.of(stockCode)); // 주식체결 해제
                unregisterRealTimeData("0D", List.of(stockCode)); // 주식호가잔량 해제
                log.info("종목 구독 해제: {}", stockCode);
            }
        } else {
            stockCodeSubscriberCount.put(stockCode, count);
            log.info("종목 구독자 수 감소: {} ({}명)", stockCode, count);
        }

        return true;
    }

    // 키움 API에 실시간 데이터 등록 요청
    public void registerRealTimeData(String type, List<String> stockCodes) {
        if (stockCodes == null || stockCodes.isEmpty()) {
            return;
        }

        try {
            ObjectNode registerMessage = objectMapper.createObjectNode();
            registerMessage.put("trnm", "REG");
            registerMessage.put("grp_no", "1");
            registerMessage.put("refresh", "1");

            ArrayNode dataArray = objectMapper.createArrayNode();
            ObjectNode dataObject = objectMapper.createObjectNode();

            ArrayNode itemArray = objectMapper.createArrayNode();
            stockCodes.forEach(itemArray::add);

            ArrayNode typeArray = objectMapper.createArrayNode();
            typeArray.add(type);

            dataObject.set("item", itemArray);
            dataObject.set("type", typeArray);
            dataArray.add(dataObject);

            registerMessage.set("data", dataArray);

            String registerMessageStr = objectMapper.writeValueAsString(registerMessage);
            client.send(registerMessageStr);
            log.info("실시간 데이터 등록 요청 전송: type={}, stocks={}", type, stockCodes);
        } catch (Exception e) {
            log.error("실시간 데이터 등록 요청 전송 중 오류 발생", e);
        }
    }

    // 키움 API에 실시간 데이터 해제 요청
    public void unregisterRealTimeData(String type, List<String> stockCodes) {
        if (stockCodes == null || stockCodes.isEmpty()) {
            return;
        }

        try {
            ObjectNode unregisterMessage = objectMapper.createObjectNode();
            unregisterMessage.put("trnm", "REMOVE");
            unregisterMessage.put("grp_no", "1");

            ArrayNode dataArray = objectMapper.createArrayNode();
            ObjectNode dataObject = objectMapper.createObjectNode();

            ArrayNode itemArray = objectMapper.createArrayNode();
            stockCodes.forEach(itemArray::add);

            ArrayNode typeArray = objectMapper.createArrayNode();
            typeArray.add(type);

            dataObject.set("item", itemArray);
            dataObject.set("type", typeArray);
            dataArray.add(dataObject);

            unregisterMessage.set("data", dataArray);

            String unregisterMessageStr = objectMapper.writeValueAsString(unregisterMessage);
            client.send(unregisterMessageStr);
            log.info("실시간 데이터 해제 요청 전송: type={}, stocks={}", type, stockCodes);
        } catch (Exception e) {
            log.error("실시간 데이터 해제 요청 전송 중 오류 발생", e);
        }
    }

    // 연결 재시작시 모든 구독 종목 재등록
    private void reregisterAllStocks() {
        if (subscribedStockCodes.isEmpty()) {
            return;
        }

        List<String> stockCodes = new ArrayList<>(subscribedStockCodes);
        log.info("기존 구독 종목 재등록: {}", stockCodes);

        // 최대 100개씩 나누어 등록 (API 한계 고려)
        int batchSize = 100;
        for (int i = 0; i < stockCodes.size(); i += batchSize) {
            List<String> batch = stockCodes.subList(i, Math.min(i + batchSize, stockCodes.size()));
            registerRealTimeData("0B", batch); // 주식체결
            registerRealTimeData("0D", batch); // 주식호가잔량
        }
    }

    private void processRealTimeData(JsonNode response) {
        try {
            JsonNode dataArray = response.get("data");
            if (dataArray != null && dataArray.isArray()) {
                for (JsonNode dataItem : dataArray) {
                    String type = dataItem.get("type").asText();
                    String stockCode = dataItem.get("item").asText();
                    JsonNode values = dataItem.get("values");

                    if ("0B".equals(type)) {
                        // 캐시에 최신 데이터 저장 (이 부분 추가)
                        latestPriceDataCache.put(stockCode, values);

                        // 로그보기
                        log.info("[실시간가격] 종목: {}, 현재가: {}, 전일대비: {}, 등락률: {}%, 체결시간: {}",
                                stockCode,
                                values.get("10").asText(),
                                values.get("11").asText(),
                                values.get("12").asText(),
                                values.get("20").asText());

                        // 주식체결 데이터 처리
                        notifyStockPriceUpdate(stockCode, values);
                    } else if ("0D".equals(type)) {
                        // 주식호가잔량 데이터 로그
                        log.info("[호가잔량] 종목: {}, 시간: {}, 최우선매도호가: {}, 최우선매수호가: {}",
                                stockCode,
                                values.get("21").asText(),
                                values.get("41").asText().trim(),  // 공백 제거
                                values.get("51").asText().replace("-", "").trim());  // 음수 부호 및 공백 제거

                        // 주식호가잔량 데이터 처리
                        notifyStockBidAskUpdate(stockCode, values);
                    }
                }
            }
        } catch (Exception e) {
            log.error("실시간 데이터 처리 중 오류 발생", e);
        }
    }

    private void notifyStockPriceUpdate(String stockCode, JsonNode data) {
        for (StockDataListener listener : listeners) {
            listener.onStockPriceUpdate(stockCode, data);
        }
    }

    private void notifyStockBidAskUpdate(String stockCode, JsonNode data) {
        for (StockDataListener listener : listeners) {
            listener.onStockBidAskUpdate(stockCode, data);
        }
    }

    private void reconnect() {
        try {
            if (client != null) {
                client.close();
            }

            // 5초 후 재연결 시도
            new Thread(() -> {
                try {
                    TimeUnit.SECONDS.sleep(5);
                    log.info("키움증권 WebSocket 재연결 시도");
                    connect();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }).start();
        } catch (Exception e) {
            log.error("키움증권 WebSocket 재연결 시도 중 오류 발생", e);
        }
    }


    // WebSocket 연결 상태 확인
    public boolean isConnected() {
        return connected && client != null && client.isOpen();
    }

    // 현재 구독중인 종목 목록 조회
    public Set<String> getSubscribedStockCodes() {
        return Collections.unmodifiableSet(subscribedStockCodes);
    }

    // 특정 종목이 구독 중인지 확인
    public boolean isSubscribed(String stockCode) {
        return subscribedStockCodes.contains(stockCode);
    }

    // 구독자 수 조회
    public int getSubscriberCount(String stockCode) {
        return stockCodeSubscriberCount.getOrDefault(stockCode, 0);
    }
}