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
import java.util.List;
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

    @PostConstruct
    public void init() {
        connect();
    }

    public void connect() {
        try {
            if (client != null && (client.isOpen())) {
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
                                // 로그인 성공 후 관심 종목 등록
                                registerStocks();
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

    public void registerStocks() {
        try {
            // 주식체결(0B) 등록
            registerRealTimeData("0B", List.of("005930", "000660", "035420"));

            // 주식호가잔량(0D) 등록
            registerRealTimeData("0D", List.of("005930", "000660", "035420"));

            log.info("실시간 주식 데이터 등록 완료");
        } catch (Exception e) {
            log.error("실시간 주식 데이터 등록 중 오류 발생", e);
        }
    }

    public void registerRealTimeData(String type, List<String> stockCodes) {
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

    private void processRealTimeData(JsonNode response) {
        try {
            JsonNode dataArray = response.get("data");
            if (dataArray != null && dataArray.isArray()) {
                for (JsonNode dataItem : dataArray) {
                    String type = dataItem.get("type").asText();
                    String stockCode = dataItem.get("item").asText();
                    JsonNode values = dataItem.get("values");

                    if ("0B".equals(type)) {
                        // 주식체결 데이터 처리
                        notifyStockPriceUpdate(stockCode, values);
                    } else if ("0D".equals(type)) {
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
}