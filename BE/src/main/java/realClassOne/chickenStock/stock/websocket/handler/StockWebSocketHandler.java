package realClassOne.chickenStock.stock.websocket.handler;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

@Component
@Slf4j
@RequiredArgsConstructor
public class StockWebSocketHandler extends TextWebSocketHandler implements KiwoomWebSocketClient.StockDataListener {

    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final ObjectMapper objectMapper;
    private final Map<String, WebSocketSession> sessions = new ConcurrentHashMap<>();

    // 각 세션별 구독 종목 관리
    private final Map<String, Set<String>> sessionSubscriptions = new ConcurrentHashMap<>();

    // 종목별 구독자 수 관리 (이미 KiwoomWebSocketClient에는 있지만, 여기서도 필요함)
    private final Map<String, Integer> stockCodeSubscriberCount = new ConcurrentHashMap<>();

    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        log.info("클라이언트 연결: {}", session.getId());
        sessions.put(session.getId(), session);
        sessionSubscriptions.put(session.getId(), ConcurrentHashMap.newKeySet());

        // KiwoomWebSocketClient에 리스너로 등록
        if (sessions.size() == 1) {
            kiwoomWebSocketClient.addListener(this);
        }

        try {
            // 클라이언트에게 연결 성공 메시지 전송
            ObjectNode connectMessage = objectMapper.createObjectNode();
            connectMessage.put("type", "connected");
            connectMessage.put("message", "실시간 주식 데이터 서버에 연결되었습니다");

            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(connectMessage)));
        } catch (Exception e) {
            log.error("연결 메시지 전송 실패", e);
        }
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        try {
            // 클라이언트로부터 메시지 수신 처리
            JsonNode requestJson = objectMapper.readTree(message.getPayload());
            String action = requestJson.get("action").asText();

            // list 작업은 stockCode가 필요하지 않으므로 별도 처리
            if ("list".equals(action)) {
                // (기존 코드 유지)
                return;
            }

            // 나머지 작업은 stockCode 필요
            String stockCode = requestJson.has("stockCode") ? requestJson.get("stockCode").asText() : null;

            if (stockCode == null || stockCode.trim().isEmpty()) {
                sendErrorMessage(session, "종목 코드가 유효하지 않습니다.");
                return;
            }

            stockCode = stockCode.trim();

            if ("subscribe".equals(action)) {
                // 클라이언트가 특정 종목 구독 요청
                log.info("클라이언트 {} 종목 구독 요청: {}", session.getId(), stockCode);

                Set<String> subscribedStocks = sessionSubscriptions.get(session.getId());
                if (subscribedStocks != null && !subscribedStocks.contains(stockCode)) {
                    // 세션 맵에는 원본 코드로 관리 (클라이언트에게 보여줄 용도)
                    // 실제 키움 API 호출에는 _AL 붙은 코드 사용
                    boolean success = kiwoomWebSocketClient.subscribeStock(stockCode);

                    if (success) {
                        subscribedStocks.add(stockCode);
                        // 구독자 수 증가 (로컬 맵에도 기록)
                        stockCodeSubscriberCount.merge(stockCode, 1, Integer::sum);
                        sendSuccessMessage(session, "구독", stockCode);
                        log.info("종목 {} 구독 성공, 현재 구독자 수: {}", stockCode, stockCodeSubscriberCount.get(stockCode));
                    } else {
                        sendErrorMessage(session, "종목 등록에 실패했습니다: " + stockCode);
                    }
                } else {
                    sendInfoMessage(session, "이미 구독 중인 종목입니다: " + stockCode);
                }
            } else if ("unsubscribe".equals(action)) {
                // 클라이언트가 특정 종목 구독 해제 요청
                log.info("클라이언트 {} 종목 구독 해제 요청: {}", session.getId(), stockCode);

                Set<String> subscribedStocks = sessionSubscriptions.get(session.getId());
                if (subscribedStocks != null && subscribedStocks.contains(stockCode)) {
                    // 키움 API에서 종목 해제
                    boolean success = kiwoomWebSocketClient.unsubscribeStock(stockCode);

                    if (success) {
                        subscribedStocks.remove(stockCode);
                        // 구독자 수 감소 (로컬 맵에도 반영)
                        stockCodeSubscriberCount.computeIfPresent(stockCode, (k, v) -> v > 1 ? v - 1 : null);
                        sendSuccessMessage(session, "구독 해제", stockCode);
                        log.info("종목 {} 구독 해제 성공, 현재 구독자 수: {}", stockCode,
                                stockCodeSubscriberCount.getOrDefault(stockCode, 0));
                    } else {
                        sendErrorMessage(session, "종목 해제에 실패했습니다: " + stockCode);
                    }
                } else {
                    sendInfoMessage(session, "구독 중이 아닌 종목입니다: " + stockCode);
                }
            } else {
                // 알 수 없는 작업 요청
                sendErrorMessage(session, "지원하지 않는 작업입니다: " + action);
            }
        } catch (Exception e) {
            log.error("클라이언트 메시지 처리 중 오류 발생", e);
            try {
                sendErrorMessage(session, "메시지 처리 중 오류가 발생했습니다.");
            } catch (Exception ex) {
                log.error("오류 메시지 전송 실패", ex);
            }
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) {
        log.info("클라이언트 연결 종료: {}", session.getId());

        // 해당 세션의 모든 구독 종목 해제
        Set<String> subscribedStocks = sessionSubscriptions.remove(session.getId());
        if (subscribedStocks != null) {
            for (String stockCode : subscribedStocks) {
                // 구독자 수 차감 (반드시 구독 해제 전에 수행)
                stockCodeSubscriberCount.computeIfPresent(stockCode, (k, v) -> v > 1 ? v - 1 : null);
                // 키움 API에서 구독 해제
                kiwoomWebSocketClient.unsubscribeStock(stockCode);
                log.info("세션 종료로 인한 종목 {} 구독 해제, 남은 구독자 수: {}",
                        stockCode, stockCodeSubscriberCount.getOrDefault(stockCode, 0));
            }
        }

        sessions.remove(session.getId());

        // 연결된 클라이언트가 없으면 리스너 제거
        if (sessions.isEmpty()) {
            kiwoomWebSocketClient.removeListener(this);
        }
    }

    private void sendSuccessMessage(WebSocketSession session, String action, String stockCode) throws IOException {
        ObjectNode message = objectMapper.createObjectNode();
        message.put("type", "success");
        message.put("action", action);
        message.put("stockCode", stockCode);
        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
    }

    private void sendErrorMessage(WebSocketSession session, String errorMessage) throws IOException {
        ObjectNode message = objectMapper.createObjectNode();
        message.put("type", "error");
        message.put("message", errorMessage);
        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
    }

    private void sendInfoMessage(WebSocketSession session, String infoMessage) throws IOException {
        ObjectNode message = objectMapper.createObjectNode();
        message.put("type", "info");
        message.put("message", infoMessage);
        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
    }

    @Override
    public void onStockPriceUpdate(String stockCode, JsonNode data) {
        try {
            // 종목 코드에서 _AL 접미사 제거
            String originalStockCode = stockCode.replace("_AL", "");

            // 로그보기
            // 주식체결 데이터 (0B)에서 필요한 정보만 추출
            String currentPrice = data.get("10").asText();      // 현재가
            String priceChange = data.get("11").asText();       // 전일대비
            String changeRate = data.get("12").asText();        // 등락율
            String timestamp = data.get("20").asText();         // 체결시간

            // 자세한 로그 추가
            log.info("[실시간체결] 종목코드: {}, 가격: {}, 변동: {}, 등락률: {}%, 시간: {}, 구독자수: {}",
                    stockCode, currentPrice, priceChange, changeRate, timestamp,
                    stockCodeSubscriberCount.getOrDefault(originalStockCode, 0));

            // 주식체결 데이터 (0B)에서 필요한 정보만 추출
            ObjectNode messageNode = objectMapper.createObjectNode();
            messageNode.put("type", "stockPrice");
            messageNode.put("stockCode", originalStockCode);  // 원본 종목 코드 사용
            messageNode.put("currentPrice", data.get("10").asText());      // 현재가
            messageNode.put("priceChange", data.get("11").asText());       // 전일대비
            messageNode.put("changeRate", data.get("12").asText());        // 등락율
            messageNode.put("timestamp", data.get("20").asText());         // 체결시간

            String message = objectMapper.writeValueAsString(messageNode);

            // 해당 종목을 구독 중인 세션에만 전송 (원본 종목 코드 사용)
            broadcastToSubscribers(originalStockCode, message);
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
            // 주식호가잔량 데이터 (0D)에서 필요한 정보만 추출
            String timestamp = data.get("21").asText();         // 호가시간
            String topAskPrice = data.get("41").asText();       // 최우선 매도호가
            String topBidPrice = data.get("51").asText();       // 최우선 매수호가

            // 매수/매도 스프레드 계산 (로깅용)
            try {
                double spread = Double.parseDouble(topAskPrice) - Double.parseDouble(topBidPrice);
                log.info("[실시간호가] 종목코드: {}, 시간: {}, 매도1호가: {}, 매수1호가: {}, 스프레드: {}, 구독자수: {}",
                        stockCode, timestamp, topAskPrice, topBidPrice, spread,
                        stockCodeSubscriberCount.getOrDefault(originalStockCode, 0));
            } catch (NumberFormatException e) {
                log.info("[실시간호가] 종목코드: {}, 시간: {}, 매도1호가: {}, 매수1호가: {}, 구독자수: {}",
                        stockCode, timestamp, topAskPrice, topBidPrice,
                        stockCodeSubscriberCount.getOrDefault(originalStockCode, 0));
            }

            // 주식호가잔량 데이터 (0D)에서 필요한 정보만 추출
            ObjectNode messageNode = objectMapper.createObjectNode();
            messageNode.put("type", "stockBidAsk");
            messageNode.put("stockCode", originalStockCode);  // 원본 종목 코드 사용
            messageNode.put("timestamp", data.get("21").asText());         // 호가시간

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

    // 특정 종목 구독자들에게만 메시지 전송
    private void broadcastToSubscribers(String stockCode, String message) {
        sessionSubscriptions.forEach((sessionId, subscribedStocks) -> {
            if (subscribedStocks.contains(stockCode)) {
                WebSocketSession session = sessions.get(sessionId);

                if (session != null && session.isOpen()) {
                    try {
                        session.sendMessage(new TextMessage(message));
                    } catch (IOException e) {
                        log.error("메시지 전송 중 오류 발생: {}", sessionId, e);
                    }
                }
            }
        });
    }
}