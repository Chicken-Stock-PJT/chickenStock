package realClassOne.chickenStock.stock.websocket.handler;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import java.util.concurrent.ConcurrentHashMap;

@Component
@Slf4j
@RequiredArgsConstructor
public class StockWebSocketHandler extends TextWebSocketHandler implements KiwoomWebSocketClient.StockDataListener {

    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final ObjectMapper objectMapper;
    private final Map<String, WebSocketSession> sessions = new ConcurrentHashMap<>();

    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        log.info("클라이언트 연결: {}", session.getId());
        sessions.put(session.getId(), session);

        // KiwoomWebSocketClient에 리스너로 등록
        if (sessions.size() == 1) {
            kiwoomWebSocketClient.addListener(this);
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) {
        log.info("클라이언트 연결 종료: {}", session.getId());
        sessions.remove(session.getId());

        // 연결된 클라이언트가 없으면 리스너 제거
        if (sessions.isEmpty()) {
            kiwoomWebSocketClient.removeListener(this);
        }
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        try {
            // 클라이언트로부터 메시지 수신 처리
            JsonNode requestJson = objectMapper.readTree(message.getPayload());
            String action = requestJson.get("action").asText();

            if ("subscribe".equals(action)) {
                // 클라이언트가 특정 종목 구독 요청
                String stockCode = requestJson.get("stockCode").asText();
                log.info("클라이언트 {} 종목 구독 요청: {}", session.getId(), stockCode);

                // 여기서 필요하다면 해당 종목을 키움API에 등록할 수 있음
            } else if ("unsubscribe".equals(action)) {
                // 클라이언트가 특정 종목 구독 해제 요청
                String stockCode = requestJson.get("stockCode").asText();
                log.info("클라이언트 {} 종목 구독 해제 요청: {}", session.getId(), stockCode);
            }
        } catch (Exception e) {
            log.error("클라이언트 메시지 처리 중 오류 발생", e);
        }
    }

    @Override
    public void onStockPriceUpdate(String stockCode, JsonNode data) {
        try {
            // 주식체결 데이터 (0B)에서 필요한 정보만 추출
            ObjectNode messageNode = objectMapper.createObjectNode();
            messageNode.put("type", "stockPrice");
            messageNode.put("stockCode", stockCode);
            messageNode.put("currentPrice", data.get("10").asText());      // 현재가
            messageNode.put("priceChange", data.get("11").asText());       // 전일대비
            messageNode.put("changeRate", data.get("12").asText());        // 등락율
            messageNode.put("timestamp", data.get("20").asText());         // 체결시간

            String message = objectMapper.writeValueAsString(messageNode);
            broadcastMessage(message);
        } catch (Exception e) {
            log.error("주식체결 데이터 처리 중 오류 발생", e);
        }
    }

    @Override
    public void onStockBidAskUpdate(String stockCode, JsonNode data) {
        try {
            // 주식호가잔량 데이터 (0D)에서 필요한 정보만 추출
            ObjectNode messageNode = objectMapper.createObjectNode();
            messageNode.put("type", "stockBidAsk");
            messageNode.put("stockCode", stockCode);
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
            broadcastMessage(message);
        } catch (Exception e) {
            log.error("주식호가잔량 데이터 처리 중 오류 발생", e);
        }
    }

    private void broadcastMessage(String message) {
        sessions.values().forEach(session -> {
            try {
                if (session.isOpen()) {
                    session.sendMessage(new TextMessage(message));
                }
            } catch (IOException e) {
                log.error("메시지 전송 중 오류 발생: {}", session.getId(), e);
            }
        });
    }
}