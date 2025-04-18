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
import realClassOne.chickenStock.stock.service.StockSearchService;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

@Component
@Slf4j
@RequiredArgsConstructor
public class StockWebSocketHandler extends TextWebSocketHandler implements KiwoomWebSocketClient.StockDataListener {

    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final ObjectMapper objectMapper;
    private final StockSearchService stockSearchService;
    private final Map<String, WebSocketSession> sessions = new ConcurrentHashMap<>();
    // 세션별 구독 종목 관리
    private final Map<String, Set<String>> sessionSubscriptions = new ConcurrentHashMap<>();
    // 종목별 구독 세션 수 관리
    private final Map<String, Integer> stockSubscriptionCount = new ConcurrentHashMap<>();

    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        log.info("클라이언트 연결: {}", session.getId());
        sessions.put(session.getId(), session);
        sessionSubscriptions.put(session.getId(), new HashSet<>());

        // 최초 연결 시 리스너 등록
        if (sessions.size() == 1) {
            kiwoomWebSocketClient.addListener(this);
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) {
        log.info("클라이언트 연결 종료: {}", session.getId());

        // 구독 중인 종목들 구독 해제
        Set<String> subscribedStocks = sessionSubscriptions.getOrDefault(session.getId(), Collections.emptySet());
        for (String stockCode : subscribedStocks) {
            unsubscribeStock(session.getId(), stockCode);
        }

        sessions.remove(session.getId());
        sessionSubscriptions.remove(session.getId());

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
                String stockName = requestJson.get("stockName").asText();

                // 종목명으로 종목코드 찾기 (실제로는 DB나 다른 서비스를 통해 조회)
                String resolvedStockCode = resolveStockCode(stockName, stockCode);

                if (resolvedStockCode != null) {
                    subscribeStock(session.getId(), resolvedStockCode);
                    sendSuccessResponse(session, "subscribe", resolvedStockCode, "구독 성공");
                } else {
                    sendErrorResponse(session, "subscribe", stockName, "종목을 찾을 수 없습니다");
                }
            } else if ("unsubscribe".equals(action)) {
                // 클라이언트가 특정 종목 구독 해제 요청
                String stockCode = requestJson.get("stockCode").asText();
                unsubscribeStock(session.getId(), stockCode);
                sendSuccessResponse(session, "unsubscribe", stockCode, "구독 해제 성공");
            }
        } catch (Exception e) {
            log.error("클라이언트 메시지 처리 중 오류 발생", e);
            try {
                sendErrorResponse(session, "error", null, "메시지 처리 중 오류가 발생했습니다");
            } catch (IOException ex) {
                log.error("오류 응답 전송 실패", ex);
            }
        }
    }

    private String resolveStockCode(String stockName, String stockCode) {
        // 이미 코드가 제공된 경우 그대로 사용
        if (stockCode != null && !stockCode.isEmpty()) {
            return stockCode;
        }

        // 종목명으로 코드 찾기 (API 활용)
        return stockSearchService.findStockCodeByName(stockName);
    }

    private void subscribeStock(String sessionId, String stockCode) {
        // 세션에 구독 종목 추가
        Set<String> subscribedStocks = sessionSubscriptions.computeIfAbsent(sessionId, k -> new HashSet<>());
        if (subscribedStocks.add(stockCode)) {
            log.info("클라이언트 {} 종목 구독: {}", sessionId, stockCode);

            // 종목별 구독 카운트 증가
            int count = stockSubscriptionCount.getOrDefault(stockCode, 0) + 1;
            stockSubscriptionCount.put(stockCode, count);

            // 최초 구독 시 키움API에 등록
            if (count == 1) {
                log.info("키움API에 종목 등록: {}", stockCode);
                kiwoomWebSocketClient.registerRealTimeData("0B", List.of(stockCode)); // 주식체결
                kiwoomWebSocketClient.registerRealTimeData("0D", List.of(stockCode)); // 주식호가잔량
            }
        }
    }

    private void unsubscribeStock(String sessionId, String stockCode) {
        // 세션에서 구독 종목 제거
        Set<String> subscribedStocks = sessionSubscriptions.getOrDefault(sessionId, Collections.emptySet());
        if (subscribedStocks.remove(stockCode)) {
            log.info("클라이언트 {} 종목 구독 해제: {}", sessionId, stockCode);

            // 종목별 구독 카운트 감소
            int count = stockSubscriptionCount.getOrDefault(stockCode, 1) - 1;
            if (count <= 0) {
                stockSubscriptionCount.remove(stockCode);
                // TODO: 필요하다면 키움API에서 실시간 데이터 요청 해제 기능 구현
                log.info("키움API에서 종목 등록 해제 (구독자 없음): {}", stockCode);
            } else {
                stockSubscriptionCount.put(stockCode, count);
            }
        }
    }

    private void sendSuccessResponse(WebSocketSession session, String action, String stockCode, String message) throws IOException {
        ObjectNode response = objectMapper.createObjectNode();
        response.put("status", "success");
        response.put("action", action);
        response.put("stockCode", stockCode);
        response.put("message", message);

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(response)));
    }

    private void sendErrorResponse(WebSocketSession session, String action, String stockName, String errorMessage) throws IOException {
        ObjectNode response = objectMapper.createObjectNode();
        response.put("status", "error");
        response.put("action", action);
        if (stockName != null) {
            response.put("stockName", stockName);
        }
        response.put("message", errorMessage);

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(response)));
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
            // 해당 종목을 구독 중인 세션에만 전송
            sendToSubscribers(stockCode, message);
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
            // 해당 종목을 구독 중인 세션에만 전송
            sendToSubscribers(stockCode, message);
        } catch (Exception e) {
            log.error("주식호가잔량 데이터 처리 중 오류 발생", e);
        }
    }

    private void sendToSubscribers(String stockCode, String message) {
        // 해당 종목을 구독 중인 세션에만 메시지 전송
        for (Map.Entry<String, Set<String>> entry : sessionSubscriptions.entrySet()) {
            String sessionId = entry.getKey();
            Set<String> subscribedStocks = entry.getValue();

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
        }
    }
}