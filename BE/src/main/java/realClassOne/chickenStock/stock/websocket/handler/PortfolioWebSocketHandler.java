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
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.dto.response.PortfolioRealtimeDTO;
import realClassOne.chickenStock.stock.dto.response.PortfolioResponseDTO;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;
import realClassOne.chickenStock.stock.service.PortfolioService;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

@Component
@Slf4j
@RequiredArgsConstructor
public class PortfolioWebSocketHandler extends TextWebSocketHandler implements KiwoomWebSocketClient.StockDataListener {

    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final JwtTokenProvider jwtTokenProvider;
    private final MemberRepository memberRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final PortfolioService portfolioService;
    private final ObjectMapper objectMapper;

    // 세션별 회원 ID 관리
    private final Map<String, Long> sessionMemberMap = new ConcurrentHashMap<>();
    // 회원별 세션 관리
    private final Map<Long, WebSocketSession> memberSessionMap = new ConcurrentHashMap<>();

    private final Map<String, Set<Long>> stockSubscribers = new ConcurrentHashMap<>();

    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        log.info("포트폴리오 WebSocket 클라이언트 연결: {}", session.getId());

        // 키움 웹소켓 클라이언트에 리스너로 등록
        kiwoomWebSocketClient.addListener(this);

        try {
            // 연결 성공 메시지 전송
            ObjectNode message = objectMapper.createObjectNode();
            message.put("type", "connected");
            message.put("message", "포트폴리오 실시간 데이터 서버에 연결되었습니다");

            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
        } catch (Exception e) {
            log.error("연결 메시지 전송 실패", e);
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) {
        String sessionId = session.getId();
        log.info("포트폴리오 WebSocket 클라이언트 연결 종료: {}", sessionId);

        // 연결 종료 지연 처리를 위한 스케줄러 사용
        new Thread(() -> {
            try {
                // 5초 대기
                Thread.sleep(5000);

                synchronized (this) {
                    // 5초 후에도 같은 세션 ID로 재연결되지 않았는지 확인
                    if (!sessionMemberMap.containsKey(sessionId)) {
                        return; // 이미 다른 스레드에서 처리했거나 재연결됨
                    }

                    // 세션-회원 매핑 정보 가져오기
                    Long memberId = sessionMemberMap.remove(sessionId);
                    if (memberId != null) {
                        memberSessionMap.remove(memberId);

                        // 해당 회원의 구독 정보 제거 및 필요시 구독 취소
                        handleMemberUnsubscribe(memberId);
                    }

                    // 등록된 클라이언트가 없으면 리스너 제거
                    if (sessionMemberMap.isEmpty()) {
                        kiwoomWebSocketClient.removeListener(this);
                    }
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.error("연결 종료 지연 처리 중 인터럽트 발생", e);
            } catch (Exception e) {
                log.error("연결 종료 지연 처리 중 오류 발생", e);
            }
        }).start();
    }

    // 회원 구독 해제 로직을 별도 메서드로 분리
    private synchronized void handleMemberUnsubscribe(Long memberId) {
        List<String> unsubscribedStocks = new ArrayList<>();

        // 해당 회원의 구독 정보 제거 및 필요시 구독 취소
        for (Map.Entry<String, Set<Long>> entry : stockSubscribers.entrySet()) {
            String stockCode = entry.getKey();
            Set<Long> subscribers = entry.getValue();

            // 해당 종목 구독자 목록에서 회원 제거
            boolean removed = subscribers.remove(memberId);

            if (removed) {
                log.info("회원 ID {} 종목 {} 구독 해제", memberId, stockCode);

                // 구독자가 없으면 종목 구독 취소 및 맵에서 제거
                if (subscribers.isEmpty()) {
                    boolean unsubscribed = kiwoomWebSocketClient.unsubscribeStockForPurpose(stockCode, "PORTFOLIO");
                    log.info("종목 {} 구독 취소 (결과: {}): 구독자 없음", stockCode, unsubscribed);
                    stockSubscribers.remove(stockCode);
                    unsubscribedStocks.add(stockCode);
                }
            }
        }

        // 해제된 종목 목록 로깅
        if (!unsubscribedStocks.isEmpty()) {
            log.info("회원 ID {} 로그아웃으로 인해 구독 취소된 종목: {}", memberId, unsubscribedStocks);
        }
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        try {
            // 클라이언트 메시지 처리
            JsonNode requestJson = objectMapper.readTree(message.getPayload());
            String action = requestJson.get("action").asText();

            if ("authenticate".equals(action)) {
                // 인증 토큰으로 회원 식별
                String token = requestJson.get("token").asText();
                authenticateUser(session, token);
            }
        } catch (Exception e) {
            log.error("클라이언트 메시지 처리 중 오류 발생", e);
            try {
                sendErrorMessage(session, "메시지 처리 중 오류가 발생했습니다");
            } catch (Exception ex) {
                log.error("오류 메시지 전송 실패", ex);
            }
        }
    }

    /**
     * 사용자 인증 처리
     */
    private void authenticateUser(WebSocketSession session, String token) throws IOException {
        // Bearer 접두사 제거
        if (token.startsWith("Bearer ")) {
            token = token.substring(7);
        }

        // 토큰 유효성 검증
        if (!jwtTokenProvider.validateToken(token)) {
            sendErrorMessage(session, "인증에 실패했습니다: 유효하지 않은 토큰");
            return;
        }

        // 토큰에서 회원 ID 추출
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
        if (memberId == null) {
            sendErrorMessage(session, "인증에 실패했습니다: 회원 정보를 찾을 수 없습니다");
            return;
        }

        // 회원 정보 조회
        Member member = memberRepository.findById(memberId).orElse(null);
        if (member == null) {
            sendErrorMessage(session, "인증에 실패했습니다: 회원 정보를 찾을 수 없습니다");
            return;
        }

        // 이전 세션이 있으면 제거
        if (memberSessionMap.containsKey(memberId)) {
            WebSocketSession oldSession = memberSessionMap.get(memberId);
            String oldSessionId = oldSession.getId();

            // 이전 세션에서 구독 정보 제거
            sessionMemberMap.remove(oldSessionId);

            // 웹소켓 연결이 아직 살아있다면 연결 종료
            if (oldSession.isOpen()) {
                try {
                    oldSession.close();
                    log.info("이전 세션 {} 강제 종료 (새 세션: {})", oldSessionId, session.getId());
                } catch (Exception e) {
                    log.warn("이전 세션 종료 중 오류 발생", e);
                }
            }
        }

        // 새 세션 등록
        sessionMemberMap.put(session.getId(), memberId);
        memberSessionMap.put(memberId, session);

        log.info("포트폴리오 웹소켓 인증 성공: 세션={}, 회원ID={}", session.getId(), memberId);

        // 사용자가 보유한 종목 구독 등록
        List<String> stockCodes = portfolioService.getMemberStockCodes(memberId);
        for (String stockCode : stockCodes) {
            // 종목별 구독자 목록에 회원 추가
            stockSubscribers.computeIfAbsent(stockCode, k -> ConcurrentHashMap.newKeySet()).add(memberId);

            // 새로운 구독이면 키움 클라이언트에 등록
            if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
                kiwoomWebSocketClient.subscribeStockWithPurpose(stockCode, "PORTFOLIO");
                log.info("회원 {} 보유 종목 {} 구독 등록", memberId, stockCode);
            } else {
                // 이미 구독 중이더라도 목적은 추가
                kiwoomWebSocketClient.subscribeStockWithPurpose(stockCode, "PORTFOLIO");
                log.info("회원 {} 보유 종목 {} 목적 추가 (이미 구독 중)", memberId, stockCode);
            }
        }

        // 인증 성공 메시지 전송
        ObjectNode response = objectMapper.createObjectNode();
        response.put("type", "authenticated");
        response.put("memberId", memberId);
        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(response)));

        // 인증 후 초기 포트폴리오 데이터 전체 전송
        sendFullPortfolioUpdate(memberId);
    }

    /**
     * 주식 가격 변동 시 호출되는 메서드 (KiwoomWebSocketClient.StockDataListener 인터페이스 구현)
     */
    @Override
    public void onStockPriceUpdate(String stockCode, JsonNode data) {
        try {
            // 현재가 추출 (부호 및 쉼표 처리)
            if (data == null || !data.has("10")) {
                return;
            }

            Long currentPrice = Long.parseLong(data.get("10").asText()
                    .replace(",", "")
                    .replace("+", "")
                    .replace("-", "")
                    .trim());

            // 해당 종목을 보유한 회원들에게 업데이트 전송
            for (Map.Entry<Long, WebSocketSession> entry : memberSessionMap.entrySet()) {
                Long memberId = entry.getKey();
                WebSocketSession session = entry.getValue();

                // JOIN FETCH를 사용하여 N+1 문제 해결 및 LazyInitializationException 방지
                List<HoldingPosition> positions = holdingPositionRepository
                        .findWithStockDataByMemberIdAndStockCode(memberId, stockCode);

                for (HoldingPosition position : positions) {
                    // 이제 position.getStockData().getShortCode()가 안전하게 호출됨
                    sendRealtimeStockUpdate(session, memberId, stockCode, currentPrice, position);
                }
            }
        } catch (Exception e) {
            log.error("주식 가격 업데이트 처리 중 오류 발생", e);
        }
    }

    /**
     * 주식 호가 업데이트 (사용하지 않음)
     */
    @Override
    public void onStockBidAskUpdate(String stockCode, JsonNode data) {
        // 포트폴리오에서는 호가 정보를 사용하지 않음
    }

    /**
     * 특정 종목의 가격 업데이트를 클라이언트에 부분 업데이트로 전송
     */
    private void sendRealtimeStockUpdate(WebSocketSession session, Long memberId,
                                         String stockCode, Long currentPrice,
                                         HoldingPosition position) {
        try {
            // 평가 손익 및 수익률 계산
            Long investmentAmount = position.getAveragePrice() * position.getQuantity();
            Long valuationAmount = currentPrice * position.getQuantity();
            Long profitLoss = valuationAmount - investmentAmount;
            Double returnRate = investmentAmount > 0
                    ? (profitLoss.doubleValue() / investmentAmount.doubleValue()) * 100
                    : 0.0;

            // 업데이트 메시지 구성
            ObjectNode message = objectMapper.createObjectNode();
            message.put("type", "stockUpdate");
            message.put("stockCode", stockCode);
            message.put("currentPrice", currentPrice);
            message.put("valuationAmount", valuationAmount);
            message.put("profitLoss", profitLoss);
            message.put("returnRate", returnRate);

            // 총 자산 및 수익률 계산을 위한 포트폴리오 정보 조회
            ObjectNode totalData = calculateTotalData(memberId);
            if (totalData != null) {
                message.set("totalData", totalData);
            }

            // 메시지 전송
            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
        } catch (Exception e) {
            log.error("주식 가격 업데이트 전송 중 오류 발생", e);
        }
    }

    /**
     * 총 자산 및 수익률 계산
     */
    private ObjectNode calculateTotalData(Long memberId) {
        try {
            Member member = memberRepository.findById(memberId).orElse(null);
            if (member == null) {
                return null;
            }

            // 변경된 부분: JOIN FETCH를 사용하여 StockData를 즉시 로딩
            List<HoldingPosition> positions = holdingPositionRepository.findWithStockDataByMemberId(memberId);

            Long totalInvestment = 0L;
            Long totalValuation = 0L;

            for (HoldingPosition position : positions) {
                String stockCode = position.getStockData().getShortCode();
                JsonNode priceData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);

                Long currentPrice = 0L;
                if (priceData != null && priceData.has("10")) {
                    // 수정된 부분: 부호 및 쉼표 처리 추가
                    currentPrice = Long.parseLong(priceData.get("10").asText()
                            .replace(",", "")
                            .replace("+", "")
                            .replace("-", "")
                            .trim());
                }

                totalInvestment += position.getAveragePrice() * position.getQuantity();
                totalValuation += currentPrice * position.getQuantity();
            }

            Long totalProfitLoss = totalValuation - totalInvestment;
            Double totalReturnRate = totalInvestment > 0
                    ? (totalProfitLoss.doubleValue() / totalInvestment.doubleValue()) * 100
                    : 0.0;
            Long totalAsset = member.getMemberMoney() + totalValuation;

            ObjectNode totalData = objectMapper.createObjectNode();
            totalData.put("totalAsset", totalAsset);
            totalData.put("totalProfitLoss", totalProfitLoss);
            totalData.put("totalReturnRate", totalReturnRate);

            return totalData;
        } catch (Exception e) {
            log.error("총 자산 정보 계산 중 오류 발생", e);
            return null;
        }
    }

    /**
     * 에러 메시지 전송
     */
    private void sendErrorMessage(WebSocketSession session, String message) throws IOException {
        ObjectNode errorMessage = objectMapper.createObjectNode();
        errorMessage.put("type", "error");
        errorMessage.put("message", message);

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(errorMessage)));
    }

    /**
     * 전체 포트폴리오 데이터를 클라이언트에 전송 (지정가 주문 체결 시 호출)
     */
    public void sendFullPortfolioUpdate(Long memberId) {
        try {
            WebSocketSession session = memberSessionMap.get(memberId);
            if (session != null && session.isOpen()) {
                // 포트폴리오 정보 조회
                PortfolioResponseDTO portfolioDTO = portfolioService.getPortfolioById(memberId);

                // 응답 메시지 생성
                ObjectNode message = objectMapper.createObjectNode();
                message.put("type", "fullPortfolioUpdate");
                message.set("data", objectMapper.valueToTree(portfolioDTO));

                // 메시지 전송
                session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
                log.info("전체 포트폴리오 업데이트 전송: 회원ID={}", memberId);
            } else {
                log.debug("포트폴리오 업데이트 전송 실패: 회원ID={} - 웹소켓 세션 없음", memberId);
            }
        } catch (Exception e) {
            log.error("포트폴리오 업데이트 전송 중 오류 발생: 회원ID={}", memberId, e);
        }
    }
}