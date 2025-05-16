package realClassOne.chickenStock.chat.websocket.handler;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;
import realClassOne.chickenStock.chat.dto.ChatMessage;
import realClassOne.chickenStock.chat.dto.NotificationMessage;
import realClassOne.chickenStock.chat.service.ChatService;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.notification.entity.Notification;
import realClassOne.chickenStock.notification.event.CommentNotificationEvent;
import realClassOne.chickenStock.notification.event.LikeNotificationEvent;
import realClassOne.chickenStock.notification.event.TradeNotificationEvent;
import realClassOne.chickenStock.notification.repository.NotificationRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.io.IOException;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Component
@Slf4j
@RequiredArgsConstructor
public class ChatWebSocketHandler extends TextWebSocketHandler {

    private final JwtTokenProvider jwtTokenProvider;
    private final MemberRepository memberRepository;
    private final ObjectMapper objectMapper;
    private final ChatService chatService;
    private final NotificationRepository notificationRepository;
    private static final DateTimeFormatter formatter =
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
                    .withZone(ZoneId.of("Asia/Seoul"));

    // 세션별 회원 정보 관리
    private final Map<String, Long> sessionMemberMap = new ConcurrentHashMap<>();
    // 회원별 세션 관리
    private final Map<Long, WebSocketSession> memberSessionMap = new ConcurrentHashMap<>();
    // 세션별 닉네임 캐시
    private final Map<String, String> sessionNicknameMap = new ConcurrentHashMap<>();

    // 마지막 활동 시간 추적 (하트비트 대체)
    private final Map<String, Long> lastActivityMap = new ConcurrentHashMap<>();
    private static final long SESSION_TIMEOUT = 180000; // 3분

    @Override
    public void afterConnectionEstablished(WebSocketSession session) {
        log.info("채팅 WebSocket 클라이언트 연결: {}", session.getId());
        lastActivityMap.put(session.getId(), System.currentTimeMillis());

        try {
            ObjectNode message = objectMapper.createObjectNode();
            message.put("type", "connected");
            message.put("message", "채팅 및 알림 서버에 연결되었습니다. 인증이 필요합니다.");
            message.put("timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter));

            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
        } catch (Exception e) {
            log.error("연결 메시지 전송 실패", e);
        }
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        // 모든 메시지 수신 시 활동 시간 업데이트
        lastActivityMap.put(session.getId(), System.currentTimeMillis());

        try {
            JsonNode jsonNode = objectMapper.readTree(message.getPayload());
            String type = jsonNode.get("type").asText();

            switch (type) {
                case "authenticate":
                    authenticateUser(session, jsonNode);
                    break;
                case "chat":
                    handleChatMessage(session, jsonNode);
                    break;
                case "private":
                    handlePrivateMessage(session, jsonNode);
                    break;
                case "ping":
                    handlePing(session);
                    break;
                case "getNotifications":
                    handleGetNotifications(session, jsonNode);
                    break;
                case "markAsRead":
                    handleMarkAsRead(session, jsonNode);
                    break;
                default:
                    sendError(session, "지원하지 않는 메시지 타입입니다: " + type);
            }
        } catch (Exception e) {
            log.error("메시지 처리 중 오류 발생", e);
            sendError(session, "메시지 처리 중 오류가 발생했습니다");
        }
    }

    // 알림 목록 조회 처리
    @Transactional(readOnly = true)
    private void handleGetNotifications(WebSocketSession session, JsonNode jsonNode) {
        try {
            Long memberId = sessionMemberMap.get(session.getId());
            if (memberId == null) {
                sendError(session, "인증이 필요합니다");
                return;
            }

            String filter = jsonNode.has("filter") ? jsonNode.get("filter").asText() : "all";
            List<Notification> notifications;

            if ("unread".equals(filter)) {
                notifications = notificationRepository.findByMemberIdAndIsReadFalseOrderByCreatedAtDesc(memberId);
            } else {
                notifications = notificationRepository.findByMemberIdOrderByCreatedAtDesc(memberId);
            }

            // 응답 생성
            ObjectNode response = objectMapper.createObjectNode();
            response.put("type", "notificationList");

            ArrayNode notificationsArray = objectMapper.createArrayNode();
            for (Notification notification : notifications) {
                ObjectNode notificationNode = objectMapper.createObjectNode();
                notificationNode.put("notificationId", notification.getId());
                notificationNode.put("notificationType", notification.getType());
                notificationNode.put("title", notification.getTitle());
                notificationNode.put("message", notification.getMessage());
                ZonedDateTime createdAtKST = notification.getCreatedAt().atZone(ZoneId.of("Asia/Seoul")); // 🌐 [KST 적용]
                notificationNode.put("timestamp", createdAtKST.format(formatter)); // 🌐 [KST 적용]
                notificationNode.put("isRead", notification.isRead());

                // relatedId 추가 (댓글 ID)
                if (notification.getRelatedId() != null) {
                    notificationNode.put("relatedId", notification.getRelatedId());
                }

                notificationsArray.add(notificationNode);
            }

            response.set("notifications", notificationsArray);
            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(response)));

        } catch (Exception e) {
            log.error("알림 목록 조회 중 오류 발생", e);
            sendError(session, "알림 목록 조회 중 오류가 발생했습니다");
        }
    }

    // 알림 읽음 처리
    @Transactional
    private void handleMarkAsRead(WebSocketSession session, JsonNode jsonNode) {
        try {
            Long memberId = sessionMemberMap.get(session.getId());
            if (memberId == null) {
                sendError(session, "인증이 필요합니다");
                return;
            }

            Long notificationId = jsonNode.get("notificationId").asLong();
            Notification notification = notificationRepository.findById(notificationId).orElse(null);

            if (notification == null || !notification.getMemberId().equals(memberId)) {
                sendError(session, "알림을 찾을 수 없거나 권한이 없습니다");
                return;
            }

            notification.markAsRead();
            notificationRepository.save(notification);

            // 읽음 처리 성공 응답
            ObjectNode response = objectMapper.createObjectNode();
            response.put("type", "notificationRead");
            response.put("notificationId", notificationId);
            response.put("success", true);

            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(response)));

        } catch (Exception e) {
            log.error("알림 읽음 처리 중 오류 발생", e);
            sendError(session, "알림 읽음 처리 중 오류가 발생했습니다");
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) {
        String sessionId = session.getId();
        log.info("채팅 WebSocket 클라이언트 연결 종료: {}", sessionId);

        Long memberId = sessionMemberMap.remove(sessionId);
        if (memberId != null) {
            memberSessionMap.remove(memberId);
            sessionNicknameMap.remove(sessionId);
            lastActivityMap.remove(sessionId);

            // 사용자 퇴장 알림
            notifyUserLeft(memberId);
        }
    }

    private void authenticateUser(WebSocketSession session, JsonNode jsonNode) {
        try {
            String token = jsonNode.get("token").asText();

            // Bearer 접두사 제거
            if (token.startsWith("Bearer ")) {
                token = token.substring(7);
            }

            // 토큰 유효성 검증
            if (!jwtTokenProvider.validateToken(token)) {
                sendError(session, "인증에 실패했습니다: 유효하지 않은 토큰");
                return;
            }

            // 토큰에서 회원 ID 추출
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
            Member member = memberRepository.findById(memberId).orElse(null);

            if (member == null) {
                sendError(session, "인증에 실패했습니다: 회원 정보를 찾을 수 없습니다");
                return;
            }

            // 동시성 문제 해결을 위한 동기화
            synchronized (memberSessionMap) {
                // 기존 세션이 있으면 종료
                WebSocketSession oldSession = memberSessionMap.get(memberId);
                if (oldSession != null && oldSession.isOpen()) {
                    try {
                        oldSession.close();
                    } catch (Exception e) {
                        log.error("기존 세션 종료 중 오류", e);
                    }
                }

                // 세션 등록
                sessionMemberMap.put(session.getId(), memberId);
                memberSessionMap.put(memberId, session);
                sessionNicknameMap.put(session.getId(), member.getNickname());
            }

            // 인증 성공 응답
            ObjectNode response = objectMapper.createObjectNode();
            response.put("type", "authenticated");
            response.put("nickname", member.getNickname());
            response.put("memberId", memberId);

            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(response)));

            // 사용자 입장 알림
            notifyUserJoined(member);

            // 미확인 알림 전송
            sendUnreadNotifications(memberId, session);

        } catch (Exception e) {
            log.error("인증 처리 중 오류 발생", e);
            sendError(session, "인증 처리 중 오류가 발생했습니다");
        }
    }

    // 이벤트 리스너 - 지정가 체결 알림
    @EventListener
    @Async
    public void handleTradeNotification(TradeNotificationEvent event) {
        try {
            WebSocketSession session = memberSessionMap.get(event.getMemberId());
            if (session != null && session.isOpen()) {
                NotificationMessage notification = new NotificationMessage();
                notification.setType("TRADE");
                notification.setTitle("지정가 체결 알림");
                notification.setMessage(String.format("%s %s %d주가 %d원에 체결되었습니다.",
                        event.getStockName(),
                        event.getOrderType().equals("BUY") ? "매수" : "매도",
                        event.getQuantity(),
                        event.getPrice()));
                notification.setTimestamp(System.currentTimeMillis());
                notification.setIsRead(false);  // 추가

                sendNotification(session, notification);
            }
        } catch (Exception e) {
            log.error("거래 알림 전송 중 오류 발생", e);
        }
    }

    // 이벤트 리스너 - 댓글 알림
    @EventListener
    @Async
    public void handleCommentNotification(CommentNotificationEvent event) {
        try {
            WebSocketSession session = memberSessionMap.get(event.getMemberId());
            if (session != null && session.isOpen()) {
                NotificationMessage notification = new NotificationMessage();
                notification.setType("COMMENT");
                notification.setTitle("댓글 알림");
                notification.setMessage(String.format("%s님이 %s 종목에 댓글을 작성했습니다.",
                        event.getCommenterNickname(), event.getStockName()));
                notification.setTimestamp(System.currentTimeMillis());
                notification.setIsRead(false);  // 추가

                sendNotification(session, notification);
            }
        } catch (Exception e) {
            log.error("댓글 알림 전송 중 오류 발생", e);
        }
    }

    // 이벤트 리스너 - 좋아요 알림
    @EventListener
    @Async
    public void handleLikeNotification(LikeNotificationEvent event) {
        try {
            WebSocketSession session = memberSessionMap.get(event.getMemberId());
            if (session != null && session.isOpen()) {
                NotificationMessage notification = new NotificationMessage();
                notification.setType("LIKE");
                notification.setTitle("좋아요 알림");
                notification.setMessage(String.format("%s님이 %s 종목의 댓글에 좋아요를 눌렀습니다.",
                        event.getLikerNickname(), event.getStockName()));
                notification.setTimestamp(System.currentTimeMillis());
                notification.setIsRead(false);  // 추가

                sendNotification(session, notification);
            }
        } catch (Exception e) {
            log.error("좋아요 알림 전송 중 오류 발생", e);
        }
    }

    private void sendUnreadNotifications(Long memberId, WebSocketSession session) {
        try {
            List<Notification> unreadNotifications = notificationRepository
                    .findByMemberIdAndIsReadFalseOrderByCreatedAtDesc(memberId);

            for (Notification notification : unreadNotifications) {
                NotificationMessage message = new NotificationMessage();
                message.setType(notification.getType());
                message.setTitle(notification.getTitle());
                message.setMessage(notification.getMessage());
                message.setTimestamp(notification.getCreatedAt().toEpochSecond(java.time.ZoneOffset.UTC) * 1000);
                message.setNotificationId(notification.getId());
                message.setIsRead(notification.isRead());
                message.setRelatedId(notification.getRelatedId());

                sendNotification(session, message);
            }
        } catch (Exception e) {
            log.error("미확인 알림 조회 중 오류 발생", e);
        }
    }

    private void handleChatMessage(WebSocketSession session, JsonNode jsonNode) {
        try {
            String sessionId = session.getId();
            Long memberId = sessionMemberMap.get(sessionId);

            if (memberId == null) {
                sendError(session, "인증이 필요합니다");
                return;
            }

            String message = jsonNode.get("message").asText();
            String nickname = sessionNicknameMap.get(sessionId);

            // 채팅 메시지 생성
            ChatMessage chatMessage = new ChatMessage();
            chatMessage.setMemberId(memberId);
            chatMessage.setNickname(nickname);
            chatMessage.setMessage(message);
            chatMessage.setTimestamp(ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter)); // 🌐 [KST 적용]

            // 채팅 메시지 저장
            chatService.saveChatMessage(chatMessage);

            // 모든 연결된 사용자에게 브로드캐스트
            broadcastMessage(chatMessage);

        } catch (Exception e) {
            log.error("채팅 메시지 처리 중 오류 발생", e);
            sendError(session, "채팅 메시지 처리 중 오류가 발생했습니다");
        }
    }

    private void handlePrivateMessage(WebSocketSession session, JsonNode jsonNode) {
        try {
            Long senderId = sessionMemberMap.get(session.getId());
            if (senderId == null) {
                sendError(session, "인증이 필요합니다");
                return;
            }

            Long targetId = jsonNode.get("targetId").asLong();
            String message = jsonNode.get("message").asText();

            WebSocketSession targetSession = memberSessionMap.get(targetId);
            if (targetSession != null && targetSession.isOpen()) {
                ObjectNode privateMessage = objectMapper.createObjectNode();
                privateMessage.put("type", "private");
                privateMessage.put("senderId", senderId);
                privateMessage.put("senderNickname", sessionNicknameMap.get(session.getId()));
                privateMessage.put("message", message);
                privateMessage.put("timestamp", System.currentTimeMillis());

                targetSession.sendMessage(new TextMessage(objectMapper.writeValueAsString(privateMessage)));
            }
        } catch (Exception e) {
            log.error("개인 메시지 처리 중 오류 발생", e);
            sendError(session, "개인 메시지 처리 중 오류가 발생했습니다");
        }
    }

    private void handlePing(WebSocketSession session) {
        try {
            // 활동 시간 업데이트
            lastActivityMap.put(session.getId(), System.currentTimeMillis());

            ObjectNode pong = objectMapper.createObjectNode();
            pong.put("type", "pong");
            pong.put("timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter)); // 🌐 [KST 적용]
            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(pong)));
        } catch (Exception e) {
            log.error("Ping 응답 중 오류 발생", e);
        }
    }

    private void broadcastMessage(ChatMessage chatMessage) {
        try {
            ObjectNode message = objectMapper.createObjectNode();
            message.put("type", "chat");
            message.put("memberId", chatMessage.getMemberId());
            message.put("nickname", chatMessage.getNickname());
            message.put("message", chatMessage.getMessage());
            message.put("timestamp", chatMessage.getTimestamp());

            String messageStr = objectMapper.writeValueAsString(message);

            for (WebSocketSession session : memberSessionMap.values()) {
                if (session.isOpen()) {
                    try {
                        session.sendMessage(new TextMessage(messageStr));
                    } catch (Exception e) {
                        log.error("메시지 전송 실패", e);
                    }
                }
            }
        } catch (Exception e) {
            log.error("브로드캐스트 중 오류 발생", e);
        }
    }

    private void sendNotification(WebSocketSession session, NotificationMessage notification) {
        try {
            ObjectNode message = objectMapper.createObjectNode();
            message.put("type", "notification");
            message.put("notificationType", notification.getType());
            message.put("title", notification.getTitle());
            message.put("message", notification.getMessage());
            message.put("timestamp", notification.getTimestamp());

            if (notification.getNotificationId() != null) {
                message.put("notificationId", notification.getNotificationId());
            }

            if (notification.getIsRead() != null) {
                message.put("isRead", notification.getIsRead());
            }

            if (notification.getRelatedId() != null) {  // 추가
                message.put("relatedId", notification.getRelatedId());
            }

            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));  // response가 아니라 message
        } catch (Exception e) {
            log.error("알림 전송 실패", e);
        }
    }

    private void notifyUserJoined(Member member) {
        try {
            ObjectNode message = objectMapper.createObjectNode();
            message.put("type", "userJoined");
            message.put("nickname", member.getNickname());
            message.put("timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter)); // 🌐 [KST 적용]

            String messageStr = objectMapper.writeValueAsString(message);

            for (WebSocketSession session : memberSessionMap.values()) {
                if (session.isOpen()) {
                    session.sendMessage(new TextMessage(messageStr));
                }
            }
        } catch (Exception e) {
            log.error("입장 알림 전송 중 오류 발생", e);
        }
    }

    private void notifyUserLeft(Long memberId) {
        try {
            String nickname = getMemberNickname(memberId);
            if (nickname != null) {
                ObjectNode message = objectMapper.createObjectNode();
                message.put("type", "userLeft");
                message.put("nickname", nickname);
                message.put("timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter)); // 🌐 [KST 적용]

                String messageStr = objectMapper.writeValueAsString(message);

                for (WebSocketSession session : memberSessionMap.values()) {
                    if (session.isOpen()) {
                        session.sendMessage(new TextMessage(messageStr));
                    }
                }
            }
        } catch (Exception e) {
            log.error("퇴장 알림 전송 중 오류 발생", e);
        }
    }

    private String getMemberNickname(Long memberId) {
        for (Map.Entry<String, Long> entry : sessionMemberMap.entrySet()) {
            if (entry.getValue().equals(memberId)) {
                return sessionNicknameMap.get(entry.getKey());
            }
        }
        return null;
    }

    private void sendError(WebSocketSession session, String errorMessage) {
        try {
            ObjectNode error = objectMapper.createObjectNode();
            error.put("type", "error");
            error.put("message", errorMessage);

            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(error)));
        } catch (Exception e) {
            log.error("에러 메시지 전송 실패", e);
        }
    }

    // 비활성 세션만 체크하는 스케줄러
    @Scheduled(fixedDelay = 60000) // 1분마다 체크
    public void checkInactiveSessions() {
        long now = System.currentTimeMillis();
        List<String> sessionsToClose = new ArrayList<>();

        for (Map.Entry<String, Long> entry : lastActivityMap.entrySet()) {
            if (now - entry.getValue() > SESSION_TIMEOUT) {
                sessionsToClose.add(entry.getKey());
            }
        }

        // 타임아웃된 세션 종료
        for (String sessionId : sessionsToClose) {
            // sessionId로 WebSocketSession 찾기
            for (Map.Entry<Long, WebSocketSession> entry : memberSessionMap.entrySet()) {
                WebSocketSession session = entry.getValue();
                if (session.getId().equals(sessionId)) {
                    try {
                        session.close(CloseStatus.SESSION_NOT_RELIABLE);
                        log.info("비활성 세션 종료: {}", sessionId);
                    } catch (IOException e) {
                        log.error("세션 종료 실패", e);
                    }
                    break;
                }
            }
        }
    }
}