package realClassOne.chickenStock.chat.websocket.handler;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;
import realClassOne.chickenStock.chat.service.ChatService;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.notification.entity.Notification;
import realClassOne.chickenStock.notification.repository.NotificationRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

@Component
@Slf4j
@RequiredArgsConstructor
public class ChatWebSocketHandler extends TextWebSocketHandler {

    private final JwtTokenProvider jwtTokenProvider;
    private final MemberRepository memberRepository;
    private final NotificationRepository notificationRepository;
    private final ObjectMapper objectMapper;

    private static final DateTimeFormatter formatter =
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
                    .withZone(ZoneId.of("Asia/Seoul"));

    // 세션 ID -> WebSocketSession
    private final Map<String, WebSocketSession> sessions = new ConcurrentHashMap<>();

    // 회원 ID -> WebSocketSession
    private final Map<Long, WebSocketSession> memberSessions = new ConcurrentHashMap<>();

    // 세션 ID -> 회원 ID
    private final Map<String, Long> sessionMemberMap = new ConcurrentHashMap<>();

    private final Map<String, Long> lastPingTimeMap = new ConcurrentHashMap<>();
    private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();

    // 클래스 초기화 시 스케줄러 시작
    @PostConstruct
    public void init() {
        // 30초마다 비활성 연결 검사
        scheduler.scheduleAtFixedRate(this::checkInactiveConnections, 30, 30, TimeUnit.SECONDS);
    }

    // 애플리케이션 종료 시 스케줄러 종료
    @PreDestroy
    public void destroy() {
        scheduler.shutdown();
    }

    @Override
    public void afterConnectionEstablished(WebSocketSession session) throws Exception {
        // 이전 같은 ID를 가진 세션이 있는지 확인
        String sessionId = session.getId();
//        log.info("웹소켓 연결 시도: sessionId={}", sessionId);

        // 기존 연결된 세션이 있으면 제거
        WebSocketSession existingSession = sessions.get(sessionId);
        if (existingSession != null && existingSession != session) {
            try {
//                log.info("이미 존재하는 세션 교체: sessionId={}", sessionId);
                existingSession.close(new CloseStatus(CloseStatus.SESSION_NOT_RELIABLE.getCode(), "새 연결로 대체"));
            } catch (Exception e) {
                log.warn("기존 세션 종료 중 오류 발생: {}", e.getMessage());
            }
        }

        sessions.put(sessionId, session);
        lastPingTimeMap.put(sessionId, System.currentTimeMillis()); // 핑 시간 초기화


        // 연결 성공 메시지 전송
        sendConnectedMessage(session);

        // 현재 연결된 사용자 수를 브로드캐스트
        broadcastActiveUserCount();
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) throws Exception {
        String payload = message.getPayload();
        log.debug("메시지 수신: sessionId={}, payload={}", session.getId(), payload);

        try {
            JsonNode jsonNode = objectMapper.readTree(payload);
            String type = jsonNode.get("type").asText();

            switch (type) {
                case "authenticate":
                    handleAuthentication(session, jsonNode);
                    break;
                case "chat":
                    handleChatMessage(session, jsonNode);
                    break;
                case "getNotifications":
                    handleGetNotifications(session, jsonNode);
                    break;
                case "markAsRead":
                    handleMarkAsRead(session, jsonNode);
                    break;
                case "markAllAsRead":  // 새로운 case 추가
                    handleMarkAllAsRead(session);
                    break;
                case "ping":
                    handlePing(session);
                    break;
                case "getUserCount":
                    sendUserCountMessage(session);
                    break;
                default:
                    log.warn("알 수 없는 메시지 유형: {}", type);
            }
        } catch (Exception e) {
            log.error("메시지 처리 중 오류 발생: {}", e.getMessage(), e);
            sendErrorMessage(session, "메시지 처리 중 오류 발생: " + e.getMessage());
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) throws Exception {
        String sessionId = session.getId();
        Long memberId = sessionMemberMap.get(sessionId);

        log.info("웹소켓 연결 종료: sessionId={}, status={}, memberId={}", sessionId, status, memberId);

        try {
            if (memberId != null) {
                memberSessions.remove(memberId);
                try {
                    sendUserLeftMessage(memberId);
                } catch (Exception e) {
                    log.error("사용자 퇴장 메시지 전송 중 오류: {}", e.getMessage());
                }
            }
        } finally {
            // 세션 정보 정리는 항상 실행되도록 finally 블록에 배치
            sessions.remove(sessionId);
            sessionMemberMap.remove(sessionId);
            lastPingTimeMap.remove(sessionId);

            // 사용자 수 브로드캐스팅은 모든 정리 작업 후에 실행
            try {
                broadcastActiveUserCount();
            } catch (Exception e) {
                log.error("사용자 수 브로드캐스팅 중 오류: {}", e.getMessage());
            }
        }
    }

    // ChatWebSocketHandler.java의 sendMessageToUser 메서드 개선
    public void sendMessageToUser(Long memberId, String message) {
        if (memberId == null) {
            log.error("회원 ID가 null입니다. 메시지를 전송할 수 없습니다.");
            return;
        }

        WebSocketSession session = memberSessions.get(memberId);
        log.info("회원 ID {} 메시지 전송 시도, 세션 존재 여부: {}", memberId, session != null);

        if (session != null) {
            log.info("세션 상태: isOpen={}, id={}", session.isOpen(), session.getId());

            if (session.isOpen()) {
                try {
                    // 메시지 유효성 확인
                    if (message == null || message.trim().isEmpty()) {
                        log.error("전송할 메시지가 비어 있습니다.");
                        return;
                    }

                    // TextMessage 생성 및 전송
                    TextMessage textMessage = new TextMessage(message);
//                    log.debug("TextMessage 생성 완료: length={}", textMessage.getPayloadLength());

                    session.sendMessage(textMessage);
//                    log.info("회원 ID {}에게 메시지 전송 성공", memberId);
                } catch (IOException e) {
                    log.error("메시지 전송 중 IO 오류 발생: memberId={}, error={}", memberId, e.getMessage(), e);
                } catch (Exception e) {
                    log.error("메시지 전송 중 예상치 못한 오류 발생: memberId={}, error={}", memberId, e.getMessage(), e);
                }
            } else {
                log.warn("세션이 닫혀 있어 메시지를 전송할 수 없습니다: memberId={}", memberId);
                // 닫힌 세션은 맵에서 제거
                memberSessions.remove(memberId);
                log.info("닫힌 세션을 memberSessions에서 제거했습니다: memberId={}", memberId);
            }
        } else {
            log.warn("회원 ID {}에 대한 세션이 없어 메시지를 전송할 수 없습니다", memberId);
        }
    }

    // 인증 처리
    private void handleAuthentication(WebSocketSession session, JsonNode jsonNode) throws IOException {
        try {
            String token = jsonNode.get("token").asText().replace("Bearer ", "");
            if (jwtTokenProvider.validateToken(token)) {
                Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
                Member member = memberRepository.findById(memberId)
                        .orElseThrow(() -> new RuntimeException("회원을 찾을 수 없습니다"));

                // 세션 정보 업데이트
                memberSessions.put(memberId, session);
                sessionMemberMap.put(session.getId(), memberId);

                // 인증 성공 메시지 전송
                sendAuthenticatedMessage(session, member);

                // 사용자 입장 메시지 브로드캐스트
                sendUserJoinedMessage(member);

                // 미확인 알림 전송
                sendUnreadNotifications(memberId, session);

                log.info("웹소켓 인증 성공: memberId={}, nickname={}", memberId, member.getNickname());

                // 인증 성공 후 현재 연결된 사용자 수를 브로드캐스트
                broadcastActiveUserCount();
            } else {
                sendErrorMessage(session, "유효하지 않은 토큰입니다");
                log.warn("웹소켓 인증 실패: 유효하지 않은 토큰");
            }
        } catch (Exception e) {
            sendErrorMessage(session, "인증 처리 중 오류 발생: " + e.getMessage());
            log.error("웹소켓 인증 처리 중 오류 발생", e);
        }
    }

    // 채팅 메시지 처리
    private void handleChatMessage(WebSocketSession session, JsonNode jsonNode) throws IOException {
        Long memberId = sessionMemberMap.get(session.getId());
        if (memberId == null) {
            sendErrorMessage(session, "인증이 필요합니다");
            return;
        }

        String chatMessage = jsonNode.get("message").asText();

        // 회원 정보 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new RuntimeException("회원을 찾을 수 없습니다"));

        // 채팅 메시지 브로드캐스트
        broadcastChatMessage(member, chatMessage);
    }

    // 알림 목록 요청 처리
    private void handleGetNotifications(WebSocketSession session, JsonNode jsonNode) throws IOException {
        Long memberId = sessionMemberMap.get(session.getId());
        if (memberId == null) {
            sendErrorMessage(session, "인증이 필요합니다");
            return;
        }

        String filter = jsonNode.get("filter").asText();
        List<Notification> notifications;

        if ("unread".equals(filter)) {
            notifications = notificationRepository.findByMemberIdAndIsReadFalseOrderByCreatedAtDesc(memberId);
        } else {
            notifications = notificationRepository.findByMemberIdOrderByCreatedAtDesc(memberId);
        }

        sendNotificationList(session, notifications);
    }

    // 알림 읽음 처리
    private void handleMarkAsRead(WebSocketSession session, JsonNode jsonNode) throws IOException {
        Long memberId = sessionMemberMap.get(session.getId());
        if (memberId == null) {
            sendErrorMessage(session, "인증이 필요합니다");
            return;
        }

        Long notificationId = jsonNode.get("notificationId").asLong();

        try {
            notificationRepository.findById(notificationId).ifPresent(notification -> {
                if (notification.getMemberId().equals(memberId)) {
                    notification.markAsRead();
                    notificationRepository.save(notification);
                }
            });

            sendNotificationReadResponse(session, notificationId, true);
        } catch (Exception e) {
            log.error("알림 읽음 처리 중 오류 발생", e);
            sendNotificationReadResponse(session, notificationId, false);
        }
    }

    // 모든 알림 읽음 처리
    private void handleMarkAllAsRead(WebSocketSession session) throws IOException {
        Long memberId = sessionMemberMap.get(session.getId());
        if (memberId == null) {
            sendErrorMessage(session, "인증이 필요합니다");
            return;
        }

        try {
            // 회원의 모든 미확인 알림 조회
            List<Notification> unreadNotifications = notificationRepository
                    .findByMemberIdAndIsReadFalseOrderByCreatedAtDesc(memberId);

            // 모든 알림에 읽음 처리
            unreadNotifications.forEach(Notification::markAsRead);
            notificationRepository.saveAll(unreadNotifications);

            // 읽음 처리된 알림 ID 목록
            List<Long> readNotificationIds = unreadNotifications.stream()
                    .map(Notification::getId)
                    .toList();

            // 읽음 처리 응답 전송
            sendAllNotificationsReadResponse(session, readNotificationIds, true);

//            log.info("회원 ID {}의 모든 알림({}) 읽음 처리 완료", memberId, unreadNotifications.size());
        } catch (Exception e) {
            log.error("모든 알림 읽음 처리 중 오류 발생", e);
            sendErrorMessage(session, "모든 알림 읽음 처리 중 오류 발생: " + e.getMessage());
        }
    }

    // 모든 알림 읽음 처리 응답 전송
    private void sendAllNotificationsReadResponse(WebSocketSession session, List<Long> notificationIds, boolean success) throws IOException {
        Map<String, Object> message = Map.of(
                "type", "allNotificationsRead",
                "notificationIds", notificationIds,
                "success", success
        );

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
    }

    // Ping 처리
    private void handlePing(WebSocketSession session) throws IOException {
        // 마지막 핑 시간 업데이트
        lastPingTimeMap.put(session.getId(), System.currentTimeMillis());

        Map<String, Object> pongMessage = Map.of(
                "type", "pong",
                "timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter)
        );

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(pongMessage)));
    }

    // 비활성 연결 검사 메서드
    private void checkInactiveConnections() {
        long currentTime = System.currentTimeMillis();
        boolean needBroadcast = false;

        // 세션 ID 목록 복사 (ConcurrentModificationException 방지)
        List<String> sessionIds = new ArrayList<>(sessions.keySet());

        for (String sessionId : sessionIds) {
            // 마지막 핑 시간 확인
            Long lastPingTime = lastPingTimeMap.get(sessionId);

            // 60초 이상 핑이 없으면 연결이 끊긴 것으로 간주
            if (lastPingTime == null || (currentTime - lastPingTime) > 60000) {
                WebSocketSession session = sessions.get(sessionId);
                if (session != null) {
                    try {
                        Long memberId = sessionMemberMap.get(sessionId);

                        if (memberId != null) {
                            memberSessions.remove(memberId);
                            try {
                                sendUserLeftMessage(memberId);
                            } catch (IOException e) {
                                log.error("사용자 퇴장 메시지 전송 실패: {}", e.getMessage());
                            }
                            log.info("비활성 사용자 제거: memberId={}", memberId);
                        }

                        sessions.remove(sessionId);
                        sessionMemberMap.remove(sessionId);
                        lastPingTimeMap.remove(sessionId);

                        session.close(new CloseStatus(CloseStatus.SESSION_NOT_RELIABLE.getCode(), "연결 시간 초과"));
                        needBroadcast = true;

                        log.info("비활성 연결 종료: sessionId={}", sessionId);
                    } catch (Exception e) {
                        log.error("비활성 연결 종료 중 오류 발생: {}", e.getMessage());
                    }
                }
            }
        }

        // 연결이 변경된 경우에만 브로드캐스트
        if (needBroadcast) {
            try {
                broadcastActiveUserCount();
            } catch (Exception e) {
                log.error("비활성 연결 정리 후 브로드캐스트 중 오류 발생: {}", e.getMessage());
            }
        }
    }

    private void broadcastActiveUserCount() {
        try {
            // 실제로 연결된 인증된 사용자 수 확인 (세션이 살아있는지 추가 검증)
            int authenticatedUserCount = 0;
            for (Map.Entry<Long, WebSocketSession> entry : memberSessions.entrySet()) {
                if (entry.getValue() != null && entry.getValue().isOpen()) {
                    authenticatedUserCount++;
                } else {
                    // 죽은 세션 발견 시 맵에서 제거
                    memberSessions.remove(entry.getKey());
                }
            }

            // 전체 연결 세션 수 검증
            int totalConnections = 0;
            for (WebSocketSession session : sessions.values()) {
                if (session != null && session.isOpen()) {
                    totalConnections++;
                }
            }

            Map<String, Object> message = Map.of(
                    "type", "userCount",
                    "authenticatedCount", authenticatedUserCount,
                    "totalCount", totalConnections,
                    "timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter)
            );

            broadcastMessage(objectMapper.writeValueAsString(message));
            log.info("활성 사용자 수 브로드캐스트: 인증된 사용자={}, 전체 연결={}", authenticatedUserCount, totalConnections);
        } catch (IOException e) {
            log.error("활성 사용자 수 브로드캐스트 중 오류 발생", e);
        }
    }

    // 특정 세션에 현재 사용자 수 전송
    private void sendUserCountMessage(WebSocketSession session) throws IOException {
        int authenticatedUserCount = memberSessions.size();
        int totalConnections = sessions.size();

        Map<String, Object> message = Map.of(
                "type", "userCount",
                "authenticatedCount", authenticatedUserCount,
                "totalCount", totalConnections,
                "timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter)
        );

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
    }

    // 연결 성공 메시지 전송
    private void sendConnectedMessage(WebSocketSession session) throws IOException {
        Map<String, Object> message = Map.of(
                "type", "connected",
                "message", "채팅 및 알림 서버에 연결되었습니다. 인증이 필요합니다."
        );

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
    }

    // 인증 성공 메시지 전송
    private void sendAuthenticatedMessage(WebSocketSession session, Member member) throws IOException {
        Map<String, Object> message = Map.of(
                "type", "authenticated",
                "nickname", member.getNickname(),
                "memberId", member.getMemberId()
        );

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
    }

    // 사용자 입장 메시지 전송
    private void sendUserJoinedMessage(Member member) throws IOException {
        Map<String, Object> message = Map.of(
                "type", "userJoined",
                "nickname", member.getNickname(),
                "timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter) // 🌐 [KST 적용]
        );

        broadcastMessage(objectMapper.writeValueAsString(message));
    }

    // 사용자 퇴장 메시지 전송
    private void sendUserLeftMessage(Long memberId) throws IOException {
        Member member = memberRepository.findById(memberId).orElse(null);
        if (member != null) {
            Map<String, Object> message = Map.of(
                    "type", "userLeft",
                    "nickname", member.getNickname(),
                    "timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter) // 🌐 [KST 적용]

            );

            broadcastMessage(objectMapper.writeValueAsString(message));
        }
    }

    // 미확인 알림 전송
    private void sendUnreadNotifications(Long memberId, WebSocketSession session) throws IOException {
        List<Notification> unreadNotifications = notificationRepository
                .findByMemberIdAndIsReadFalseOrderByCreatedAtDesc(memberId);

        for (Notification notification : unreadNotifications) {
            Map<String, Object> payload = Map.of(
                    "type", "notification",
                    "notificationType", notification.getType(),
                    "title", notification.getTitle(),
                    "message", notification.getMessage(),
                    "timestamp", notification.getCreatedAt().atZone(ZoneId.of("Asia/Seoul")).format(formatter), // 🌐 [KST 적용]
                    "notificationId", notification.getId(),
                    "isRead", notification.isRead()
            );

            session.sendMessage(new TextMessage(objectMapper.writeValueAsString(payload)));
        }
    }

    // 채팅 메시지 브로드캐스트
    private void broadcastChatMessage(Member member, String chatMessage) throws IOException {
        Map<String, Object> message = Map.of(
                "type", "chat",
                "memberId", member.getMemberId(),
                "nickname", member.getNickname(),
                "message", chatMessage,
                "timestamp", ZonedDateTime.now(ZoneId.of("Asia/Seoul")).format(formatter) // 🌐 [KST 적용]
        );

        broadcastMessage(objectMapper.writeValueAsString(message));
    }

    // 알림 목록 전송
    private void sendNotificationList(WebSocketSession session, List<Notification> notifications) throws IOException {
        Map<String, Object> message = Map.of(
                "type", "notificationList",
                "notifications", notifications.stream().map(this::convertNotificationToMap).toList()
        );

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
    }

    // 알림 읽음 처리 응답 전송
    private void sendNotificationReadResponse(WebSocketSession session, Long notificationId, boolean success) throws IOException {
        Map<String, Object> message = Map.of(
                "type", "notificationRead",
                "notificationId", notificationId,
                "success", success
        );

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
    }

    // 에러 메시지 전송
    private void sendErrorMessage(WebSocketSession session, String errorMessage) throws IOException {
        Map<String, Object> message = Map.of(
                "type", "error",
                "message", errorMessage
        );

        session.sendMessage(new TextMessage(objectMapper.writeValueAsString(message)));
    }

    // 전체 브로드캐스트
    private void broadcastMessage(String message) {
        sessions.values().forEach(session -> {
            if (session.isOpen()) {
                try {
                    session.sendMessage(new TextMessage(message));
                } catch (IOException e) {
                    log.error("메시지 브로드캐스트 실패: sessionId={}", session.getId(), e);
                }
            }
        });
    }

    // Notification 엔티티를 Map으로 변환
    private Map<String, Object> convertNotificationToMap(Notification notification) {
        Map<String, Object> map = Map.of(
                "notificationId", notification.getId(),
                "notificationType", notification.getType(),
                "title", notification.getTitle(),
                "message", notification.getMessage(),
                "timestamp", notification.getCreatedAt().atZone(ZoneId.of("Asia/Seoul")).format(formatter), // 🌐 [KST 적용]
                "isRead", notification.isRead()
        );

        if (notification.getRelatedId() != null) {
            ((Map)map).put("relatedId", notification.getRelatedId());
        }

        return map;
    }

    // 회원에 대한 웹소켓 세션이 존재하는지 확인하는 메서드
    public boolean hasSession(Long memberId) {
        WebSocketSession session = memberSessions.get(memberId);
        return session != null && session.isOpen();
    }
}