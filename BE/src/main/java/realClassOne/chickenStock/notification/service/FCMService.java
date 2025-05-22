package realClassOne.chickenStock.notification.service;

import com.google.auth.oauth2.GoogleCredentials;
import com.google.firebase.FirebaseApp;
import com.google.firebase.FirebaseOptions;
import com.google.firebase.messaging.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
@RequiredArgsConstructor
public class FCMService {

    @Value("${firebase.config.project-id}")
    private String projectId;

    @Value("${firebase.config.type}")
    private String type;

    @Value("${firebase.config.private-key-id}")
    private String privateKeyId;

    @Value("${firebase.config.private-key}")
    private String privateKey;

    @Value("${firebase.config.client-email}")
    private String clientEmail;

    @Value("${firebase.config.client-id}")
    private String clientId;

    @Value("${firebase.config.auth-uri}")
    private String authUri;

    @Value("${firebase.config.token-uri}")
    private String tokenUri;

    @Value("${firebase.config.auth-provider-x509-cert-url}")
    private String authProviderCertUrl;

    @Value("${firebase.config.client-x509-cert-url}")
    private String clientCertUrl;

    @Value("${firebase.config.universe_domain}")
    private String universeDomain;

    private final FCMTokenService fcmTokenService;

    @PostConstruct
    public void initialize() {
        try {
            if (FirebaseApp.getApps().isEmpty()) {
                // yml 파일의 설정으로 JSON 형식의 서비스 계정 키를 생성
                String serviceAccountJson = String.format(
                        "{\n" +
                                "  \"type\": \"%s\",\n" +
                                "  \"project_id\": \"%s\",\n" +
                                "  \"private_key_id\": \"%s\",\n" +
                                "  \"private_key\": \"%s\",\n" +
                                "  \"client_email\": \"%s\",\n" +
                                "  \"client_id\": \"%s\",\n" +
                                "  \"auth_uri\": \"%s\",\n" +
                                "  \"token_uri\": \"%s\",\n" +
                                "  \"auth_provider_x509_cert_url\": \"%s\",\n" +
                                "  \"client_x509_cert_url\": \"%s\",\n" +
                                "  \"universe_domain\": \"%s\"\n" +
                                "}",
                        type, projectId, privateKeyId, privateKey.replace("\\n", "\n"),
                        clientEmail, clientId, authUri, tokenUri,
                        authProviderCertUrl, clientCertUrl, universeDomain
                );

                log.info("Firebase 초기화 시작 - projectId: {}", projectId);

                // 생성된 JSON 문자열로부터 GoogleCredentials 객체 생성
                GoogleCredentials credentials = GoogleCredentials.fromStream(
                        new ByteArrayInputStream(serviceAccountJson.getBytes())
                );

                // Firebase 초기화
                FirebaseOptions options = FirebaseOptions.builder()
                        .setCredentials(credentials)
                        .setProjectId(projectId)
                        .build();

                FirebaseApp.initializeApp(options);
                log.info("Firebase 초기화 성공");
            }
        } catch (IOException e) {
            log.error("Firebase 초기화 중 오류 발생", e);
        }
    }

    /**
     * FCM을 통해 지정가 체결 알림 전송 (사용자 ID 기반)
     */
    public boolean sendTradeNotificationToMember(Long memberId, String stockName, String orderType,
                                                 Integer quantity, Long price) {
        try {
            // 회원의 FCM 토큰 목록 조회
            List<String> tokens = fcmTokenService.getUserTokens(memberId);

            if (tokens.isEmpty()) {
                log.debug("회원 ID {}의 FCM 토큰이 없어 푸시 알림을 보낼 수 없습니다.", memberId);
                return false;
            }

            // 토큰 목록으로 알림 전송
            boolean result = sendTradeNotification(memberId, tokens, stockName, orderType, quantity, price);

            if (result) {
                log.info("FCM 지정가 체결 알림 전송 성공: 회원ID={}, 종목={}", memberId, stockName);
            } else {
                log.warn("FCM 지정가 체결 알림 전송 실패 또는 부분 성공: 회원ID={}, 종목={}", memberId, stockName);
            }

            return result;
        } catch (Exception e) {
            log.error("회원 FCM 알림 전송 중 오류 발생", e);
            return false;
        }
    }

    /**
     * FCM을 통해 지정가 체결 알림 전송 (토큰 목록 기반) - 성공 여부 반환
     */
    public boolean sendTradeNotification(Long memberId, List<String> tokens, String stockName, String orderType,
                                         Integer quantity, Long price) {
        if (tokens == null || tokens.isEmpty()) {
            log.warn("FCM 토큰이 없어 알림을 전송할 수 없습니다.");
            return false;
        }

        try {
            boolean isBuy = "BUY".equalsIgnoreCase(orderType);
            String title = "지정가 체결 알림";
            String body = String.format("%s %s %d주가 %,d원에 체결되었습니다.",
                    stockName, isBuy ? "매수" : "매도", quantity, price);

            // 추가 데이터 구성
            Map<String, String> data = new HashMap<>();
            data.put("type", "TRADE");
            data.put("stockName", stockName);
            data.put("orderType", orderType);
            data.put("quantity", quantity.toString());
            data.put("price", price.toString());

            // 여러 기기에 동시 알림 전송
            return sendMessageToTokens(memberId, tokens, title, body, data);
        } catch (Exception e) {
            log.error("FCM 메시지 전송 중 예외 발생", e);
            return false;
        }
    }

    /**
     * 단일 기기 알림 전송
     */
    public boolean sendMessageToToken(String token, String title, String body, Map<String, String> data) {
        try {
            Message message = Message.builder()
                    .setNotification(Notification.builder()
                            .setTitle(title)
                            .setBody(body)
                            .build())
                    .putAllData(data)
                    .setToken(token)
                    .build();

            // 비동기 대신 동기 방식으로 전송
            String response = FirebaseMessaging.getInstance().send(message);
            log.info("FCM 메시지 전송 성공: 토큰={}, 응답={}", token, response);
            return true;
        } catch (FirebaseMessagingException e) {
            log.error("FCM 메시지 전송 실패: 토큰={}, 오류={}", token, e.getMessage());
            return false;
        }
    }

    /**
     * 여러 기기에 개별적으로 알림 전송
     */
    public boolean sendMessageToTokens(Long memberId, List<String> tokens, String title, String body, Map<String, String> data) {
        if (tokens == null || tokens.isEmpty()) {
            log.warn("FCM 토큰 목록이 비어 있습니다. 알림을 보낼 수 없습니다.");
            return false;
        }

        log.info("FCM 메시지 전송 시작: 토큰 수={}, 제목='{}'", tokens.size(), title);

        int successCount = 0;
        List<String> invalidTokens = new ArrayList<>();

        // 각 토큰에 대해 개별적으로 메시지 전송
        for (String token : tokens) {
            try {
                Message message = Message.builder()
                        .setNotification(com.google.firebase.messaging.Notification.builder()
                                .setTitle(title)
                                .setBody(body)
                                .build())
                        .putAllData(data)
                        .setToken(token)
                        .build();

                // 동기식으로 전송
                String response = FirebaseMessaging.getInstance().send(message);
                successCount++;
                log.info("FCM 메시지 전송 성공: 토큰={}, 응답={}", token, response);
            } catch (FirebaseMessagingException e) {
                // 토큰이 더 이상 유효하지 않은 경우 자동으로 제거
                if (e.getMessagingErrorCode() == MessagingErrorCode.INVALID_ARGUMENT ||
                        e.getMessagingErrorCode() == MessagingErrorCode.UNREGISTERED) {
                    log.warn("무효한 FCM 토큰 발견: {}, 오류: {}", token, e.getMessage());
                    invalidTokens.add(token);

                    // 무효한 토큰 제거
                    if (memberId != null) {
                        fcmTokenService.removeUserToken(memberId, token);
                    }
                } else {
                    log.error("FCM 메시지 전송 실패: 토큰={}, 오류={}", token, e.getMessage());
                }
            }
        }

        // 무효한 토큰을 자동으로 제거한 결과 보고
        if (!invalidTokens.isEmpty()) {
            log.info("FCM 메시지 전송 중 무효한 토큰 {}개가 자동으로 제거되었습니다.", invalidTokens.size());
        }

        boolean allSuccess = successCount == tokens.size();
        log.info("FCM 메시지 전송 완료: 성공={}, 실패={}", successCount, tokens.size() - successCount);

        return successCount > 0; // 하나 이상 성공하면 true 반환
    }

    /**
     * 테스트용 메시지 전송
     */
    public boolean sendTestMessage(Long memberId, String title, String body, Map<String, String> data) {
        try {
            // 회원의 FCM 토큰 목록 조회
            List<String> tokens = fcmTokenService.getUserTokens(memberId);

            if (tokens.isEmpty()) {
                log.debug("회원 ID {}의 FCM 토큰이 없어 테스트 알림을 보낼 수 없습니다.", memberId);
                return false;
            }

            // 토큰 목록으로 알림 전송
            boolean success = sendMessageToTokens(memberId, tokens, title, body, data);

            if (success) {
                log.info("FCM 테스트 알림 전송 성공: 회원ID={}", memberId);
            } else {
                log.warn("FCM 테스트 알림 전송 실패: 회원ID={}", memberId);
            }

            return success;
        } catch (Exception e) {
            log.error("테스트 알림 전송 중 오류 발생", e);
            return false;
        }
    }
}