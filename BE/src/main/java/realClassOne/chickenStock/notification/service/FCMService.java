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
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

@Slf4j
@Service
@RequiredArgsConstructor
public class FCMService {

    @Value("${firebase.config.project-id}")
    private String projectId;

    private final FCMTokenService fcmTokenService;

    @PostConstruct
    public void initialize() {
        try {
            if (FirebaseApp.getApps().isEmpty()) {
                FirebaseOptions options = FirebaseOptions.builder()
                        .setCredentials(GoogleCredentials.getApplicationDefault())
                        .setProjectId(projectId)
                        .build();

                FirebaseApp.initializeApp(options);
            }
        } catch (IOException e) {
            log.error("Firebase 초기화 중 오류 발생", e);
        }
    }

    /**
     * FCM을 통해 지정가 체결 알림 전송 (사용자 ID 기반)
     */
    public void sendTradeNotificationToMember(Long memberId, String stockName, String orderType,
                                              Integer quantity, Long price) {
        try {
            // 회원의 FCM 토큰 목록 조회
            List<String> tokens = fcmTokenService.getUserTokens(memberId);

            if (tokens.isEmpty()) {
                log.debug("회원 ID {}의 FCM 토큰이 없어 푸시 알림을 보낼 수 없습니다.", memberId);
                return;
            }

            // 토큰 목록으로 알림 전송
            BatchResponse response = sendTradeNotification(tokens, stockName, orderType, quantity, price);

            // 무효한 토큰 정리 로직 추가
            if (response != null && response.getFailureCount() > 0) {
                List<SendResponse> responses = response.getResponses();
                for (int i = 0; i < responses.size(); i++) {
                    if (!responses.get(i).isSuccessful()) {
                        SendResponse failedResponse = responses.get(i);
                        String failedToken = tokens.get(i);

                        // FirebaseMessagingException에서 유효하지 않은 토큰 판별
                        FirebaseMessagingException error = failedResponse.getException();
                        if (error != null &&
                                (error.getMessagingErrorCode() == MessagingErrorCode.INVALID_ARGUMENT ||
                                        error.getMessagingErrorCode() == MessagingErrorCode.UNREGISTERED)) {
                            // 무효한 토큰 제거
                            log.info("무효한 FCM 토큰 삭제: {}", failedToken);
                            fcmTokenService.removeUserToken(memberId, failedToken);
                        }
                    }
                }
            }

//            log.info("FCM 지정가 체결 알림 전송 완료: 회원ID={}, 종목={}, 토큰 수={}",
//                    memberId, stockName, tokens.size());

        } catch (Exception e) {
            log.error("회원 FCM 알림 전송 중 오류 발생", e);
        }
    }

    /**
     * 수정: FCM을 통해 지정가 체결 알림 전송 (토큰 목록 기반) - 응답 반환
     */
    public BatchResponse sendTradeNotification(List<String> tokens, String stockName, String orderType,
                                               Integer quantity, Long price) {
        if (tokens == null || tokens.isEmpty()) {
            log.warn("FCM 토큰이 없어 알림을 전송할 수 없습니다.");
            return null;
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
            return sendMessageToTokens(tokens, title, body, data);
        } catch (Exception e) {
            log.error("FCM 메시지 전송 중 예외 발생", e);
            return null;
        }
    }

    /**
     * 단일 기기 알림 전송
     */
    public void sendMessageToToken(String token, String title, String body, Map<String, String> data) {
        Message message = Message.builder()
                .setNotification(Notification.builder()
                        .setTitle(title)
                        .setBody(body)
                        .build())
                .putAllData(data)
                .setToken(token)
                .build();

        try {
            String response = FirebaseMessaging.getInstance().sendAsync(message).get();
//            log.info("FCM 메시지 전송 성공: {}", response);
        } catch (InterruptedException | ExecutionException e) {
            log.error("FCM 메시지 전송 실패", e);
            Thread.currentThread().interrupt();
        }
    }

    /**
     * 여러 기기에 동시 알림 전송 (BatchResponse 반환)
     */
    public BatchResponse sendMessageToTokens(List<String> tokens, String title, String body, Map<String, String> data) {
        if (tokens == null || tokens.isEmpty()) {
            log.warn("FCM 토큰 목록이 비어 있습니다. 알림을 보낼 수 없습니다.");
            return null;
        }

        MulticastMessage message = MulticastMessage.builder()
                .setNotification(Notification.builder()
                        .setTitle(title)
                        .setBody(body)
                        .build())
                .putAllData(data)
                .addAllTokens(tokens)
                .build();

        try {
            BatchResponse response = FirebaseMessaging.getInstance().sendMulticastAsync(message).get();
//            log.info("FCM 메시지 전송 완료: 성공 {}, 실패 {}",
//                    response.getSuccessCount(), response.getFailureCount());

            if (response.getFailureCount() > 0) {
                List<SendResponse> responses = response.getResponses();
                for (int i = 0; i < responses.size(); i++) {
                    if (!responses.get(i).isSuccessful()) {
                        log.warn("FCM 메시지 전송 실패: 토큰={}, 오류={}",
                                tokens.get(i), responses.get(i).getException().getMessage());
                    }
                }
            }

            return response;
        } catch (InterruptedException | ExecutionException e) {
            log.error("FCM 메시지 전송 중 예외 발생", e);
            Thread.currentThread().interrupt();
            return null;
        }
    }
}