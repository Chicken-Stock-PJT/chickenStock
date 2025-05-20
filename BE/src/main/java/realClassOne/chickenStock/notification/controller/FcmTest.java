//package realClassOne.chickenStock.notification.controller;
//
//import lombok.RequiredArgsConstructor;
//import org.springframework.http.HttpStatus;
//import org.springframework.http.ResponseEntity;
//import org.springframework.web.bind.annotation.GetMapping;
//import org.springframework.web.bind.annotation.RequestMapping;
//import org.springframework.web.bind.annotation.RequestParam;
//import org.springframework.web.bind.annotation.RestController;
//import realClassOne.chickenStock.notification.service.FCMService;
//import realClassOne.chickenStock.notification.service.FCMTokenService;
//
//import java.util.HashMap;
//import java.util.Map;
//
//@RestController
//@RequestMapping("/api/notification/")
//@RequiredArgsConstructor
//public class FcmTest {
//
//    private final FCMService fcmService;
//    private final FCMTokenService fcmTokenService;
//
//    @GetMapping("/test-fcm")
//    public ResponseEntity<?> testFCM(@RequestParam Long memberId) {
//        try {
//            Map<String, String> data = new HashMap<>();
//            data.put("type", "TEST");
//            data.put("key1", "value1");
//            data.put("timestamp", String.valueOf(System.currentTimeMillis()));
//
//            boolean success = fcmService.sendTestMessage(
//                    memberId,
//                    "테스트 알림",
//                    "효성아 도착하면 바로 MM좀 ㅜㅠ",
//                    data
//            );
//
//            if (success) {
//                return ResponseEntity.ok("FCM 테스트 알림이 성공적으로 전송되었습니다.");
//            } else {
//                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
//                        .body("FCM 테스트 알림 전송 실패: 유효한 FCM 토큰이 없거나 전송 중 오류가 발생했습니다.");
//            }
//        } catch (Exception e) {
//            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
//                    .body("FCM 알림 발송 오류: " + e.getMessage());
//        }
//    }
//
//    // 직접 토큰으로 FCM 메시지를 전송하는 테스트 API
//    @GetMapping("/test-direct-token")
//    public ResponseEntity<?> testDirectToken(@RequestParam String token) {
//        try {
//            // 요청된 FCM 메시지 형식에 맞게 데이터 구성
//            Map<String, String> data = new HashMap<>();
//            data.put("type", "DIRECT_TEST");
//            data.put("key1", "value1");
//            data.put("timestamp", String.valueOf(System.currentTimeMillis()));
//
//            // FCM 메시지 직접 전송
//            String title = "토큰 테스트";
//            String body = "효성아 도착하면 바로 mm줘!! - " + System.currentTimeMillis();
//
//            // FCMService의 sendMessageToToken 메서드 사용
//            boolean success = fcmService.sendMessageToToken(
//                    token,
//                    title,
//                    body,
//                    data
//            );
//
//            // 응답 구성 - FCM 형식과 동일하게 할 필요는 없음
//            if (success) {
//                // FCM 형식으로 보낸 메시지 내용을 응답에 포함
//                Map<String, Object> fcmMessage = new HashMap<>();
//                fcmMessage.put("to", token);
//
//                Map<String, String> notification = new HashMap<>();
//                notification.put("title", title);
//                notification.put("body", body);
//                fcmMessage.put("notification", notification);
//
//                fcmMessage.put("data", data);
//
//                return ResponseEntity.ok(Map.of(
//                        "success", true,
//                        "message", "FCM 메시지 전송 성공",
//                        "fcmMessage", fcmMessage  // 사용된 FCM 메시지 형식을 응답에 포함
//                ));
//            } else {
//                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
//                        .body(Map.of(
//                                "success", false,
//                                "message", "FCM 메시지 전송 실패",
//                                "token", token
//                        ));
//            }
//        } catch (Exception e) {
//            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
//                    .body(Map.of(
//                            "success", false,
//                            "message", "오류 발생: " + e.getMessage(),
//                            "token", token
//                    ));
//        }
//    }
//}