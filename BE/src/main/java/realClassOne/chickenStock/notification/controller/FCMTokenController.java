package realClassOne.chickenStock.notification.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.notification.request.FCMTokenRequestDTO;
import realClassOne.chickenStock.notification.service.FCMTokenService;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.util.Map;

@RestController
@RequestMapping("/api/notification/fcm")
@RequiredArgsConstructor
@Slf4j
public class FCMTokenController {

    private final FCMTokenService fcmTokenService;
    private final JwtTokenProvider jwtTokenProvider;

    @PostMapping("/token")
    public ResponseEntity<?> registerToken(
            @RequestHeader("Authorization") String authHeader,
            @RequestBody FCMTokenRequestDTO request) {

        try {
            String token = jwtTokenProvider.resolveToken(authHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            fcmTokenService.saveUserToken(memberId, request.getToken());

            return ResponseEntity.ok().body(Map.of(
                    "success", true,
                    "message", "FCM 토큰이 성공적으로 등록되었습니다."
            ));
        } catch (Exception e) {
            log.error("FCM 토큰 등록 중 오류 발생", e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "message", "FCM 토큰 등록 실패: " + e.getMessage()
            ));
        }
    }

    @PostMapping("/token")
    public ResponseEntity<?> unregisterToken(
            @RequestHeader("Authorization") String authHeader,
            @RequestBody FCMTokenRequestDTO request) {

        try {
            String token = jwtTokenProvider.resolveToken(authHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            fcmTokenService.removeUserToken(memberId, request.getToken());

            return ResponseEntity.ok().body(Map.of(
                    "success", true,
                    "message", "FCM 토큰이 성공적으로 삭제되었습니다."
            ));
        } catch (Exception e) {
            log.error("FCM 토큰 삭제 중 오류 발생", e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "message", "FCM 토큰 삭제 실패: " + e.getMessage()
            ));
        }
    }
}