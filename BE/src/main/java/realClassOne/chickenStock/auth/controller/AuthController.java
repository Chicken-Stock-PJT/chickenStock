package realClassOne.chickenStock.auth.controller;

import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import realClassOne.chickenStock.auth.dto.common.TokenDto;
import realClassOne.chickenStock.auth.dto.common.WebTokenResponseDTO;
import realClassOne.chickenStock.auth.dto.request.*;
import realClassOne.chickenStock.auth.dto.response.PasswordResetResponseDTO;
import realClassOne.chickenStock.auth.dto.response.SignupResponseDTO;
import realClassOne.chickenStock.auth.service.AuthService;
import realClassOne.chickenStock.auth.dto.response.EmailCheckResponseDTO;
import realClassOne.chickenStock.auth.service.EmailService;
import realClassOne.chickenStock.auth.dto.response.EmailVerifyResponseDTO;
import realClassOne.chickenStock.auth.dto.response.NicknameCheckResponseDTO;
import realClassOne.chickenStock.common.util.CookieUtils;

import java.io.IOException;

@Slf4j
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
public class AuthController {
    // 로그인, 로그아웃, 토큰 갱신 등 인증 작업
    private final AuthService authService;
    private final EmailService emailService;

    // OAuth2 인증 페이지로 리다이렉트하는 편의성 메서드
    @GetMapping("/oauth2/redirect/{provider}")
    public void redirectToOAuth2Provider(
            @PathVariable String provider,
            @RequestParam(required = false) String redirectUri,
            @RequestParam(required = false, defaultValue = "web") String platform,
            HttpServletResponse response) throws IOException {

        // 플랫폼 쿠키 설정
        CookieUtils.addCookie(response, "platform", platform, 300);

        // 리다이렉트 URI가 있으면 쿠키에 저장
        if (redirectUri != null && !redirectUri.isEmpty()) {
            CookieUtils.addCookie(response, "redirect_uri", redirectUri, 300);
        }

        // OAuth2 인증 URL로 직접 리다이렉트
        response.sendRedirect("/api/oauth2/authorization/" + provider);
    }

    @PostMapping("/signup")
    public ResponseEntity<SignupResponseDTO> signup(@Valid @RequestBody SignupRequestDTO signupRequestDTO) {
        return ResponseEntity.status(HttpStatus.CREATED).body(authService.signup(signupRequestDTO));
    }

    // 이메일 중복 체크
    @PostMapping("/check-email")
    public ResponseEntity<EmailCheckResponseDTO> checkEmail(@RequestBody EmailRequestDTO request) {
        EmailCheckResponseDTO response = authService.checkEmailDuplicateAndRespond(request.getEmail());

        if (!response.isSuccess()) {
            return ResponseEntity.badRequest().body(response);
        }
        return ResponseEntity.ok(response);
    }

    // 이메일 인증번호 전송 API
    @PostMapping("/send-code")
    public ResponseEntity<Void> sendCode(@RequestBody EmailRequestDTO request) {
//        emailService.sendVerificationCode(request.getEmail());
        emailService.sendVerificationCodeWithTTL(request.getEmail());
        return ResponseEntity.ok().build();
    }

    // 인증번호 확인 API
    @PostMapping("/verify-code")
    public ResponseEntity<EmailVerifyResponseDTO> verifyCode(@RequestBody EmailVerifyRequestDTO request) {
        EmailVerifyResponseDTO response = emailService.verifyEmailCodeAndRespond(request.getEmail(), request.getCode());

        if (!response.isSuccess()) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(response);
        }
        return ResponseEntity.ok(response);
    }

    @PostMapping("/exchange")
    public ResponseEntity<?> exchangeToken(
            @RequestBody ExchangeRequestDTO request,
            HttpServletResponse response) {

        Object result = authService.exchangeToken(request, response);
        return ResponseEntity.ok(result);
    }

    @PostMapping("/login")
    public ResponseEntity<?> login(@Valid @RequestBody LoginRequestDTO loginRequestDTO,
                                                  HttpServletResponse response) {
        return ResponseEntity.ok(authService.login(loginRequestDTO,response));
    }

    @PostMapping("/token/refresh-web")
    public ResponseEntity<WebTokenResponseDTO> refreshAccessToken(
            @CookieValue(name = "refreshToken") String refreshToken,
            HttpServletResponse response) {
        return ResponseEntity.ok(authService.refreshAccessTokenWeb(refreshToken, response));
    }

    @PostMapping("/token/refresh-mobile")
    public ResponseEntity<TokenDto> refreshAccessTokenMobile(
            @RequestBody OnlyRefreshTokenRequestDTO request) {
        return ResponseEntity.ok(authService.refreshAccessTokenMobile(request));
    }

    @PostMapping("/token/refresh-all-web")
    public ResponseEntity<WebTokenResponseDTO> refreshAllTokensWeb(
            @CookieValue(name = "refreshToken", required = false) String refreshToken,
            @CookieValue(name = "Authorization", required = false) String accessToken,
            HttpServletResponse response) {

        return ResponseEntity.ok(authService.refreshAllTokensWeb(refreshToken, accessToken, response));
    }

    @PostMapping("/token/refresh-all-mobile")
    public ResponseEntity<TokenDto> refreshAllTokensMobile(
            @RequestHeader("Authorization") String authorizationHeader,
            @RequestBody RefreshTokenRequestDTO request) {
        return ResponseEntity.ok(authService.refreshAllTokensMobile(request));
    }

    @PostMapping("/logout")
    public ResponseEntity<Void> logout(@RequestHeader("Authorization") String authorizationHeader) {
        authService.logout(authorizationHeader);
        return ResponseEntity.noContent().build();
    }

    // 이메일 인증 성공 후, 임시 비밀번호 발급 요청
    @PostMapping("/reset-password-by-code")
    public ResponseEntity<PasswordResetResponseDTO> resetPasswordByCode(@RequestBody EmailRequestDTO request) {
        PasswordResetResponseDTO response = authService.resetPasswordAfterVerification(request.getEmail());
        return ResponseEntity.ok(response);
    }

    // 닉네임 중복 체크
    @PostMapping("/check-nickname")
    public ResponseEntity<NicknameCheckResponseDTO> checkNickname(
            @RequestBody @Valid NicknameCheckRequestDTO request) {

        NicknameCheckResponseDTO response = authService.checkNickname(request.getNickname());
        return ResponseEntity.ok(response);
    }


}
