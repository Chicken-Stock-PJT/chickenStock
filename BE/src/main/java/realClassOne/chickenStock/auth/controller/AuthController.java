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
import realClassOne.chickenStock.auth.dto.request.ExchangeRequestDTO;
import realClassOne.chickenStock.auth.dto.request.LoginRequestDTO;
import realClassOne.chickenStock.auth.dto.request.RefreshTokenRequestDTO;
import realClassOne.chickenStock.auth.dto.request.SignupRequestDTO;
import realClassOne.chickenStock.auth.dto.response.SignupResponseDTO;
import realClassOne.chickenStock.auth.service.AuthService;
import realClassOne.chickenStock.auth.dto.request.EmailRequestDTO;
import realClassOne.chickenStock.auth.dto.response.EmailCheckResponseDTO;
import realClassOne.chickenStock.auth.service.EmailService;
import realClassOne.chickenStock.auth.dto.response.EmailVerifyResponseDTO;
import realClassOne.chickenStock.auth.dto.request.EmailVerifyRequestDTO;


@Slf4j
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
public class AuthController {
    // 로그인, 로그아웃, 토큰 갱신 등 인증 작업
    private final AuthService authService;
    private final EmailService emailService;

    @PostMapping("/signup")
    public ResponseEntity<SignupResponseDTO> signup(@Valid @RequestBody SignupRequestDTO signupRequestDTO) {
        return ResponseEntity.status(HttpStatus.CREATED).body(authService.signup(signupRequestDTO));
    }

    // 이메일 인증 코드 전송
    @PostMapping("/check-email")
    public ResponseEntity<EmailCheckResponseDTO> checkEmail(@RequestBody EmailRequestDTO request) {
        EmailCheckResponseDTO response = authService.checkEmailDuplicateAndRespond(request.getEmail());

        if (!response.isSuccess()) {
            return ResponseEntity.badRequest().body(response);
        }
        return ResponseEntity.ok(response);
    }

    @PostMapping("/send-code")
    public ResponseEntity<Void> sendCode(@RequestBody EmailRequestDTO request) {
        emailService.sendVerificationCode(request.getEmail());
        return ResponseEntity.ok().build();
    }

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
            @CookieValue(name = "refreshToken", required = false) String refreshToken,
            @CookieValue(name = "accessToken", required = false) String accessToken,
            HttpServletResponse response) {
        return ResponseEntity.ok(authService.refreshAccessTokenWeb(refreshToken, accessToken, response));
    }

    @PostMapping("/token/refresh-mobile")
    public ResponseEntity<TokenDto> refreshAccessTokenMobile(@RequestBody RefreshTokenRequestDTO request) {
        return ResponseEntity.ok(authService.refreshAccessTokenMobile(request.getRefreshToken()));
    }

    @PostMapping("/token/refresh-all-web")
    public ResponseEntity<WebTokenResponseDTO> refreshAllTokensWeb(
            @CookieValue(name = "refreshToken", required = false) String refreshToken,
            HttpServletResponse response) {

        return ResponseEntity.ok(authService.refreshAllTokensWeb(refreshToken, response));
    }

    @PostMapping("/token/refresh-all-mobile")
    public ResponseEntity<TokenDto> refreshAllTokensMobile(@RequestBody RefreshTokenRequestDTO request) {
        return ResponseEntity.ok(authService.refreshAllTokensMobile(request.getRefreshToken()));
    }

    @PostMapping("/logout")
    public ResponseEntity<Void> logout(@RequestHeader("refreshToken") String authorizationHeader) {
        authService.logout(authorizationHeader);
        return ResponseEntity.noContent().build();
    }
}
