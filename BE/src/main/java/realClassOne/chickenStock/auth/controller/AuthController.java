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

@Slf4j
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
public class AuthController {
    // 로그인, 로그아웃, 토큰 갱신 등 인증 작업
    private final AuthService authService;

    @PostMapping("/signup")
    public ResponseEntity<SignupResponseDTO> signup(@Valid @RequestBody SignupRequestDTO signupRequestDTO) {
        return ResponseEntity.status(HttpStatus.CREATED).body(authService.signup(signupRequestDTO));
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
