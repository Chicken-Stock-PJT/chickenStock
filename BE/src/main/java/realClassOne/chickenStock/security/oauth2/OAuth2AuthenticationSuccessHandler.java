package realClassOne.chickenStock.security.oauth2;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationSuccessHandler;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;
import realClassOne.chickenStock.common.util.CookieUtils;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.io.IOException;
import java.util.Optional;

@Slf4j
@Component
@RequiredArgsConstructor
public class OAuth2AuthenticationSuccessHandler extends SimpleUrlAuthenticationSuccessHandler {

    private final JwtTokenProvider tokenProvider;

    @Value("${app.oauth2.authorized-redirect-uri}")
    private String defaultRedirectUri;

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
                                        Authentication authentication) throws IOException {

        OAuth2User oAuth2User = (OAuth2User) authentication.getPrincipal();
        Long memberId = oAuth2User.getAttribute("member_id");

        String oneTimeCode = tokenProvider.generateOneTimeTokenCode(memberId);

        // 플랫폼 정보 가져오기 (기본값: web)
        String platform = CookieUtils.getCookie(request, "platform")
                .map(Cookie::getValue)
                .orElse("web");

        log.debug("OAuth2 인증 성공: memberId={}, platform={}", memberId, platform);

        // 리다이렉트 URL 결정 및 원타임 코드 추가
        String targetUrl = determineTargetUrl(request, oneTimeCode, platform);

        if (response.isCommitted()) {
            log.debug("응답이 이미 커밋되었습니다. " + targetUrl + "로 리다이렉트할 수 없습니다.");
            return;
        }

        // 사용한 쿠키 삭제
        CookieUtils.deleteCookie(request, response, "platform");
        CookieUtils.deleteCookie(request, response, "redirect_uri");

        // 리다이렉트 실행
        getRedirectStrategy().sendRedirect(request, response, targetUrl);
    }

    protected String determineTargetUrl(HttpServletRequest request, String oneTimeCode, String platform) {
        // 쿠키에서 리다이렉트 URI 가져오기
        Optional<String> redirectUriOptional = CookieUtils.getCookie(request, "redirect_uri")
                .map(Cookie::getValue);

        // 모바일 플랫폼인 경우
        if ("mobile".equals(platform)) {
            // 모바일용 리다이렉트 URI가 있으면 사용하고, 없으면 앱 스킴 형식 사용
            String mobileRedirectUri = redirectUriOptional.orElse("chickenstock://oauth2callback");

            // URL에 oneTimeCode 추가
            return UriComponentsBuilder.fromUriString(mobileRedirectUri)
                    .queryParam("oneTimeCode", oneTimeCode)
                    .build().toUriString();
        }

        // 웹 플랫폼인 경우
        // 설정된 리다이렉트 URI가 있으면 사용하고, 없으면 기본 URI 사용
        String targetUrl = redirectUriOptional.orElse(defaultRedirectUri);

        // URL에 oneTimeCode 추가
        return UriComponentsBuilder.fromUriString(targetUrl)
                .queryParam("oneTimeCode", oneTimeCode)
                .build().toUriString();
    }
}