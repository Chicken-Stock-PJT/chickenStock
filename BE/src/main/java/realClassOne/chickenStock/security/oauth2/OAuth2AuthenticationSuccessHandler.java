package realClassOne.chickenStock.security.oauth2;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationSuccessHandler;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;
import realClassOne.chickenStock.common.util.CookieUtils;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.io.IOException;
import java.net.URI;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class OAuth2AuthenticationSuccessHandler extends SimpleUrlAuthenticationSuccessHandler {

    private final JwtTokenProvider tokenProvider;

    @Value("${app.oauth2.authorized-redirect-uri}")
    private String redirectUri;

    // 사용하지 않는 쿠키 만료 관련 변수들 제거함

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
                                        Authentication authentication) throws IOException {

        OAuth2User oAuth2User = (OAuth2User) authentication.getPrincipal();
        Long memberId = oAuth2User.getAttribute("member_id");

        String oneTimeCode = tokenProvider.generateOneTimeTokenCode(memberId);

        // 리다이렉트 URL 결정 및 원타임 코드 추가
        String targetUrl = determineTargetUrl(request, oneTimeCode);

        if (response.isCommitted()) {
            logger.debug("응답이 이미 커밋되었습니다. " + targetUrl + "로 리다이렉트할 수 없습니다.");
            return;
        }

        // 리다이렉트 실행
        getRedirectStrategy().sendRedirect(request, response, targetUrl);
    }

    // 메서드 파라미터 간소화
    protected String determineTargetUrl(HttpServletRequest request, String oneTimeCode) {
        Optional<String> redirectUri = CookieUtils.getCookie(request, "redirect_uri")
                .map(Cookie::getValue);

        String targetUrl = redirectUri.orElse(getDefaultTargetUrl());

        // URI 유효성 검사
        URI clientRedirectUri = URI.create(targetUrl);
        URI authorizedURI = URI.create(this.redirectUri);

        // 승인되지 않은 리다이렉트
        if (!authorizedURI.getHost().equalsIgnoreCase(clientRedirectUri.getHost())) {
            targetUrl = this.redirectUri;
        }

        // 원타임 코드를 URL에 추가
        return UriComponentsBuilder.fromUriString(targetUrl)
                .queryParam("oneTimeCode", oneTimeCode)
                .build().toUriString();
    }
}