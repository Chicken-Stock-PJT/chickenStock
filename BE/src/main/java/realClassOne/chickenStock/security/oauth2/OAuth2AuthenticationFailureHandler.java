package realClassOne.chickenStock.security.oauth2;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;
import realClassOne.chickenStock.common.util.CookieUtils;

import java.io.IOException;

@Component
public class OAuth2AuthenticationFailureHandler extends SimpleUrlAuthenticationFailureHandler {

    @Override
    public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response,
                                        AuthenticationException exception) throws IOException {
        /*
            eX)
            const redirectUri = window.location.href;
            document.cookie = `redirect_uri=${redirectUri}; path=/`;
            window.location.href = "https://yourdomain.com/oauth2/authorization/google";
            그 쿠키에 담긴 redirect_uri 주소로 반환시키게 될거야!
         */
        String targetUrl = CookieUtils.getCookie(request, "redirect_uri")
                .map(Cookie::getValue)
                .orElse("/");

        targetUrl = UriComponentsBuilder.fromUriString(targetUrl)
                .queryParam("error", exception.getLocalizedMessage())
                .build().toUriString();

        CookieUtils.deleteCookie(request, response, "redirect_uri");

        getRedirectStrategy().sendRedirect(request, response, targetUrl);
    }
}
