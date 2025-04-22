package realClassOne.chickenStock.security.jwt;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;
import realClassOne.chickenStock.auth.service.RedisTokenBlacklistService;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.security.excpetion.SecurityErrorCode;

import java.io.IOException;

@Component
@RequiredArgsConstructor
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtTokenProvider jwtTokenProvider;
    private final RedisTokenBlacklistService redisTokenBlacklistService;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {

        String header = request.getHeader("Authorization");
        System.out.println(">>> [JwtAuthFilter] Authorization header: " + header);

        // 로그아웃 요청인 경우 로직 탄 후 처리
        if (request.getMethod().equals("POST") && request.getRequestURI().endsWith("/api/auth/logout")) {
            System.out.println("로그아웃 요청 감지");
            filterChain.doFilter(request, response);
            return;
        }
        String token = resolveToken(request);
        if (StringUtils.hasText(token)) {
            // 블랙리스트 확인 (Redis)
            if (redisTokenBlacklistService.isBlacklisted(token)) {
                // 보안 컨텍스트 정리
                SecurityContextHolder.clearContext();
                throw new CustomException(SecurityErrorCode.BLACKLISTED_JWT_TOKEN);
            } else if (jwtTokenProvider.validateToken(token)) {
                // 토큰에서 Authentication 객체 가져오기
                System.out.println("3");
                Authentication authentication = jwtTokenProvider.getAuthentication(token);
                // SecurityContext에 Authentication 객체 저장
                System.out.println("4");
                SecurityContextHolder.getContext().setAuthentication(authentication);
            }
        }

        filterChain.doFilter(request, response);
    }

    // 요청 헤더에서 토큰 추출
    private String resolveToken(HttpServletRequest request) {
        String bearerToken = request.getHeader("Authorization");
        if (StringUtils.hasText(bearerToken) && bearerToken.startsWith("Bearer ")) {
            return bearerToken.substring(7);
        }
        return null;
    }
}
