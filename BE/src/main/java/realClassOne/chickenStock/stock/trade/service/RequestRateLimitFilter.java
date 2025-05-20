package realClassOne.chickenStock.stock.trade.service;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

@Component
public class RequestRateLimitFilter extends OncePerRequestFilter {

    private final LoadingCache<String, TokenBucket> ipBuckets;

    public RequestRateLimitFilter() {
        // IP별 제한만 유지 (클라이언트별 제한 제거)
        this.ipBuckets = CacheBuilder.newBuilder()
                .expireAfterWrite(1, TimeUnit.HOURS)
                .build(new CacheLoader<String, TokenBucket>() {
                    @Override
                    public TokenBucket load(String key) {
                        return new TokenBucket(120, 120, 60); // 분당 120개 요청
                    }
                });
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {

        // 트레이딩 API에만 적용
        String requestURI = request.getRequestURI();
        if (requestURI.startsWith("/api/stock/trading/")) {
            // IP 기반 제한 확인
            String clientIp = getClientIP(request);
            if (!ipBuckets.getUnchecked(clientIp).tryConsume()) {
                response.setStatus(HttpStatus.TOO_MANY_REQUESTS.value());
                response.getWriter().write("{\"error\": \"Too many requests from this IP address\", \"retryAfter\": 60}");
                return;
            }
        }

        filterChain.doFilter(request, response);
    }

    private String getClientIP(HttpServletRequest request) {
        String xfHeader = request.getHeader("X-Forwarded-For");
        if (xfHeader == null) {
            return request.getRemoteAddr();
        }
        return xfHeader.split(",")[0];
    }

    /**
     * 토큰 버킷 구현 (API 요청 제한용)
     */
    private static class TokenBucket {
        private final int capacity;
        private final int refillRate;
        private final long refillPeriodSeconds;
        private int tokens;
        private long lastRefillTime;

        public TokenBucket(int capacity, int refillRate, long refillPeriodSeconds) {
            this.capacity = capacity;
            this.refillRate = refillRate;
            this.refillPeriodSeconds = refillPeriodSeconds;
            this.tokens = capacity;
            this.lastRefillTime = System.currentTimeMillis();
        }

        public synchronized boolean tryConsume() {
            refill();

            if (tokens > 0) {
                tokens--;
                return true;
            }

            return false;
        }

        private void refill() {
            long now = System.currentTimeMillis();
            long elapsedSeconds = (now - lastRefillTime) / 1000;

            if (elapsedSeconds >= refillPeriodSeconds) {
                tokens = Math.min(capacity, tokens + refillRate);
                lastRefillTime = now;
            }
        }
    }
}