package realClassOne.chickenStock.stock.service.trade;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.stock.dto.response.StockInfoResponseDTO;
import realClassOne.chickenStock.stock.service.KiwoomStockApiService;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class StockPriceCacheService {

    private final RedisTemplate<String, String> redisTemplate;
    private final KiwoomStockApiService kiwoomStockApiService;
    private final ObjectMapper objectMapper;

    // 캐시 키 접두사 및 TTL 설정
    private static final String PRICE_CACHE_KEY_PREFIX = "stock:price:";
    private static final long PRICE_CACHE_TTL_SECONDS = 3; // 2-3초 TTL

    // API 요청 제한을 위한 세마포어 (초당 3회 제한)
    private final Semaphore kiwoomApiSemaphore = new Semaphore(3);

    /**
     * 종목 현재가 조회 (캐싱 적용)
     * 1. 캐시 확인
     * 2. 캐시에 없으면 API 호출 (API 제한 적용)
     * 3. 결과 캐싱 후 반환
     */
    public Long getCurrentStockPrice(String stockCode) {
        String cacheKey = PRICE_CACHE_KEY_PREFIX + stockCode;

        // 1. 캐시 확인
        String cachedPrice = redisTemplate.opsForValue().get(cacheKey);
        if (cachedPrice != null) {
            try {
                log.debug("캐시에서 종목 {} 가격 조회: {}", stockCode, cachedPrice);
                return Long.parseLong(cachedPrice);
            } catch (NumberFormatException e) {
                log.warn("캐시된 가격 변환 오류: {}", cachedPrice);
                // 캐시 삭제 후 다시 조회
                redisTemplate.delete(cacheKey);
            }
        }

        // 2. API 호출 (세마포어 적용)
        try {
            // API 요청 제한 적용 (3초 타임아웃)
            if (!kiwoomApiSemaphore.tryAcquire(3, TimeUnit.SECONDS)) {
                log.warn("API 요청 한도 초과. 종목 {} 가격 조회 실패", stockCode);
                return null;
            }

            try {
                // 키움 API 호출
                Long currentPrice = fetchCurrentPriceFromAPI(stockCode);

                if (currentPrice != null) {
                    // 3. 결과 캐싱 (2-3초 TTL)
                    redisTemplate.opsForValue().set(cacheKey, String.valueOf(currentPrice),
                            PRICE_CACHE_TTL_SECONDS, TimeUnit.SECONDS);
                    log.debug("종목 {} 가격 캐싱 완료: {} (TTL: {}초)",
                            stockCode, currentPrice, PRICE_CACHE_TTL_SECONDS);
                }

                return currentPrice;
            } finally {
                // 333ms 후에 세마포어 반환 (초당 3회 제한 맞추기)
                schedulePermitRelease(333);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.error("API 세마포어 획득 중 인터럽트 발생", e);
            return null;
        } catch (Exception e) {
            log.error("종목 {} 가격 조회 중 오류 발생", stockCode, e);
            return null;
        }
    }

    /**
     * 시간 간격을 두고 세마포어 반환 (API 요청 간격 유지)
     */
    private void schedulePermitRelease(long delayMs) {
        CompletableFuture.delayedExecutor(delayMs, TimeUnit.MILLISECONDS)
                .execute(() -> kiwoomApiSemaphore.release());
    }

    /**
     * 키움 API에서 현재가 조회
     */
    private Long fetchCurrentPriceFromAPI(String stockCode) {
        try {
            // 1. 먼저 getStockInfo 메서드 시도
            StockInfoResponseDTO stockInfo = kiwoomStockApiService.getStockInfo(stockCode);
            if (stockInfo != null) {
                String priceStr = stockInfo.getCurrentPrice()
                        .replace(",", "")
                        .replace("+", "")
                        .replace("-", "")
                        .trim();
                return Long.parseLong(priceStr);
            }

            // 2. 실패시 getStockBasicInfo 메서드 시도
            JsonNode stockBasicInfo = kiwoomStockApiService.getStockBasicInfo(stockCode);
            if (stockBasicInfo != null && stockBasicInfo.has("cur_prc")) {
                String priceStr = stockBasicInfo.get("cur_prc").asText()
                        .replace(",", "")
                        .replace("+", "")
                        .replace("-", "")
                        .trim();
                return Long.parseLong(priceStr);
            }

            log.warn("종목 {} API 응답에서 현재가를 찾을 수 없음", stockCode);
            return null;
        } catch (Exception e) {
            log.error("종목 {} API 가격 조회 중 오류 발생", stockCode, e);
            return null;
        }
    }

    /**
     * 여러 종목의 가격을 일괄 조회 (배치 처리)
     */
    public Map<String, Long> getCurrentStockPrices(List<String> stockCodes) {
        Map<String, Long> results = new HashMap<>();
        Map<String, String> missingInCache = new HashMap<>();

        // 1. 캐시에서 먼저 조회
        for (String stockCode : stockCodes) {
            String cacheKey = PRICE_CACHE_KEY_PREFIX + stockCode;
            String cachedPrice = redisTemplate.opsForValue().get(cacheKey);

            if (cachedPrice != null) {
                try {
                    results.put(stockCode, Long.parseLong(cachedPrice));
                } catch (NumberFormatException e) {
                    missingInCache.put(cacheKey, stockCode);
                }
            } else {
                missingInCache.put(cacheKey, stockCode);
            }
        }

        // 2. 캐시에 없는 종목만 API 호출 (최대 3개씩 병렬 처리)
        if (!missingInCache.isEmpty()) {
            List<String> missingCodes = new ArrayList<>(missingInCache.values());
            int batchSize = 3; // 세마포어 크기와 일치

            for (int i = 0; i < missingCodes.size(); i += batchSize) {
                List<String> batch = missingCodes.subList(i,
                        Math.min(i + batchSize, missingCodes.size()));

                // 병렬 처리
                List<CompletableFuture<Void>> futures = batch.stream()
                        .map(code -> CompletableFuture.runAsync(() -> {
                            Long price = getCurrentStockPrice(code);
                            if (price != null) {
                                results.put(code, price);
                            }
                        }))
                        .collect(Collectors.toList());

                // 배치 완료 대기
                CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();

                // API 요청 간격 조절 (배치 간 1초 대기)
                if (i + batchSize < missingCodes.size()) {
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                }
            }
        }

        return results;
    }

    /**
     * 캐시 상태 확인
     */
    public Map<String, Object> getCacheStats() {
        Map<String, Object> stats = new HashMap<>();
        Set<String> keys = redisTemplate.keys(PRICE_CACHE_KEY_PREFIX + "*");

        stats.put("cacheSize", keys != null ? keys.size() : 0);
        stats.put("availablePermits", kiwoomApiSemaphore.availablePermits());

        return stats;
    }

    /**
     * 특정 종목의 가격 캐시를 무효화합니다.
     * @param stockCode 종목 코드
     */
    public void invalidateCache(String stockCode) {
        String cacheKey = PRICE_CACHE_KEY_PREFIX + stockCode;
        redisTemplate.delete(cacheKey);
        log.debug("종목 {} 가격 캐시 무효화", stockCode);
    }
}