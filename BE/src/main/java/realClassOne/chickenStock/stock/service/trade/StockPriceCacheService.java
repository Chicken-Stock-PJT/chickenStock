package realClassOne.chickenStock.stock.service.trade;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.stock.dto.response.StockInfoResponseDTO;
import realClassOne.chickenStock.stock.service.KiwoomStockApiService;

import java.util.concurrent.TimeUnit;

@Service
@RequiredArgsConstructor
@Slf4j
public class StockPriceCacheService {

    private final RedisTemplate<String, Object> redisTemplate;
    private final KiwoomStockApiService kiwoomStockApiService;

    private static final String CACHE_KEY_PREFIX = "stock:price:";
    private static final long CACHE_TTL_SECONDS = 3; // 3초 캐시 유효시간

    /**
     * 캐시에서만 종목 가격을 조회 (API 호출 없음)
     * @param stockCode 종목 코드
     * @return 캐시된 가격 (없으면 null)
     */
    public Long getFromCacheOnly(String stockCode) {
        try {
            String cacheKey = CACHE_KEY_PREFIX + stockCode;
            Object cached = redisTemplate.opsForValue().get(cacheKey);

            if (cached != null) {
                // Integer와 Long 모두 처리할 수 있도록 안전한 변환
                if (cached instanceof Integer) {
                    Integer intPrice = (Integer) cached;
                    Long longPrice = intPrice.longValue();
                    log.debug("캐시에서 종목 {} 가격 조회(Integer->Long 변환): {}", stockCode, longPrice);
                    return longPrice;
                } else if (cached instanceof Long) {
                    Long price = (Long) cached;
                    log.debug("캐시에서 종목 {} 가격 조회: {}", stockCode, price);
                    return price;
                } else if (cached instanceof Number) {
                    // 다른 숫자 타입도 처리
                    Number number = (Number) cached;
                    Long price = number.longValue();
                    log.debug("캐시에서 종목 {} 가격 조회(Number->Long 변환): {}", stockCode, price);
                    return price;
                } else if (cached instanceof String) {
                    // 문자열인 경우 파싱 시도
                    try {
                        String strValue = (String) cached;
                        strValue = strValue.replace(",", "").replace("+", "").replace("-", "").trim();
                        Long price = Long.parseLong(strValue);
                        log.debug("캐시에서 종목 {} 가격 조회(String->Long 변환): {}", stockCode, price);
                        return price;
                    } catch (NumberFormatException e) {
                        log.warn("캐시된 문자열 가격 파싱 실패: {}", cached);
                    }
                } else {
                    // 문자열이나 다른 타입인 경우
                    log.warn("캐시에서 종목 {} 가격 조회 실패: 예상치 못한 데이터 타입 ({})",
                            stockCode, cached.getClass().getName());
                    redisTemplate.delete(cacheKey); // 잘못된 타입의 데이터 삭제
                    return null;
                }
            }
        } catch (Exception e) {
            log.error("캐시에서 종목 {} 가격 조회 중 오류: {}", stockCode, e.getMessage());
        }
        return null;
    }

    /**
     * 종목 현재가를 조회 (캐시 → API 호출 순)
     * @param stockCode 종목 코드
     * @return 현재가 (실패 시 null)
     */
    public Long getCurrentStockPrice(String stockCode) {
        // 캐시에서 먼저 조회
        Long cachedPrice = getFromCacheOnly(stockCode);
        if (cachedPrice != null) {
            return cachedPrice;
        }

        // 캐시에 없으면 API 호출
        try {
            StockInfoResponseDTO stockInfo = kiwoomStockApiService.getStockInfo(stockCode);
            if (stockInfo != null) {
                String priceStr = stockInfo.getCurrentPrice()
                        .replace(",", "")
                        .replace("+", "")
                        .replace("-", "")
                        .trim();
                Long price = Long.parseLong(priceStr);

                // 캐시에 저장 (Long으로 저장되도록 명시적으로 변환)
                String cacheKey = CACHE_KEY_PREFIX + stockCode;
                redisTemplate.opsForValue().set(cacheKey, price, CACHE_TTL_SECONDS, TimeUnit.SECONDS);
                log.debug("종목 {} 가격 캐싱 완료: {} (TTL: {}초)", stockCode, price, CACHE_TTL_SECONDS);

                return price;
            }
        } catch (Exception e) {
            log.error("종목 {} 가격 조회 실패: {}", stockCode, e.getMessage());
        }

        return null;
    }

    /**
     * 캐시 무효화
     */
    public void invalidateCache(String stockCode) {
        String cacheKey = CACHE_KEY_PREFIX + stockCode;
        redisTemplate.delete(cacheKey);
        log.debug("종목 {} 가격 캐시 무효화", stockCode);
    }

    /**
     * 강제로 캐시 갱신 및 반환 (락 획득 실패 시 폴백)
     */
    public Long refreshAndGetPrice(String stockCode) {
        try {
            // 기존 캐시 즉시 무효화
            invalidateCache(stockCode);

            // API 직접 호출하여 최신 데이터 가져오기
            StockInfoResponseDTO stockInfo = kiwoomStockApiService.getStockInfo(stockCode);
            if (stockInfo != null) {
                String priceStr = stockInfo.getCurrentPrice()
                        .replace(",", "")
                        .replace("+", "")
                        .replace("-", "")
                        .trim();
                Long price = Long.parseLong(priceStr);

                // 캐시에 새로 저장
                String cacheKey = CACHE_KEY_PREFIX + stockCode;
                redisTemplate.opsForValue().set(cacheKey, price, CACHE_TTL_SECONDS, TimeUnit.SECONDS);
                log.info("종목 {} 가격 강제 리프레시 완료: {} (TTL: {}초)", stockCode, price, CACHE_TTL_SECONDS);

                return price;
            }
        } catch (Exception e) {
            log.error("종목 {} 가격 강제 리프레시 실패: {}", stockCode, e.getMessage());
        }

        return null;
    }

    /**
     * 여러 종목의 가격을 일괄 리프레시 (배치 처리용)
     */
    public int batchRefreshPrices(String... stockCodes) {
        int successCount = 0;

        for (String stockCode : stockCodes) {
            try {
                Long price = refreshAndGetPrice(stockCode);
                if (price != null) {
                    successCount++;
                }
            } catch (Exception e) {
                log.error("배치 리프레시 중 종목 {} 처리 실패: {}", stockCode, e.getMessage());
            }
        }

        log.info("배치 가격 리프레시 완료: {}/{} 성공", successCount, stockCodes.length);
        return successCount;
    }
}