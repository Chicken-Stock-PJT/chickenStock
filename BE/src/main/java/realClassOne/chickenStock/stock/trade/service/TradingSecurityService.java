package realClassOne.chickenStock.stock.trade.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.util.concurrent.TimeUnit;

@Service
@RequiredArgsConstructor
@Slf4j
public class TradingSecurityService {

    private final RedisTemplate<String, String> redisTemplate;

    /**
     * 비정상적인 거래 패턴 체크
     */
    public boolean checkAbnormalTradingPattern(Long memberId, String stockCode, String tradeType) {
        // 단기간 과도한 거래 패턴 확인 (3초당 거래 제한)
        String shortTermKey = String.format("trades:short:%s:%s:%d", memberId, stockCode, System.currentTimeMillis() / 1000);
        Long shortTermCount = redisTemplate.opsForValue().increment(shortTermKey, 1);
        redisTemplate.expire(shortTermKey, 3, TimeUnit.SECONDS); // 3초 동안 유효

        if (shortTermCount > 15) { // 3초 내 15회 이상 거래 시도는 비정상으로 판단
            log.warn("단기간 과도한 거래 패턴 감지: 회원={}, 종목={}, 타입={}, 3초 내 거래 시도 횟수={}",
                    memberId, stockCode, tradeType, shortTermCount);

            // 10분간 거래 제한
            String flagKey = String.format("abnormal:member:%s", memberId);
            redisTemplate.opsForValue().set(flagKey, "true", 10, TimeUnit.MINUTES);
            return true;
        }

        // 연속 거래 패턴 체크 (매수-매도 스위칭)
        String switchKey = String.format("trades:switch:%s:%d", memberId, System.currentTimeMillis() / 1000);
        String lastTradeType = redisTemplate.opsForValue().get(switchKey + ":last");

        // 이전 거래가 있고, 타입이 다른 경우 (매수→매도 or 매도→매수)
        if (lastTradeType != null && !lastTradeType.equals(tradeType)) {
            Long switchCount = redisTemplate.opsForValue().increment(switchKey, 1);
            redisTemplate.expire(switchKey, 3, TimeUnit.SECONDS);

            // 3초 내 5회 이상 매수-매도 스위칭은 비정상으로 판단
            if (switchCount > 5) {
                log.warn("비정상 매수-매도 스위칭 패턴 감지: 회원={}, 3초 내 스위칭 횟수={}",
                        memberId, switchCount);

                String flagKey = String.format("abnormal:member:%s", memberId);
                redisTemplate.opsForValue().set(flagKey, "true", 10, TimeUnit.MINUTES);

                return true;
            }
        }

        // 현재 거래 타입 저장
        redisTemplate.opsForValue().set(switchKey + ":last", tradeType, 3, TimeUnit.SECONDS);

        return false;
    }

    /**
     * 회원 제한 상태 확인
     */
    public boolean isRestricted(Long memberId) {
        String flagKey = String.format("abnormal:member:%s", memberId);
        return Boolean.TRUE.equals(redisTemplate.hasKey(flagKey));
    }

    /**
     * 회원 제한 수동 해제 (관리자용)
     */
    public boolean removeRestriction(Long memberId) {
        String flagKey = String.format("abnormal:member:%s", memberId);
        return Boolean.TRUE.equals(redisTemplate.delete(flagKey));
    }
}