package realClassOne.chickenStock.rank.scheduler;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.entity.TradeHistory;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;
import realClassOne.chickenStock.stock.repository.PendingOrderRepository;
import realClassOne.chickenStock.stock.repository.TradeHistoryRepository;
import realClassOne.chickenStock.stock.service.KiwoomStockApiService;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.*;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
@Slf4j
public class UserRankingScheduler {

    private final MemberRepository memberRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final TradeHistoryRepository tradeHistoryRepository;
    private final ZSetOperations<String, String> zSetOperations;
    private final RedisTemplate<String, String> redisTemplate;
    private final KiwoomStockApiService kiwoomStockApiService;
    private final PendingOrderRepository pendingOrderRepository;

    private static final String REDIS_KEY = "ranking:totalAsset";
    private static final String RETURN_RATE_KEY = "ranking:returnRate";
    private static final String LATEST_PRICE_KEY = "stock:latestPrice"; // 최신 가격 캐시 키

    /**
     * 매 1시간마다 회원별 총자산 기준 Redis 랭킹 갱신
     */
    @Scheduled(cron = "0 */5 * * * *")
    @Transactional(readOnly = true)
    public void updateRanking() {
        log.info("🔄 [랭킹 스케줄러] Redis에 랭킹 갱신 시작");

        // Redis 초기화
        zSetOperations.getOperations().delete(REDIS_KEY);
        zSetOperations.getOperations().delete(RETURN_RATE_KEY);

        // 모든 회원 조회
        List<Member> members = memberRepository.findAll();

        // 모든 회원 보유 종목 취합 (중복 제거)
        Set<String> allStockCodes = members.stream()
                .flatMap(member -> holdingPositionRepository.findByMember(member).stream())
                .map(h -> h.getStockData().getShortCode())
                .collect(Collectors.toSet());

        List<String> stockCodeList = new ArrayList<>(allStockCodes);

        // 종목 코드 100개씩 잘라서 요청
        Map<String, JsonNode> priceMap = new HashMap<>();
        int batchSize = 100;

        for (int i = 0; i < stockCodeList.size(); i += batchSize) {
            List<String> batch = stockCodeList.subList(i, Math.min(i + batchSize, stockCodeList.size()));
            Map<String, JsonNode> partialResult = kiwoomStockApiService.getWatchListInfoMap(batch);
            priceMap.putAll(partialResult);

            // API 요청 제한 회피를 위해 1초 대기
            if (i + batchSize < stockCodeList.size()) {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    log.error("스케줄러 sleep 중단됨", e);
                }
            }
        }

        HashOperations<String, String, String> hashOps = redisTemplate.opsForHash(); // HashOps 불러오기

        for (Map.Entry<String, JsonNode> entry : priceMap.entrySet()) {
            String code = entry.getKey(); // ex. 005930_AL
            JsonNode stockInfo = entry.getValue();
            if (stockInfo.has("cur_prc")) {
                String rawPrice = stockInfo.get("cur_prc").asText().replaceAll("[^0-9]", "");
                if (!rawPrice.isEmpty()) {
                    hashOps.put(LATEST_PRICE_KEY, code, rawPrice); // 캐시에 저장
                }
            }
        }

        for (Member member : members) {
            List<HoldingPosition> holdings = holdingPositionRepository.findByMember(member);
            long totalAsset = member.getMemberMoney();

            // PENDING 상태의 BUY 주문 자산 반영
            List<PendingOrder> pendingOrders = pendingOrderRepository.findByMemberAndStatus(member, PendingOrder.OrderStatus.PENDING);
            for (PendingOrder pending : pendingOrders) {
                if (pending.getOrderType() == TradeHistory.TradeType.BUY) {
                    long pendingValue = pending.getTargetPrice() * pending.getQuantity();
                    totalAsset += pendingValue;
                }
            }

            for (HoldingPosition holding : holdings) {
                String code = holding.getStockData().getShortCode();
                String key = code + "_AL"; // _AL 붙여주기
                JsonNode stockInfo = priceMap.get(key);
                long price = 0L;
                if (stockInfo != null && stockInfo.has("cur_prc")) {
                    String rawPrice = stockInfo.get("cur_prc").asText();
                    rawPrice = rawPrice.replaceAll("[^0-9]", ""); // 여기서 문자열 필터링
                    if (!rawPrice.isEmpty()) {
                        price = Long.parseLong(rawPrice);
                    }
                }else {
                    // 현재가 없을 때 fallback 캐시 사용

                    String cachedPrice = hashOps.get(LATEST_PRICE_KEY, key);
                    if (cachedPrice != null && !cachedPrice.isEmpty()) {
                        price = Long.parseLong(cachedPrice);
                        log.warn("시세 없음, 캐시된 가격 사용: {} → {}", key, price);
                    } else {
                        log.warn("시세 및 캐시 모두 없음: {}", key);
                    }
                }

                totalAsset += price * holding.getQuantity();
            }

//            zSetOperations.add(REDIS_KEY, member.getMemberId().toString(), totalAsset);



            String memberIdStr = member.getMemberId().toString();
            zSetOperations.add(REDIS_KEY, memberIdStr, totalAsset);

            // 수익률 계산 추가 시작
            List<TradeHistory> tradeHistories = tradeHistoryRepository.findWithStockDataByMember(member);
            Map<Long, List<TradeHistory>> groupedByStock = tradeHistories.stream()
                    .collect(Collectors.groupingBy(t -> t.getStockData().getStockDataId()));

            long totalInvestment = 0L;
            long totalEvaluation = 0L;

            for (Map.Entry<Long, List<TradeHistory>> entry : groupedByStock.entrySet()) {
                List<TradeHistory> trades = entry.getValue();
                long totalBuyQty = 0L;
                long totalBuyAmount = 0L;
                long totalSellQty = 0L;

                for (TradeHistory trade : trades) {
                    if (trade.getTradeType() == TradeHistory.TradeType.BUY) {
                        totalBuyQty += trade.getQuantity();
                        totalBuyAmount += trade.getTotalPrice();
                    } else if (trade.getTradeType() == TradeHistory.TradeType.SELL) {
                        totalSellQty += trade.getQuantity();
                    }
                }


                String shortCode = trades.get(0).getStockData().getShortCode();

                long holdingQty = totalBuyQty - totalSellQty;
                if (holdingQty <= 0) {
                    continue;
                }

                long avgBuyPrice = totalBuyAmount / totalBuyQty;
                long investAmount = avgBuyPrice * holdingQty;
                totalInvestment += investAmount;


                String priceKey = shortCode + "_AL";
                JsonNode priceInfo = priceMap.get(priceKey);


                long currentPrice = 0L;
                if (priceInfo != null && priceInfo.has("cur_prc")) {
                    String rawPrice = priceInfo.get("cur_prc").asText().replaceAll("[^0-9]", "");
                    if (!rawPrice.isEmpty()) {
                        currentPrice = Long.parseLong(rawPrice);
                    }
                } else {
                    // 수익률 계산에서도 캐시 fallback
                    String cachedPrice = hashOps.get(LATEST_PRICE_KEY, priceKey);
                    if (cachedPrice != null && !cachedPrice.isEmpty()) {
                        currentPrice = Long.parseLong(cachedPrice);
                        log.warn("수익률용 캐시된 가격 사용: {} → {}", priceKey, currentPrice);
                    } else {
                        log.warn("수익률용 가격 없음: {}", priceKey);
                    }
                }



                totalEvaluation += currentPrice * holdingQty;
            }

            if (totalInvestment > 0L) {
                double returnRate = ((double) totalEvaluation - totalInvestment) / totalInvestment * 100;
                returnRate = Math.round(returnRate * 100.0) / 100.0;
                zSetOperations.add(RETURN_RATE_KEY, memberIdStr, returnRate);
            } else {
                zSetOperations.add(RETURN_RATE_KEY, memberIdStr, 0.0);
            }
            // 수익률 계산 끝
        }

        log.info("✅ [랭킹 스케줄러] 랭킹 갱신 완료 (총 {}명)", members.size());

    }
}
