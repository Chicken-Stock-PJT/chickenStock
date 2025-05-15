package realClassOne.chickenStock.rank.scheduler;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;
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
    private final ZSetOperations<String, String> zSetOperations;
    private final KiwoomStockApiService kiwoomStockApiService;

    private static final String REDIS_KEY = "ranking:totalAsset";

    /**
     * 매 1시간마다 회원별 총자산 기준 Redis 랭킹 갱신
     */
    @Scheduled(cron = "0 0 * * * *") // 매 정시마다 실행 (ex. 12:00, 13:00, ...)
    public void updateRanking() {
        log.info("🔄 [랭킹 스케줄러] Redis에 총자산 랭킹 갱신 시작");

        // Redis 초기화
        zSetOperations.getOperations().delete(REDIS_KEY);

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
        for (Member member : members) {
            List<HoldingPosition> holdings = holdingPositionRepository.findByMember(member);
            long totalAsset = member.getMemberMoney();
            for (HoldingPosition holding : holdings) {
                String code = holding.getStockData().getShortCode();
                JsonNode stockInfo = priceMap.get(code + "_AL"); // _AL 붙여주기
                long price = 0L;
                if (stockInfo != null && stockInfo.has("cur_prc")) {
                    price = Long.parseLong(stockInfo.get("cur_prc").asText());
                }
                totalAsset += price * holding.getQuantity();
            }

            zSetOperations.add(REDIS_KEY, member.getMemberId().toString(), totalAsset);
        }

        log.info("✅ [랭킹 스케줄러] 총자산 랭킹 갱신 완료 (총 {}명)", members.size());

    }
}
