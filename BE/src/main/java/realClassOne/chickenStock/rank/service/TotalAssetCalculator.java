package realClassOne.chickenStock.rank.service;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;
import realClassOne.chickenStock.stock.service.KiwoomStockApiService;

import java.util.*;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
@Slf4j
public class TotalAssetCalculator {

    private final MemberRepository memberRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final KiwoomStockApiService kiwoomStockApiService;
    private final ZSetOperations<String, String> zSetOperations;

    private static final String REDIS_KEY = "ranking:totalAsset";

    public void recalculateAll() {
        log.info("📊 [총자산 계산기] 전체 회원 총자산 재계산 시작");

        zSetOperations.getOperations().delete(REDIS_KEY);
        List<Member> members = memberRepository.findAll();

        Set<String> allCodes = members.stream()
                .flatMap(m -> holdingPositionRepository.findByMember(m).stream())
                .map(h -> h.getStockData().getShortCode())
                .collect(Collectors.toSet());

        Map<String, JsonNode> priceMap = new HashMap<>();
        List<String> codeList = new ArrayList<>(allCodes);
        int batchSize = 100;

        for (int i = 0; i < codeList.size(); i += batchSize) {
            List<String> batch = codeList.subList(i, Math.min(i + batchSize, codeList.size()));
            priceMap.putAll(kiwoomStockApiService.getWatchListInfoMap(batch));

            if (i + batchSize < codeList.size()) {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        }

        for (Member member : members) {
            List<HoldingPosition> holdings = holdingPositionRepository.findByMember(member);
            long totalAsset = member.getMemberMoney();

            for (HoldingPosition h : holdings) {
                String code = h.getStockData().getShortCode();
                JsonNode priceData = priceMap.get(code);
                if (priceData != null && priceData.has("cur_prc")) {
                    try {
                        long price = Long.parseLong(priceData.get("cur_prc").asText());
                        totalAsset += price * h.getQuantity();
                    } catch (NumberFormatException ignored) {}
                }
            }

            zSetOperations.add(REDIS_KEY, member.getMemberId().toString(), (double) totalAsset);
        }

        log.info("✅ [총자산 계산기] 총 {}명 자산 재계산 완료", members.size());
    }
}
