package realClassOne.chickenStock.rank.scheduler;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.stock.dto.response.PortfolioResponseDTO;
import realClassOne.chickenStock.stock.service.PortfolioService;

import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class RankingScheduler {

    private final MemberRepository memberRepository;
    private final PortfolioService portfolioService;
    private final ZSetOperations<String, String> zSetOperations;

    private static final String REDIS_KEY = "ranking:totalAsset";

    /**
     * 매 1시간마다 회원별 총자산 기준 Redis 랭킹 갱신
     */
    @Scheduled(cron = "0 0 * * * *") // 매 정시마다 실행 (ex. 12:00, 13:00, ...)
    public void updateTotalAssetRanking() {
        log.info("🔄 [랭킹 스케줄러] Redis에 총자산 랭킹 갱신 시작");

        // Redis 초기화
        zSetOperations.getOperations().delete(REDIS_KEY);

        // 모든 회원 조회
        List<Member> members = memberRepository.findAll();

        for (Member member : members) {
            try {
                // 회원 ID 기준으로 포트폴리오 조회
                PortfolioResponseDTO portfolio = portfolioService.getPortfolioById(member.getMemberId());
                Long totalAsset = portfolio.getTotalAsset();

                if (totalAsset != null) {
                    // Redis ZSET에 저장 (memberId를 문자열로 저장)
                    zSetOperations.add(REDIS_KEY, member.getMemberId().toString(), totalAsset);
                }
            } catch (Exception e) {
                log.warn("❗ 회원 {}의 포트폴리오 조회 실패: {}", member.getMemberId(), e.getMessage());
            }
        }

        log.info("✅ [랭킹 스케줄러] 총자산 랭킹 갱신 완료 (총 {}명)", members.size());
    }
}
