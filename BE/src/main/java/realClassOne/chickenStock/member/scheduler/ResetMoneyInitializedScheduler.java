package realClassOne.chickenStock.member.scheduler;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class ResetMoneyInitializedScheduler {

    private final MemberRepository memberRepository;

    /**
     * 매주 월요일 00시에 모든 회원의 moneyInitialized 플래그를 false로 초기화
     */
    @Scheduled(cron = "0 0 0 * * MON") // 매주 월요일 00시
    @Transactional
    public void resetAllMemberMoneyInitialized() {
        log.info("🕛 [스케줄러] 모든 회원의 1억 초기화 여부 플래그를 초기화합니다.");

        List<Member> members = memberRepository.findAll();
        for (Member member : members) {
            member.resetMoneyInitialized();
        }

        log.info("✅ [스케줄러 완료] 총 {}명의 회원 초기화 플래그가 리셋되었습니다.", members.size());
    }
}
