package realClassOne.chickenStock.auth.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.member.repository.MemberRepository;

import java.time.LocalDateTime;

// 매일 오전 00:00:00에 자동으로 만료된 Member 엔티티의 refreshToken을 null로 설정.
@Component
public class MemberRefreshTokenCleaner {
    private final MemberRepository memberRepository;
    private final Logger logger = LoggerFactory.getLogger(MemberRefreshTokenCleaner.class);

    public MemberRefreshTokenCleaner(MemberRepository memberRepository) {
        this.memberRepository = memberRepository;
    }

    // 매일 00:00:00에 실행 (초, 분, 시, 일, 월, 요일)
    @Scheduled(cron = "0 0 0 * * *", zone = "Asia/Seoul")
    public void cleanExpiredRefreshTokens() {
        logger.info("[Scheduler Start] Member refreshToken 만료토큰 클리너 실행됨");
        LocalDateTime now = LocalDateTime.now();
        int updatedCount = memberRepository.invalidateExpiredTokens(now);
        logger.info("만료된 Member refreshToken {}건 처리 (null 처리됨)", updatedCount);
    }
}
