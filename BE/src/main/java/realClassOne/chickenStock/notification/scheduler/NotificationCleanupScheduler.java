package realClassOne.chickenStock.notification.scheduler;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.notification.repository.NotificationRepository;

import java.time.LocalDateTime;

@Component
@Slf4j
@RequiredArgsConstructor
public class NotificationCleanupScheduler {

    private final NotificationRepository notificationRepository;

    /**
     * 매일 자정에 실행되어 하루 이상 지난 읽은 알림들을 삭제
     */
    @Scheduled(cron = "0 0 0 * * *")  // 매일 자정에 실행
    @Transactional
    public void cleanupReadNotifications() {
        try {
            LocalDateTime oneDayAgo = LocalDateTime.now().minusDays(1);

            // 하루 이상 지난 읽은 알림 삭제
            notificationRepository.deleteReadNotificationsOlderThan(oneDayAgo);

        } catch (Exception e) {
            log.error("읽은 알림 정리 중 오류 발생", e);
        }
    }

    /**
     * 특정 주기(예: 12시간)마다 실행되어 7일 이상 지난 모든 알림 삭제
     */
    @Scheduled(cron = "0 0 */12 * * *")  // 12시간마다 실행
    @Transactional
    public void cleanupOldNotifications() {
        try {
            LocalDateTime sevenDaysAgo = LocalDateTime.now().minusDays(7);

            // 7일 이상 지난 모든 알림 삭제 (읽음 여부 무관)
            notificationRepository.deleteByCreatedAtBefore(sevenDaysAgo);

        } catch (Exception e) {
            log.error("오래된 알림 정리 중 오류 발생", e);
        }
    }
}