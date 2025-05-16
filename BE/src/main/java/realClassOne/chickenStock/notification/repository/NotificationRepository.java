package realClassOne.chickenStock.notification.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import realClassOne.chickenStock.notification.entity.Notification;

import java.time.LocalDateTime;
import java.util.List;

public interface NotificationRepository extends JpaRepository<Notification, Long> {
    List<Notification> findByMemberIdAndIsReadFalseOrderByCreatedAtDesc(Long memberId);
    List<Notification> findByMemberIdOrderByCreatedAtDesc(Long memberId);

    // 읽은 상태에 따른 알림 조회
    List<Notification> findByMemberIdAndIsReadOrderByCreatedAtDesc(Long memberId, boolean isRead);

    void deleteByMemberId(Long memberId);

    // 읽은 알림 중 특정 날짜 이전 것들 삭제
    @Modifying
    @Query("DELETE FROM Notification n WHERE n.isRead = true AND n.createdAt < :dateTime")
    void deleteReadNotificationsOlderThan(@Param("dateTime") LocalDateTime dateTime);

    // 특정 회원의 읽은 알림 중 특정 날짜 이전 것들 삭제
    @Modifying
    @Query("DELETE FROM Notification n WHERE n.memberId = :memberId AND n.isRead = true AND n.createdAt < :dateTime")
    void deleteReadNotificationsByMemberOlderThan(@Param("memberId") Long memberId, @Param("dateTime") LocalDateTime dateTime);

    // 특정 날짜 이전의 모든 알림 삭제
    @Modifying
    void deleteByCreatedAtBefore(LocalDateTime dateTime);

    List<Notification> findByMemberIdAndTypeOrderByCreatedAtDesc(Long memberId, String type);
}