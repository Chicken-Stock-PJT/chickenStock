package realClassOne.chickenStock.notification.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import realClassOne.chickenStock.notification.entity.Notification;

import java.util.List;

public interface NotificationRepository extends JpaRepository<Notification, Long> {
    List<Notification> findByMemberIdAndIsReadFalseOrderByCreatedAtDesc(Long memberId);
    List<Notification> findByMemberIdOrderByCreatedAtDesc(Long memberId);
    void deleteByMemberId(Long memberId);
}