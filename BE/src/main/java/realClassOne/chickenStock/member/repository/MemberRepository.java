package realClassOne.chickenStock.member.repository;

import org.springframework.data.repository.query.Param;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.member.entity.Member;

import java.time.LocalDateTime;
import java.util.Optional;

@Repository
public interface MemberRepository extends JpaRepository<Member, Long> {

    Optional<Member> findByEmail(String email);
    boolean existsByEmail(String email);
    boolean existsByNickname(String nickname);


    @Modifying(clearAutomatically = true)
    @Transactional
    @Query("UPDATE Member m SET m.refreshToken = null, m.tokenExpiryDate = null WHERE m.tokenExpiryDate < :now")
    int invalidateExpiredTokens(@Param("now") LocalDateTime now);
}
