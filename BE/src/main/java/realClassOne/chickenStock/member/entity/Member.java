package realClassOne.chickenStock.member.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name = "members")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@EntityListeners(AuditingEntityListener.class)
public class Member {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long memberId;

    @Column(nullable = false, unique = true)
    private String email;

    @Column
    private String memberPoint;

    @Column(nullable = false)
    private String name;
    @Column
    private String nickname;

    @Column
    private String password;

    @Column(name = "member_money")
    private Long memberMoney;

    @Column
    private String imageUrl;

    @Column(nullable = false)
    private String provider;

    @Column
    private String providerId;

    @Column(length = 500)
    private String refreshToken;

    @ElementCollection(fetch = FetchType.EAGER)
    @Enumerated(EnumType.STRING)
    @CollectionTable(name = "user_roles", joinColumns = @JoinColumn(name = "user_id"))
    @Column(name = "role")
    private Set<MemberRole> roles = new HashSet<>();

    @CreatedDate
    @Column(nullable = false, updatable = false, columnDefinition = "DATETIME(0)")
    private LocalDateTime createdAt;

    @LastModifiedDate
    @Column(nullable = false, columnDefinition = "DATETIME(0)")
    private LocalDateTime updatedAt;

    @Column(columnDefinition = "DATETIME(0)")
    private LocalDateTime tokenExpiryDate;

    public static Member of(String email, String password, String name,
                            String imageUrl, String provider,
                            String providerId, Set<MemberRole> roles) {
        Member member = new Member();
        member.email = email;
        member.password = password;
        member.name = name;
        member.imageUrl = imageUrl;
        member.provider = provider;
        member.providerId = providerId;
        member.roles = roles;
        return member;
    }
    public void updateOAuth2Info(String name, String imageUrl) {
        this.name = name;
        if (imageUrl != null && !imageUrl.isEmpty()) {
            this.imageUrl = imageUrl;
        }
    }

    public void updateRefreshToken(String refreshToken, LocalDateTime tokenExpiryDate) {
        this.refreshToken = refreshToken;
        this.tokenExpiryDate = tokenExpiryDate;
    }

    public void clearRefreshToken() {
        this.refreshToken = null;
        this.tokenExpiryDate = null;
    }

    public void updateImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    public void updatePassword(String newPassword) {
        this.password = newPassword;
    }
}
