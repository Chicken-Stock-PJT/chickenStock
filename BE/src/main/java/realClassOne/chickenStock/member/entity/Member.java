package realClassOne.chickenStock.member.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.entity.TradeHistory;
import realClassOne.chickenStock.stock.exception.StockErrorCode;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Entity
@Table(name = "member")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@EntityListeners(AuditingEntityListener.class)
public class Member {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "member_id")
    private Long memberId;

    @Column(name = "email", nullable = false, unique = true)
    private String email;

    @Column(name = "member_money")
    private Long memberMoney;

    @Column(name = "name")
    private String name;

    @Column(name = "nickname")
    private String nickname;

    @Column(name = "password")
    private String password;

    @Column(name = "profile_image")
    private String profileImage;

    @Column(name = "provider")
    private String provider;

    @Column(name = "provider_id")
    private String providerId;

    @Column(name = "refresh_token")
    private String refreshToken;

    @ElementCollection(fetch = FetchType.EAGER)
    @Enumerated(EnumType.STRING)
    @CollectionTable(name = "user_roles", joinColumns = @JoinColumn(name = "user_id"))
    @Column(name = "roles")
    private Set<MemberRole> roles = new HashSet<>();

    @CreatedDate
    @Column(name = "created_at", nullable = false, updatable = false, columnDefinition = "DATETIME(0)")
    private LocalDateTime createdAt;

    @LastModifiedDate
    @Column(name = "updated_at", nullable = false, columnDefinition = "DATETIME(0)")
    private LocalDateTime updatedAt;

    @Column(name = "token_expiry_date", columnDefinition = "DATETIME(0)")
    private LocalDateTime tokenExpiryDate;

    @OneToMany(mappedBy = "member", cascade = CascadeType.ALL)
    private List<TradeHistory> tradeHistories = new ArrayList<>();

    @OneToMany(mappedBy = "member", cascade = CascadeType.ALL)
    private List<PendingOrder> pendingOrders = new ArrayList<>();

    @OneToMany(mappedBy = "member", cascade = CascadeType.ALL)
    private List<HoldingPosition> holdingPositions = new ArrayList<>();

    @OneToOne(mappedBy = "member", cascade = CascadeType.ALL)
    private InvestmentSummary investmentSummary;

    public static Member of(String email, String password, String name,
                            String imageUrl, String provider,
                            String providerId, Set<MemberRole> roles) {
        Member member = new Member();
        member.email = email;
        member.password = password;
        member.name = name;
        member.profileImage = imageUrl;
        member.provider = provider;
        member.providerId = providerId;
        member.roles = roles;
        member.memberMoney = 0L; // 초기 금액 설정
        return member;
    }

    public void updateOAuth2Info(String name, String imageUrl) {
        this.name = name;
        if (imageUrl != null && !imageUrl.isEmpty()) {
            this.profileImage = imageUrl;
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
        this.profileImage = imageUrl;
    }

    // 양방향 관계 관리를 위한 메서드 추가
    public void addTradeHistory(TradeHistory tradeHistory) {
        this.tradeHistories.add(tradeHistory);
    }

    public void addHoldingPosition(HoldingPosition holdingPosition) {
        this.holdingPositions.add(holdingPosition);
    }

    public void addPendingOrder(PendingOrder pendingOrder) {
        this.pendingOrders.add(pendingOrder);
    }

    public void setInvestmentSummary(InvestmentSummary investmentSummary) {
        this.investmentSummary = investmentSummary;
    }

    public void updateMemberMoney(Long amount) {
        this.memberMoney = amount;
    }

    public void addMemberMoney(Long amount) {
        this.memberMoney += amount;
    }

    public void subtractMemberMoney(Long amount) {
        if (this.memberMoney < amount) {
            throw new CustomException(StockErrorCode.INSUFFICIENT_BALANCE);
        }
        this.memberMoney -= amount;
    }

    public void updatePassword(String newPassword) {
        this.password = newPassword;
    }

    public void changeNickname(String nickname) {
        this.nickname = nickname;
    }
}
