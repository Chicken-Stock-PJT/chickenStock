package realClassOne.chickenStock.member.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Entity
@Table(name = "investment_summary")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class InvestmentSummary {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "investment_summary_id")
    private Long InvestmentSummaryId;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "member_id", nullable = false)
    private Member member;

    @Column(name = "total_investment", nullable = false)
    private Long totalInvestment;

    @Column(name = "total_valuation", nullable = false)
    private Long totalValuation;

    @Column(name = "total_profit_loss", nullable = false)
    private Long totalProfitLoss;

    @Column(name = "return_rate", nullable = false)
    private Double returnRate;

    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    private InvestmentSummary(Member member, Long totalInvestment, Long totalValuation,
                              Long totalProfitLoss, Double returnRate) {
        this.member = member;
        this.totalInvestment = totalInvestment;
        this.totalValuation = totalValuation;
        this.totalProfitLoss = totalProfitLoss;
        this.returnRate = returnRate;
        this.updatedAt = LocalDateTime.now();
    }

    public static InvestmentSummary of(Member member, Long totalInvestment, Long totalValuation,
                                       Long totalProfitLoss, Double returnRate) {
        InvestmentSummary summary = new InvestmentSummary(member, totalInvestment, totalValuation, totalProfitLoss, returnRate);
        member.setInvestmentSummary(summary); // 양방향 관계 설정
        return summary;
    }

    public void updateValues(Long totalInvestment, Long totalValuation, Long totalProfitLoss, Double returnRate) {
        this.totalInvestment = totalInvestment;
        this.totalValuation = totalValuation;
        this.totalProfitLoss = totalProfitLoss;
        this.returnRate = returnRate;
    }

    @PrePersist
    @PreUpdate
    public void preUpdate() {
        this.updatedAt = LocalDateTime.now();
    }
}
