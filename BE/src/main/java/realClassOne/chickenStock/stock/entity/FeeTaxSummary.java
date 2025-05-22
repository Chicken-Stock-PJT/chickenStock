package realClassOne.chickenStock.stock.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.LocalDateTime;

@Entity
@Table(name = "fee_tax_summary")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@EntityListeners(AuditingEntityListener.class)
public class FeeTaxSummary {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "fee_tax_id")
    private Long feeTaxId;

    @Column(name = "total_buy_fee", nullable = false)
    private Long totalBuyFee;

    @Column(name = "total_sell_fee", nullable = false)
    private Long totalSellFee;

    @Column(name = "total_sell_tax", nullable = false)
    private Long totalSellTax;

    @Column(name = "total_amount", nullable = false)
    private Long totalAmount;

    @Version
    private Long version; // 낙관적 락을 위한 버전 필드

    @CreatedDate
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @Column(name = "last_updated_at")
    private LocalDateTime lastUpdatedAt;

    private FeeTaxSummary(Long totalBuyFee, Long totalSellFee, Long totalSellTax) {
        this.totalBuyFee = totalBuyFee;
        this.totalSellFee = totalSellFee;
        this.totalSellTax = totalSellTax;
        this.totalAmount = totalBuyFee + totalSellFee + totalSellTax;
        this.lastUpdatedAt = LocalDateTime.now();
    }

    public static FeeTaxSummary create() {
        return new FeeTaxSummary(0L, 0L, 0L);
    }

    public void addBuyFee(Long fee) {
        this.totalBuyFee += fee;
        this.totalAmount += fee;
        this.lastUpdatedAt = LocalDateTime.now();
    }

    public void addSellFee(Long fee) {
        this.totalSellFee += fee;
        this.totalAmount += fee;
        this.lastUpdatedAt = LocalDateTime.now();
    }

    public void addSellTax(Long tax) {
        this.totalSellTax += tax;
        this.totalAmount += tax;
        this.lastUpdatedAt = LocalDateTime.now();
    }
}