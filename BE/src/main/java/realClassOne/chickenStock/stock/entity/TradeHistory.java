package realClassOne.chickenStock.stock.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.member.entity.Member;

import java.time.LocalDateTime;

@Entity
@Table(name = "trade_history")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class TradeHistory {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "trade_history_id")
    private Long tradeHistoryId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "member_id", nullable = false)
    private Member member;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "stock_data_id", nullable = false)
    private StockData stockData;

    @Enumerated(EnumType.STRING)
    @Column(name = "trade_type", nullable = false)
    private TradeType tradeType;

    @Column(name = "quantity", nullable = false)
    private Integer quantity;

    @Column(name = "unit_price", nullable = false)
    private Long unitPrice;

    @Column(name = "total_price", nullable = false)
    private Long totalPrice;

    @Column(name = "fee", nullable = false)
    private Long fee = 0L;

    @Column(name = "tax", nullable = false)
    private Long tax = 0L;

    @Column(name = "created_at", nullable = false, columnDefinition = "DATETIME(0)")
    private LocalDateTime createdAt;

    @Column(name = "traded_at", columnDefinition = "DATETIME(0)")
    private LocalDateTime tradedAt;

    private TradeHistory(Member member, StockData stockData, TradeType tradeType,
                         Integer quantity, Long unitPrice, Long totalPrice,
                         Long fee, Long tax, LocalDateTime tradedAt) {
        this.member = member;
        this.stockData = stockData;
        this.tradeType = tradeType;
        this.quantity = quantity;
        this.unitPrice = unitPrice;
        this.totalPrice = totalPrice;
        this.fee = fee;
        this.tax = tax;
        this.tradedAt = tradedAt;
    }

    // 팩토리 메서드 수정
    public static TradeHistory of(Member member, StockData stockData, TradeType tradeType,
                                  Integer quantity, Long unitPrice, Long totalPrice,
                                  Long fee, Long tax, LocalDateTime tradedAt) {
        return new TradeHistory(member, stockData, tradeType, quantity, unitPrice,
                totalPrice, fee, tax, tradedAt);
    }

    @PrePersist
    public void prePersist() {
        this.createdAt = LocalDateTime.now();
    }

    public void setQuantity(int i) {
    }

    public enum TradeType {
        BUY, SELL
    }
}
