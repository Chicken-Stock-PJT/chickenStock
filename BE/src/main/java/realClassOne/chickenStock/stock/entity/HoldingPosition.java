package realClassOne.chickenStock.stock.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.member.entity.Member;

@Entity
@Table(name = "holding_position")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class HoldingPosition {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "stock_holding_id")
    private Long stockHoldingId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "member_id", nullable = false)
    private Member member;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "stock_data_id", nullable = false)
    private StockData stockData;

    @Column(name = "quantity", nullable = false)
    private Integer quantity;

    @Column(name = "average_price", nullable = false)
    private Long averagePrice;

    @Column(name = "current_profit", nullable = false)
    private Long currentProfit;

    @Column(name = "return_rate", nullable = false)
    private Double returnRate;

    private HoldingPosition(Member member, StockData stockData, Integer quantity,
                            Long averagePrice, Long currentProfit, Double returnRate) {
        this.member = member;
        this.stockData = stockData;
        this.quantity = quantity;
        this.averagePrice = averagePrice;
        this.currentProfit = currentProfit;
        this.returnRate = returnRate;
    }

    public static HoldingPosition of(Member member, StockData stockData, Integer quantity,
                                     Long averagePrice, Long currentProfit, Double returnRate) {
        return new HoldingPosition(member, stockData, quantity, averagePrice, currentProfit, returnRate);
    }

    public void updatePosition(Integer quantity, Long averagePrice, Long currentProfit, Double returnRate) {
        this.quantity = quantity;
        this.averagePrice = averagePrice;
        this.currentProfit = currentProfit;
        this.returnRate = returnRate;
    }
}
