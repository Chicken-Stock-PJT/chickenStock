package realClassOne.chickenStock.stock.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.member.entity.Member;

import java.time.LocalDateTime;

@Entity
@Table(name = "pending_order")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class PendingOrder {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "order_id")
    private Long orderId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "member_id", nullable = false)
    private Member member;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "stock_data_id", nullable = false)
    private StockData stockData;

    @Enumerated(EnumType.STRING)
    @Column(name = "order_type", nullable = false)
    private TradeHistory.TradeType orderType;

    @Column(name = "quantity", nullable = false)
    private Integer quantity;

    @Column(name = "target_price", nullable = false)
    private Long targetPrice;

    @Column(name = "reserved_fee")
    private Long reservedFee; // 매수 주문시 예약된 수수료

    @Column(name = "expected_fee")
    private Long expectedFee; // 매도 주문시 예상 수수료

    @Column(name = "expected_tax")
    private Long expectedTax; // 매도 주문시 예상 세금

    @Column(name = "created_at", nullable = false, columnDefinition = "DATETIME(0)")
    private LocalDateTime createdAt;

    @Column(name = "status", nullable = false)
    @Enumerated(EnumType.STRING)
    private OrderStatus status;

    public enum OrderStatus {
        PENDING,      // 대기 상태
        PROCESSING,   // 처리 진행중
        COMPLETED,    // 완료됨
        CANCELED,     // 취소됨
        FAILED        // 실패함
    }

    private PendingOrder(Member member, StockData stockData, TradeHistory.TradeType orderType,
                         Integer quantity, Long targetPrice, OrderStatus status) {
        this.member = member;
        this.stockData = stockData;
        this.orderType = orderType;
        this.quantity = quantity;
        this.targetPrice = targetPrice;
        this.status = status;
        this.createdAt = LocalDateTime.now();
    }

    // 매수 주문용 정적 팩토리 메서드
    public static PendingOrder createBuyOrder(Member member, StockData stockData,
                                              Integer quantity, Long targetPrice, Long reservedFee) {
        PendingOrder order = new PendingOrder(member, stockData, TradeHistory.TradeType.BUY,
                quantity, targetPrice, OrderStatus.PENDING);
        order.reservedFee = reservedFee;
        return order;
    }

    // 매도 주문용 정적 팩토리 메서드
    public static PendingOrder createSellOrder(Member member, StockData stockData,
                                               Integer quantity, Long targetPrice,
                                               Long expectedFee, Long expectedTax) {
        PendingOrder order = new PendingOrder(member, stockData, TradeHistory.TradeType.SELL,
                quantity, targetPrice, OrderStatus.PENDING);
        order.expectedFee = expectedFee;
        order.expectedTax = expectedTax;
        return order;
    }

    // 기존 of 메서드 (하위 호환성을 위해 유지)
    public static PendingOrder of(Member member, StockData stockData, TradeHistory.TradeType orderType,
                                  Integer quantity, Long targetPrice) {
        return new PendingOrder(member, stockData, orderType, quantity, targetPrice, OrderStatus.PENDING);
    }

    // 수수료/세금 정보 업데이트 메서드
    public void updateBuyOrderFees(Long reservedFee) {
        if (this.orderType != TradeHistory.TradeType.BUY) {
            throw new IllegalStateException("매수 주문이 아닌 경우 예약 수수료를 설정할 수 없습니다.");
        }
        this.reservedFee = reservedFee;
    }

    public void updateSellOrderFees(Long expectedFee, Long expectedTax) {
        if (this.orderType != TradeHistory.TradeType.SELL) {
            throw new IllegalStateException("매도 주문이 아닌 경우 예상 수수료/세금을 설정할 수 없습니다.");
        }
        this.expectedFee = expectedFee;
        this.expectedTax = expectedTax;
    }

    public void complete() {
        this.status = OrderStatus.COMPLETED;
    }

    public void cancel() {
        this.status = OrderStatus.CANCELED;
    }

    /**
     * 주문을 처리 중 상태로 변경합니다.
     * 이 상태는 주문이 체결 진행 중임을 나타냅니다.
     */
    public void processing() {
        if (this.status != OrderStatus.PENDING) {
            throw new IllegalStateException("대기 상태인 주문만 처리 중으로 변경할 수 있습니다.");
        }
        this.status = OrderStatus.PROCESSING;
    }

    /**
     * 주문 처리 실패 시 상태를 실패로 변경합니다.
     * 보다 유연하게 작동하도록 수정
     */
    public void fail() {
        // 이미 완료/취소된 주문은 실패로 변경하지 않음
        if (this.status == OrderStatus.COMPLETED || this.status == OrderStatus.CANCELED) {
            return;
        }
        this.status = OrderStatus.FAILED;
    }

    /**
     * 현재 주문이 취소 가능한 상태인지 확인합니다.
     * @return 취소 가능 여부
     */
    public boolean isCancelable() {
        return this.status == OrderStatus.PENDING;
    }

    /**
     * 주문의 상태가 최종 상태(완료, 취소, 실패)인지 확인합니다.
     * @return 최종 상태 여부
     */
    public boolean isFinalized() {
        return this.status == OrderStatus.COMPLETED ||
                this.status == OrderStatus.CANCELED ||
                this.status == OrderStatus.FAILED;
    }
}