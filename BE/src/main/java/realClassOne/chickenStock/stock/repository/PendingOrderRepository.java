package realClassOne.chickenStock.stock.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.entity.TradeHistory;

import java.util.List;

public interface PendingOrderRepository extends JpaRepository<PendingOrder, Long> {

    List<PendingOrder> findByMemberAndStatus(Member member, PendingOrder.OrderStatus status);

    List<PendingOrder> findByStatus(PendingOrder.OrderStatus status);

    @Query("SELECT po FROM PendingOrder po WHERE po.stockData.shortCode = :stockCode AND po.orderType = :orderType " +
            "AND po.status = realClassOne.chickenStock.stock.entity.PendingOrder$OrderStatus.PENDING " +
            "AND ((po.orderType = realClassOne.chickenStock.stock.entity.TradeHistory$TradeType.BUY AND po.targetPrice >= :price) " +
            "OR (po.orderType = realClassOne.chickenStock.stock.entity.TradeHistory$TradeType.SELL AND po.targetPrice <= :price))")
    List<PendingOrder> findExecutableOrders(@Param("stockCode") String stockCode,
                                            @Param("orderType") TradeHistory.TradeType orderType,
                                            @Param("price") Long price);

    @Query("SELECT po FROM PendingOrder po JOIN FETCH po.stockData WHERE po.status = realClassOne.chickenStock.stock.entity.PendingOrder$OrderStatus.PENDING")
    List<PendingOrder> findPendingOrdersWithStockData();
}