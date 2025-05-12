package realClassOne.chickenStock.stock.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.entity.TradeHistory;

import java.util.List;
import java.util.Optional;

public interface PendingOrderRepository extends JpaRepository<PendingOrder, Long> {

    void deleteByMember(Member member);

    List<PendingOrder> findByMemberAndStatus(Member member, PendingOrder.OrderStatus status);

    @Query("SELECT po FROM PendingOrder po JOIN FETCH po.stockData WHERE po.status = realClassOne.chickenStock.stock.entity.PendingOrder$OrderStatus.PENDING")
    List<PendingOrder> findPendingOrdersWithStockData();

    boolean existsByStockDataShortCodeAndStatus(String stockCode, PendingOrder.OrderStatus status);

    @Query("SELECT po FROM PendingOrder po JOIN FETCH po.stockData JOIN FETCH po.member WHERE po.stockData.shortCode = :stockCode AND po.orderType = :orderType " +
            "AND po.status = realClassOne.chickenStock.stock.entity.PendingOrder$OrderStatus.PENDING " +
            "AND ((po.orderType = realClassOne.chickenStock.stock.entity.TradeHistory$TradeType.BUY AND po.targetPrice >= :price) " +
            "OR (po.orderType = realClassOne.chickenStock.stock.entity.TradeHistory$TradeType.SELL AND po.targetPrice <= :price))")
    List<PendingOrder> findExecutableOrdersWithRelations(@Param("stockCode") String stockCode,
                                                         @Param("orderType") TradeHistory.TradeType orderType,
                                                         @Param("price") Long price);


    @Query(value = "SELECT COUNT(po) > 0 FROM PendingOrder po " +
            "WHERE po.stockData.shortCode = :stockCode " +
            "AND po.status = realClassOne.chickenStock.stock.entity.PendingOrder$OrderStatus.PENDING")
    boolean existsActivePendingOrderForStock(@Param("stockCode") String stockCode);

    Page<PendingOrder> findPageByStatus(PendingOrder.OrderStatus status, Pageable pageable);

    @Query("SELECT COALESCE(SUM(p.quantity), 0) FROM PendingOrder p " +
            "WHERE p.member.memberId = :memberId " +
            "AND p.stockData.shortCode = :stockCode " +
            "AND p.orderType = 'SELL' " +
            "AND p.status = 'PENDING'")
    int getTotalPendingSellQuantityForMemberAndStock(@Param("memberId") Long memberId,
                                                     @Param("stockCode") String stockCode);
}