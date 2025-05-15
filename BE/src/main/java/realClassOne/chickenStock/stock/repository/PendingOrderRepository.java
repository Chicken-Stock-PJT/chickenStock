package realClassOne.chickenStock.stock.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.entity.TradeHistory;

import java.util.List;

@Repository
public interface PendingOrderRepository extends JpaRepository<PendingOrder, Long> {

    // 상태별 조회
    List<PendingOrder> findByStatus(PendingOrder.OrderStatus status);

    Page<PendingOrder> findPageByStatus(PendingOrder.OrderStatus status, Pageable pageable);

    List<PendingOrder> findByMemberAndStatus(Member member, PendingOrder.OrderStatus status);

    // 상태별 정렬 조회
    List<PendingOrder> findByStatusOrderByCreatedAtAsc(PendingOrder.OrderStatus status);

    // 회원별 상태별 조회
    List<PendingOrder> findByMemberAndStatusOrderByCreatedAtDesc(Member member, PendingOrder.OrderStatus status);

    // 종목별 미체결 주문 존재 여부
    @Query("SELECT COUNT(po) > 0 FROM PendingOrder po WHERE po.stockData.shortCode = :stockCode AND po.status = 'PENDING'")
    boolean existsActivePendingOrderForStock(@Param("stockCode") String stockCode);

    // 종목별, 상태별 존재 여부
    boolean existsByStockDataShortCodeAndStatus(String stockCode, PendingOrder.OrderStatus status);

    // 회원의 특정 종목에 대한 대기 중인 매도 주문 수량 합계
    @Query("SELECT COALESCE(SUM(po.quantity), 0) FROM PendingOrder po " +
            "WHERE po.member.memberId = :memberId AND po.stockData.shortCode = :stockCode " +
            "AND po.orderType = 'SELL' AND po.status = 'PENDING'")
    int getTotalPendingSellQuantityForMemberAndStock(@Param("memberId") Long memberId,
                                                     @Param("stockCode") String stockCode);

    // 체결 가능한 매수 주문 조회 (현재가가 지정가 이하)
    @Query("SELECT po FROM PendingOrder po " +
            "JOIN FETCH po.member " +
            "JOIN FETCH po.stockData " +
            "WHERE po.stockData.shortCode = :stockCode " +
            "AND po.orderType = :orderType " +
            "AND po.status = 'PENDING' " +
            "AND po.targetPrice >= :currentPrice " +
            "ORDER BY po.targetPrice DESC, po.createdAt ASC")
    List<PendingOrder> findExecutableBuyOrders(@Param("stockCode") String stockCode,
                                               @Param("orderType") TradeHistory.TradeType orderType,
                                               @Param("currentPrice") Long currentPrice);

    // 체결 가능한 매도 주문 조회 (현재가가 지정가 이상)
    @Query("SELECT po FROM PendingOrder po " +
            "JOIN FETCH po.member " +
            "JOIN FETCH po.stockData " +
            "WHERE po.stockData.shortCode = :stockCode " +
            "AND po.orderType = :orderType " +
            "AND po.status = 'PENDING' " +
            "AND po.targetPrice <= :currentPrice " +
            "ORDER BY po.targetPrice ASC, po.createdAt ASC")
    List<PendingOrder> findExecutableSellOrders(@Param("stockCode") String stockCode,
                                                @Param("orderType") TradeHistory.TradeType orderType,
                                                @Param("currentPrice") Long currentPrice);

    // 체결 가능한 주문 통합 조회
    @Query("SELECT po FROM PendingOrder po " +
            "JOIN FETCH po.member " +
            "JOIN FETCH po.stockData " +
            "WHERE po.stockData.shortCode = :stockCode " +
            "AND po.orderType = :orderType " +
            "AND po.status = 'PENDING' " +
            "AND ((po.orderType = 'BUY' AND po.targetPrice >= :currentPrice) " +
            "     OR (po.orderType = 'SELL' AND po.targetPrice <= :currentPrice)) " +
            "ORDER BY po.createdAt ASC")
    List<PendingOrder> findExecutableOrdersWithRelations(@Param("stockCode") String stockCode,
                                                         @Param("orderType") TradeHistory.TradeType orderType,
                                                         @Param("currentPrice") Long currentPrice);
}