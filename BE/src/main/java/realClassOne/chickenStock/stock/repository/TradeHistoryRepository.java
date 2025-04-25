package realClassOne.chickenStock.stock.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.entity.TradeHistory;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface TradeHistoryRepository extends JpaRepository<TradeHistory, Long> {

    @Query("SELECT th FROM TradeHistory th WHERE th.member = :member AND th.tradedAt BETWEEN :startDate AND :endDate ORDER BY th.tradedAt DESC")
    List<TradeHistory> findByMemberAndTradedAtBetween(
            @Param("member") Member member,
            @Param("startDate") LocalDateTime startDate,
            @Param("endDate") LocalDateTime endDate);

    @Query("SELECT th FROM TradeHistory th WHERE th.member = :member AND th.stockData = :stockData AND th.tradedAt BETWEEN :startDate AND :endDate ORDER BY th.tradedAt DESC")
    List<TradeHistory> findByMemberAndStockDataAndTradedAtBetween(
            @Param("member") Member member,
            @Param("stockData") StockData stockData,
            @Param("startDate") LocalDateTime startDate,
            @Param("endDate") LocalDateTime endDate);
}