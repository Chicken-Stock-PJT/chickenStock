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

    List<TradeHistory> findByMember(Member member);

    // 특정 시점 이전의 거래 내역을 최신순으로 조회합니다.
    @Query("SELECT t FROM TradeHistory t WHERE t.member.memberId = :memberId AND t.tradedAt <= :dateTime ORDER BY t.tradedAt DESC")
    List<TradeHistory> findByMemberIdAndTradedAtBefore(@Param("memberId") Long memberId, @Param("dateTime") LocalDateTime dateTime);

    // 특정 기간 동안의 거래 내역을 조회합니다.
//    * @param memberId 회원 ID
//     * @param startDateTime 시작 시점
//     * @param endDateTime 종료 시점
//     * @return 거래 내역 목록
    @Query("SELECT t FROM TradeHistory t WHERE t.member.memberId = :memberId AND t.tradedAt BETWEEN :startDateTime AND :endDateTime ORDER BY t.tradedAt ASC")
    List<TradeHistory> findByMemberIdAndTradedAtBetween(
            @Param("memberId") Long memberId,
            @Param("startDateTime") LocalDateTime startDateTime,
            @Param("endDateTime") LocalDateTime endDateTime);
}