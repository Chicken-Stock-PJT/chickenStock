package realClassOne.chickenStock.stock.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.entity.TradeHistory;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface TradeHistoryRepository extends JpaRepository<TradeHistory, Long> {
    List<TradeHistory> findByMember(Member member);

    void deleteByMember(Member member);

    List<TradeHistory> findByMemberAndStockData(Member member, StockData stockData);

    // 특정 시점 이전의 거래 내역을 최신순으로 조회합니다.
    @Query("SELECT t FROM TradeHistory t WHERE t.member.memberId = :memberId AND t.tradedAt <= :dateTime ORDER BY t.tradedAt DESC")
    List<TradeHistory> findByMemberIdAndTradedAtBefore(@Param("memberId") Long memberId, @Param("dateTime") LocalDateTime dateTime);

    // 유저 별 거래 내역 조회
    // 커서 기반 조회 - createdAt < 기준시간 조건 + 정렬 + 페이징
    List<TradeHistory> findByMemberAndCreatedAtBefore(
            Member member,
            LocalDateTime createdAt,
            Pageable pageable
    );

    // 특정 회원의 모든 거래 내역을 조회합니다.
    @Query("SELECT t FROM TradeHistory t WHERE t.member.memberId = :memberId ORDER BY t.tradedAt ASC")
    List<TradeHistory> findByMemberId(@Param("memberId") Long memberId);

    List<TradeHistory> findByMemberOrderByCreatedAtDesc(Member member);

    @Query("SELECT t FROM TradeHistory t WHERE t.member = :member AND t.tradedAt >= :startDate ORDER BY t.tradedAt DESC")
    List<TradeHistory> findTodayTradesByMember(@Param("member") Member member, @Param("startDate") LocalDateTime startDate);

    @Query("SELECT t FROM TradeHistory t WHERE t.member = :member AND t.tradedAt BETWEEN :startDate AND :endDate")
    List<TradeHistory> findByMemberAndTradedAtBetween(
            @Param("member") Member member,
            @Param("startDate") LocalDateTime startDate,
            @Param("endDate") LocalDateTime endDate);

    @Query("SELECT t FROM TradeHistory t JOIN FETCH t.stockData WHERE t.member = :member")
    List<TradeHistory> findWithStockDataByMember(@Param("member") Member member);

    @Query("SELECT t FROM TradeHistory t WHERE t.member = :member AND t.stockData = :stockData AND t.tradedAt < :dateTime ORDER BY t.tradedAt ASC")
    List<TradeHistory> findByMemberAndStockDataAndTradedAtBefore(
            @Param("member") Member member,
            @Param("stockData") StockData stockData,
            @Param("dateTime") LocalDateTime dateTime);
}