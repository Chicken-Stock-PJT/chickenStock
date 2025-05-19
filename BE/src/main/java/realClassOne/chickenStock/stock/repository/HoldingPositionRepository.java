package realClassOne.chickenStock.stock.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.entity.StockData;

import java.util.List;
import java.util.Optional;

@Repository
public interface HoldingPositionRepository extends JpaRepository<HoldingPosition, Long> {
    Optional<HoldingPosition> findByMemberAndStockData(Member member, StockData stockData);
    List<HoldingPosition> findByMember(Member member);

    // 새로 추가할 메서드: Eager Loading 활용
    @Query("SELECT hp FROM HoldingPosition hp JOIN FETCH hp.stockData WHERE hp.member.memberId = :memberId")
    List<HoldingPosition> findWithStockDataByMemberId(@Param("memberId") Long memberId);

    // 특정 종목 코드에 대해 필터링
    @Query("SELECT hp FROM HoldingPosition hp JOIN FETCH hp.stockData sd WHERE hp.member.memberId = :memberId AND sd.shortCode = :stockCode")
    List<HoldingPosition> findWithStockDataByMemberIdAndStockCode(@Param("memberId") Long memberId, @Param("stockCode") String stockCode);

    boolean existsByStockDataShortCode(String stockCode);

    @Query("SELECT hp FROM HoldingPosition hp JOIN FETCH hp.stockData WHERE hp.member = :member")
    List<HoldingPosition> findByMemberWithStockData(@Param("member") Member member);

    Optional<HoldingPosition> findByMemberAndStockDataAndActiveTrue(Member member, StockData stockData);

    List<HoldingPosition> findByMemberAndActiveTrue(Member member);

    // active 상태 업데이트를 위한 메서드 추가
    @Modifying
    @Query("UPDATE HoldingPosition h SET h.active = :active WHERE h.stockHoldingId = :id")
    int updateActiveStatus(@Param("id") Long id, @Param("active") boolean active);

    @Modifying
    @Query(value = "DELETE FROM holding_position WHERE member_id = :memberId AND stock_data_id = :stockDataId",
            nativeQuery = true)
    int deleteByMemberIdAndStockDataIdNative(@Param("memberId") Long memberId, @Param("stockDataId") Long stockDataId);

    List<HoldingPosition> findAllByMemberAndStockDataAndActiveTrue(Member member, StockData stockData);

    // 풀매도를 위한 메서드
    Optional<HoldingPosition> findByMemberIdAndStockDataId(Long memberId, Long stockDataId);
}