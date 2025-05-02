package realClassOne.chickenStock.stock.repository;

import org.springframework.data.jpa.repository.JpaRepository;
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
    List<HoldingPosition> findByStockData(StockData stockData);
    void deleteByMemberAndStockData(Member member, StockData stockData);
    List<HoldingPosition> findAllByMember_MemberId(Long memberId);

    // 새로 추가할 메서드: Eager Loading 활용
    @Query("SELECT hp FROM HoldingPosition hp JOIN FETCH hp.stockData WHERE hp.member.memberId = :memberId")
    List<HoldingPosition> findWithStockDataByMemberId(@Param("memberId") Long memberId);

    // 특정 종목 코드에 대해 필터링
    @Query("SELECT hp FROM HoldingPosition hp JOIN FETCH hp.stockData sd WHERE hp.member.memberId = :memberId AND sd.shortCode = :stockCode")
    List<HoldingPosition> findWithStockDataByMemberIdAndStockCode(@Param("memberId") Long memberId, @Param("stockCode") String stockCode);

    boolean existsByStockDataShortCode(String stockCode);

}