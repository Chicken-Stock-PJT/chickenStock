package realClassOne.chickenStock.stock.repository;

import org.springframework.data.jpa.repository.JpaRepository;
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
}