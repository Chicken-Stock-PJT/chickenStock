package realClassOne.chickenStock.member.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.entity.WatchList;
import realClassOne.chickenStock.stock.entity.StockData;

import java.util.List;

@Repository
public interface WatchListRepository extends JpaRepository<WatchList, Long> {

    /**
     * 특정 회원의 관심종목 목록을 조회합니다.
     *
     * @param member 회원 엔티티
     * @return 관심종목 목록
     */
    List<WatchList> findByMember(Member member);

    /**
     * 특정 회원이 특정 종목을 관심종목으로 등록했는지 확인합니다.
     *
     * @param member 회원 엔티티
     * @param stockData 주식 데이터 엔티티
     * @return 등록 여부
     */
    boolean existsByMemberAndStockData(Member member, StockData stockData);

    /**
     * 특정 회원의 특정 종목을 관심종목에서 삭제합니다.
     *
     * @param member 회원 엔티티
     * @param stockData 주식 데이터 엔티티
     */
    void deleteByMemberAndStockData(Member member, StockData stockData);
}
