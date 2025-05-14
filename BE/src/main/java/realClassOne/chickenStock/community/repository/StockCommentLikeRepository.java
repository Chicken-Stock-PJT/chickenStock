package realClassOne.chickenStock.community.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import realClassOne.chickenStock.community.entity.StockComment;
import realClassOne.chickenStock.community.entity.StockCommentLike;
import realClassOne.chickenStock.member.entity.Member;

import java.util.Optional;

public interface StockCommentLikeRepository extends JpaRepository<StockCommentLike, Long> {
    Optional<StockCommentLike> findByMemberAndStockComment(Member member, StockComment stockComment);
    long countByStockComment(StockComment stockComment);

    long countByStockComment_Id(Long commentId);
}
