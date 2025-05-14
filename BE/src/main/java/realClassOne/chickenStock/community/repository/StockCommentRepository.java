package realClassOne.chickenStock.community.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import realClassOne.chickenStock.community.entity.StockComment;

import java.util.List;
import java.util.Optional;

public interface StockCommentRepository extends JpaRepository<StockComment, Long> {

    // 특정 종목의 부모 댓글만 조회
    List<StockComment> findByStockDataStockDataIdAndParentIsNullOrderByCreatedAtDesc(Long stockId);

    // 특정 댓글의 대댓글 조회
    List<StockComment> findByParentIdOrderByCreatedAt(Long parentId);

    // 부모 id로 댓글 찾기
    Optional<StockComment> findById(Long id);
}

