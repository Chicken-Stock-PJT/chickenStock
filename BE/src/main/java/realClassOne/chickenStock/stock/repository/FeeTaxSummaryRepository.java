package realClassOne.chickenStock.stock.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import realClassOne.chickenStock.stock.entity.FeeTaxSummary;

import java.util.Optional;

@Repository
public interface FeeTaxSummaryRepository extends JpaRepository<FeeTaxSummary, Long> {
    @Query("SELECT f FROM FeeTaxSummary f ORDER BY f.createdAt DESC")
    Optional<FeeTaxSummary> findLatest();
}