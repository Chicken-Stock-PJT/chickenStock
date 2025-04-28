package realClassOne.chickenStock.stock.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import realClassOne.chickenStock.stock.entity.StockData;

import java.util.Optional;

@Repository
public interface StockDataRepository extends JpaRepository<StockData, String> {
    Optional<StockData> findByShortName(String shortName);
    Optional<StockData> findByShortCode(String shortCode);
}