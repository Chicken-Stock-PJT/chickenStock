package realClassOne.chickenStock.stock.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import realClassOne.chickenStock.stock.entity.StockMasterData;

import java.util.Optional;

@Repository
public interface StockRepository extends JpaRepository<StockMasterData, String> {
    Optional<StockMasterData> findByShortName(String shortName);
}