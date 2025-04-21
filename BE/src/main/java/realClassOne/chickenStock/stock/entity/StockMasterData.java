package realClassOne.chickenStock.stock.entity;

import jakarta.persistence.*;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "stock_master_data")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class StockMasterData {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "stock_data_id")
    private Long stockDataId;

    @Column(name = "short_code")
    private String shortCode; // 단축코드 (6자리)

    @Column(name = "short_name", nullable = false)
    private String shortName;  // 한글 종목약명

    @Column(name = "market", nullable = false)
    private String market;  // 시장구분 (KOSPI, KOSDAQ 등)

    @Column(name = "stock_type")
    private String stockType;   // 주식종류

    @Column(name = "face_value")
    private String faceValue;  // 액면가

    @OneToMany(mappedBy = "stockData", cascade = CascadeType.ALL)
    private List<TradeHistory> tradeHistories = new ArrayList<>();

    @OneToMany(mappedBy = "stockData", cascade = CascadeType.ALL)
    private List<HoldingPosition> holdingPositions = new ArrayList<>();

    private StockMasterData(String shortCode, String shortName, String market, String stockType, String faceValue) {
        this.shortCode = shortCode;
        this.shortName = shortName;
        this.market = market;
        this.stockType = stockType != null ? stockType : "보통주";
        this.faceValue = faceValue;
    }

    public static StockMasterData of(String shortCode, String shortName, String market, String stockType, String faceValue) {
        return new StockMasterData(shortCode, shortName, market, stockType, faceValue);
    }

    public static StockMasterData of(String shortCode, String shortName, String market) {
        return new StockMasterData(shortCode, shortName, market, "일반", null);
    }

    // 양방향 관계 관리를 위한 메서드 추가
    public void addTradeHistory(TradeHistory tradeHistory) {
        this.tradeHistories.add(tradeHistory);
    }

    public void addHoldingPosition(HoldingPosition holdingPosition) {
        this.holdingPositions.add(holdingPosition);
    }

}
