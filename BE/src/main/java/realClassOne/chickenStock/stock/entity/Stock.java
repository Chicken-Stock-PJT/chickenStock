package realClassOne.chickenStock.stock.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "stocks")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Stock {
    @Id
    @Column(length = 10)
    private String shortCode;     // 단축코드 (6자리)

    @Column(length = 100)
    private String shortName;     // 한글 종목약명

    @Column(length = 20)
    private String market;        // 시장구분 (KOSPI, KOSDAQ 등)

    @Column(length = 50)
    private String stockType;     // 주식종류

    @Column(length = 20)
    private String faceValue;     // 액면가
}
