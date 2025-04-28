package realClassOne.chickenStock.member.entity;


import jakarta.persistence.*;
import lombok.*;
import realClassOne.chickenStock.stock.entity.StockData;

import java.time.LocalDateTime;

@Entity
@Table(name = "watch_list",
        uniqueConstraints = @UniqueConstraint(columnNames = {"member_id", "stock_data_id"}))
@Getter
@Builder
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
public class WatchList {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "watch_list_id")
    private Long watchListId;

    @ManyToOne(fetch = FetchType.LAZY)
    private Member member;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "stock_data_id", nullable = false)
    private StockData stockData;

    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;
}
