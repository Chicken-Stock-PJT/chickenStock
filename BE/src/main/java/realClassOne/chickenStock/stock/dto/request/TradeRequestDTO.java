package realClassOne.chickenStock.stock.dto.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class TradeRequestDTO {
    private String stockCode;      // 종목 코드
    private Integer quantity;      // 거래 수량
    private Long price;            // 거래 희망 가격 (시장가 주문시 null 가능)
    private Boolean marketOrder;   // 시장가 주문 여부 (true: 시장가, false: 지정가)
}