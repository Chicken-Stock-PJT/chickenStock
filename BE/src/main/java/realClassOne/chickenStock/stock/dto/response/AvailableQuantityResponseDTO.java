package realClassOne.chickenStock.stock.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class AvailableQuantityResponseDTO {
    private Long stockDataId;
    private int availableQuantity;
}
