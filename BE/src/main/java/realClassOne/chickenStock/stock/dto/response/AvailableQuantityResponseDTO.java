package realClassOne.chickenStock.stock.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class AvailableQuantityResponseDTO {
    private String shortCode;
    private int availableQuantity;
}
