package realClassOne.chickenStock.stock.dto.response;

public record InitializeMoneyResponseDTO(
        String status,
        String message,
        Long memberId
) {}
