package realClassOne.chickenStock.stock.trade.dto.response;

import lombok.Getter;

@Getter
public class OrderCancelResponseDTO {

    private String status;
    private String message;

    // 성공 응답 생성 팩토리 메소드
    public static OrderCancelResponseDTO success() {
        OrderCancelResponseDTO dto = new OrderCancelResponseDTO();
        dto.status = "success";
        dto.message = "주문이 성공적으로 취소되었습니다.";
        return dto;
    }

    // 실패 응답 생성 팩토리 메소드
    public static OrderCancelResponseDTO fail() {
        OrderCancelResponseDTO dto = new OrderCancelResponseDTO();
        dto.status = "error";
        dto.message = "주문 취소에 실패했습니다. 주문이 존재하지 않거나 이미 처리되었습니다.";
        return dto;
    }

    public static OrderCancelResponseDTO fail(String message) {
        OrderCancelResponseDTO dto = new OrderCancelResponseDTO();
        dto.status = "error";
        dto.message = message;
        return dto;
    }
}
