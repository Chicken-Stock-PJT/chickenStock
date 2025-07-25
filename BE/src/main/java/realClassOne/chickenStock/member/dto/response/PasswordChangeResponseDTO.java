package realClassOne.chickenStock.member.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class PasswordChangeResponseDTO {
    private String message;

    public static PasswordChangeResponseDTO of(String message) {
        return new PasswordChangeResponseDTO(message);
    }
}
