package realClassOne.chickenStock.auth.dto.response;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class EmailVerifyResponseDTO {
    private boolean success;
    private String message;

    public static EmailVerifyResponseDTO of(boolean success, String message) {
        return new EmailVerifyResponseDTO(success, message);
    }
}
