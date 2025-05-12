package realClassOne.chickenStock.auth.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(staticName = "of")
public class PasswordResetResponseDTO {
    private String message;
}
