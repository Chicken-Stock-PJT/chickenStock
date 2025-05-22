package realClassOne.chickenStock.auth.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(staticName = "of")
public class NicknameCheckResponseDTO {
    private boolean isDuplicate;
    private String message;
}
