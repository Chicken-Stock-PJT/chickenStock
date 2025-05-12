package realClassOne.chickenStock.auth.dto.response;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class EmailCheckResponseDTO {
    private boolean success;
    private String message;

    public static EmailCheckResponseDTO of(boolean success, String message) {
        return new EmailCheckResponseDTO(success, message);
    }
}
