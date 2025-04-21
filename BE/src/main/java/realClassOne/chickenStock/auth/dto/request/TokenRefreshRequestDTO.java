package realClassOne.chickenStock.auth.dto.request;

import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TokenRefreshRequestDTO {

    @NotBlank(message = "리프레시 토큰은 필수 입력 값입니다")
    private String refreshToken;
}
