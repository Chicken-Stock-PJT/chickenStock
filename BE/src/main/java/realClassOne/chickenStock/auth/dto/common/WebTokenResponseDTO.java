package realClassOne.chickenStock.auth.dto.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WebTokenResponseDTO {
    private String accessToken;
    private Long accessTokenExpiresIn;
}