package realClassOne.chickenStock.auth.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LoginResponseDTO {
    private Long memberId;
    private String email;
    private String memberPoint;
    private String name;
    private String nickname;
    private String imageUrl;
    private String accessToken;
    private String refreshToken;
    private long accessTokenExpiresIn;
}
