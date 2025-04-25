package realClassOne.chickenStock.auth.dto.request;

import jakarta.validation.constraints.NotBlank;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class NicknameCheckRequestDTO {

    @NotBlank(message = "닉네임은 필수입니다.")
    private String nickname;

    public NicknameCheckRequestDTO(String nickname) {
        this.nickname = nickname;
    }
}
