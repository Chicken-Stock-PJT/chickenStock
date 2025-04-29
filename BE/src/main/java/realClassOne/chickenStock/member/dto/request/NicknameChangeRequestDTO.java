package realClassOne.chickenStock.member.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.Getter;

@Getter
public class NicknameChangeRequestDTO {

    @NotBlank(message = "닉네임은 비어 있을 수 없습니다.")
    @Size(max = 10, message = "닉네임은 최대 10자까지 가능합니다.")
    @Pattern(regexp = "^[a-zA-Z가-힣0-9]+$", message = "닉네임은 한글, 영문, 숫자만 사용할 수 있습니다.")
    private final String nickname;

    private NicknameChangeRequestDTO(String nickname) {
        this.nickname = nickname;
    }

    public static NicknameChangeRequestDTO of(String nickname) {
        return new NicknameChangeRequestDTO(nickname);
    }
}