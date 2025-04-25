package realClassOne.chickenStock.auth.dto.request;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class NicknameCheckRequestDTO {

    private String nickname;

    public NicknameCheckRequestDTO(String nickname) {
        this.nickname = nickname;
    }
}
