package realClassOne.chickenStock.member.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor(staticName = "of")
public class SimpleMemberProfileResponseDTO {
    private String nickname;
    private String totalAsset;
    private String returnRate;
    private String isOauth;
    private String memberMoney;
}
