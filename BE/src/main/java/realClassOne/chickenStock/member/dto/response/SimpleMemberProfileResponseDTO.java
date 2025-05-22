package realClassOne.chickenStock.member.dto.response;

import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class SimpleMemberProfileResponseDTO {
    private String nickname;          // 회원 닉네임
    private String totalAsset;        // 총 자산 (현금 + 주식가치)
    private String returnRate;        // 총 수익률
    private String isOauth;           // OAuth 계정 여부
    private String memberMoney;       // 순 현금 자산
    private String pendingOrderAmount; // 미체결 주문 금액
    private String stockValuation;    // 주식 평가 금액
}