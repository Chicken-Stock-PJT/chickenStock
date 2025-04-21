package realClassOne.chickenStock.member.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.entity.MemberRole;

import java.util.Set;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MemberResponseDto {
    private Long id;
    private String email;
    private String name;
    private String imageUrl;
    private String provider;
    private Set<MemberRole> roles;

    public static MemberResponseDto from(Member member) {
        return MemberResponseDto.builder()
                .id(member.getMemberId())
                .email(member.getEmail())
                .name(member.getName())
                .imageUrl(member.getProfileImage())
                .provider(member.getProvider())
                .roles(member.getRoles())
                .build();
    }
}
