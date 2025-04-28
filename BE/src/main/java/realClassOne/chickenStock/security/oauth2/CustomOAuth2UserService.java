package realClassOne.chickenStock.security.oauth2;

import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.user.DefaultOAuth2User;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.entity.MemberRole;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.oauth2.member.OAuth2UserInfo;
import realClassOne.chickenStock.security.oauth2.member.OAuth2UserInfoFactory;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    private final MemberRepository memberRepository;

    @Override
    public OAuth2User loadUser(OAuth2UserRequest userRequest) throws OAuth2AuthenticationException {
        OAuth2User oAuth2User = super.loadUser(userRequest);

        try {
            return processOAuth2User(userRequest, oAuth2User);
        } catch (AuthenticationException ex) {
            throw ex;
        } catch (Exception ex) {
            throw new InternalAuthenticationServiceException(ex.getMessage(), ex.getCause());
        }
    }

    private OAuth2User processOAuth2User(OAuth2UserRequest userRequest, OAuth2User oAuth2User) {
        String provider = userRequest.getClientRegistration().getRegistrationId();
        OAuth2UserInfo oAuth2UserInfo = OAuth2UserInfoFactory.getOAuth2UserInfo(provider, oAuth2User.getAttributes());

        if (!StringUtils.hasText(oAuth2UserInfo.getEmail())) {
            throw new OAuth2AuthenticationException("Email not found from OAuth2 provider");
        }

        Optional<Member> memberOptional = memberRepository.findByEmail(oAuth2UserInfo.getEmail());
        Member member;

        if (memberOptional.isPresent()) {
            member = memberOptional.get();
            if (!member.getProvider().equals(provider) && !member.getProvider().equals("local")) {
                throw new OAuth2AuthenticationException(
                        "이미 다른 소셜 계정(" + member.getProvider() + ")으로 가입된 이메일입니다."
                );
            }
            member = updateExistingMember(member, oAuth2UserInfo);
        } else {
            member = registerNewMember(userRequest, oAuth2UserInfo);
        }

        Map<String, Object> attributes = new HashMap<>(oAuth2User.getAttributes());
        attributes.put("member_id", member.getMemberId());

        return new DefaultOAuth2User(
                Collections.singleton(new SimpleGrantedAuthority("ROLE_USER")),
                attributes,
                userRequest.getClientRegistration().getProviderDetails().getUserInfoEndpoint().getUserNameAttributeName()
        );
    }

    private Member registerNewMember(OAuth2UserRequest oAuth2UserRequest, OAuth2UserInfo oAuth2UserInfo) {
        Member member = Member.of(
                oAuth2UserInfo.getEmail(),
                null, // password는 소셜 로그인 시 필요 없으므로 null 처리
                oAuth2UserInfo.getName(),
                oAuth2UserInfo.getImageUrl(),
                oAuth2UserRequest.getClientRegistration().getRegistrationId(),
                oAuth2UserInfo.getId(),
                Collections.singleton(MemberRole.ROLE_USER)
        );
        return memberRepository.save(member);
    }

    private Member updateExistingMember(Member existingMember, OAuth2UserInfo oAuth2UserInfo) {
        existingMember.updateOAuth2Info(
                oAuth2UserInfo.getName(),
                oAuth2UserInfo.getImageUrl()
        );
        return memberRepository.save(existingMember);
    }
}