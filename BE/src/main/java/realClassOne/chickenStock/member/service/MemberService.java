package realClassOne.chickenStock.member.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.common.util.SecurityUtil;
import realClassOne.chickenStock.member.dto.response.MemberResponseDto;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import org.springframework.security.crypto.password.PasswordEncoder;
import realClassOne.chickenStock.member.dto.request.PasswordChangeRequestDTO;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

@Slf4j
@Service
@RequiredArgsConstructor
public class MemberService {

    private final MemberRepository memberRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtTokenProvider jwtTokenProvider;


    @Transactional(readOnly = true)
    public MemberResponseDto getCurrentUser() {
        String email = SecurityUtil.getCurrentUserEmail();

        Member member = memberRepository.findByEmail(email)
                .orElseThrow(() -> new CustomException(MemberErrorCode.ALREADY_REGISTERED_EMAIL));

        return MemberResponseDto.from(member);
    }
    @Transactional
    public void changePassword(String authorizationHeader, PasswordChangeRequestDTO dto) {

        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        if (!passwordEncoder.matches(dto.getCurrentPassword(), member.getPassword())) {
            throw new CustomException(MemberErrorCode.INVALID_PASSWORD);
        }

        if (!dto.getNewPassword().equals(dto.getCheckPassword())) {
            throw new CustomException(MemberErrorCode.PASSWORD_CONFIRM_MISMATCH);
        }

        String encodedNewPassword = passwordEncoder.encode(dto.getNewPassword());
        member.updatePassword(encodedNewPassword);
    }

}
