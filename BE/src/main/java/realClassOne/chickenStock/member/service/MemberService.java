package realClassOne.chickenStock.member.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.common.util.SecurityUtil;
import realClassOne.chickenStock.member.dto.response.MemberResponseDto;
import realClassOne.chickenStock.member.dto.response.NicknameChangeResponseDTO;
import realClassOne.chickenStock.member.dto.response.PasswordChangeResponseDTO;
import realClassOne.chickenStock.member.dto.response.SimpleMemberProfileResponseDTO;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import org.springframework.security.crypto.password.PasswordEncoder;
import realClassOne.chickenStock.member.dto.request.PasswordChangeRequestDTO;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class MemberService {

    private final MemberRepository memberRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtTokenProvider jwtTokenProvider;
    private final HoldingPositionRepository holdingPositionRepository;



    @Transactional(readOnly = true)
    public MemberResponseDto getCurrentUser() {
        String email = SecurityUtil.getCurrentUserEmail();

        Member member = memberRepository.findByEmail(email)
                .orElseThrow(() -> new CustomException(MemberErrorCode.ALREADY_REGISTERED_EMAIL));

        return MemberResponseDto.from(member);
    }
    @Transactional
    public PasswordChangeResponseDTO changePassword(String authorizationHeader, PasswordChangeRequestDTO dto) {

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

        return PasswordChangeResponseDTO.of("비밀번호가 성공적으로 변경되었습니다.");
    }

    @Transactional
    public NicknameChangeResponseDTO changeNickname(String authorizationHeader, String newNickname) {
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // 사용자 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // 닉네임 중복 여부 확인
        if (memberRepository.existsByNickname(newNickname)) {
            throw new CustomException(MemberErrorCode.DUPLICATE_NICKNAME);
        }

        // 닉네임 변경
        member.changeNickname(newNickname);

        return NicknameChangeResponseDTO.of("닉네임이 성공적으로 변경되었습니다.");
    }


    public SimpleMemberProfileResponseDTO getSimpleProfile(String authorizationHeader) {
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        List<HoldingPosition> holdingPositions = holdingPositionRepository.findAllByMember_MemberId(memberId);

        // 수익률 평균 계산
        double averageReturnRate = 0.0;
        if (holdingPositions != null && !holdingPositions.isEmpty()) {
            averageReturnRate = holdingPositions.stream()
                    .mapToDouble(HoldingPosition::getReturnRate)
                    .average()
                    .orElse(0.0);
        }

        String nickname = member.getNickname();
        String memberMoney = member.getMemberMoney() != null ? member.getMemberMoney().toString() : "0";
        String returnRate = String.valueOf(averageReturnRate);
        String isOauth = !"local".equals(member.getProvider()) ? "true" : "false";

        return SimpleMemberProfileResponseDTO.of(nickname, memberMoney, returnRate, isOauth);
    }

}
