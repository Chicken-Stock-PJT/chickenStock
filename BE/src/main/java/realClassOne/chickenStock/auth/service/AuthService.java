package realClassOne.chickenStock.auth.service;

import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.auth.dto.common.TokenDto;
import realClassOne.chickenStock.auth.dto.common.WebTokenResponseDTO;
import realClassOne.chickenStock.auth.dto.request.*;
import realClassOne.chickenStock.auth.dto.response.NicknameCheckResponseDTO;
import realClassOne.chickenStock.auth.dto.response.PasswordResetResponseDTO;
import realClassOne.chickenStock.auth.dto.response.SignupResponseDTO;
import realClassOne.chickenStock.auth.exception.AuthErrorCode;
import realClassOne.chickenStock.auth.repository.VerificationCodeRepository;
import realClassOne.chickenStock.common.exception.CommonErrorCode;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.common.util.CookieUtils;
import realClassOne.chickenStock.config.security.JwtConfig;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.entity.MemberRole;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.auth.dto.response.EmailCheckResponseDTO;
import realClassOne.chickenStock.auth.service.EmailService;


import java.util.Collections;
import java.util.UUID;
import java.util.regex.Pattern;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthService {

    private final MemberRepository memberRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtTokenProvider jwtTokenProvider;
    private final JwtConfig jwtConfig;
    private final RedisTokenBlacklistService redisTokenBlacklistService;
    private final AuthenticationManagerBuilder authenticationManagerBuilder;
    private final EmailService emailService;
    private final VerificationCodeRepository verificationCodeRepository;

    // 소셜 토큰 반환
    public Object exchangeToken(ExchangeRequestDTO request, HttpServletResponse response) {

        String oneTimeCode = request.getOneTimeCode();
        Long memberId = Long.parseLong(redisTokenBlacklistService.getTokenFromOneTimeCode(oneTimeCode));

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        return handleTokenByPlatform(member, request.getPlatform(), response);
    }

    // 일반 회원가입
    @Transactional
    public SignupResponseDTO signup(SignupRequestDTO signupRequestDTO) {
        if (memberRepository.existsByEmail(signupRequestDTO.getEmail())) {
            throw new CustomException(MemberErrorCode.ALREADY_REGISTERED_EMAIL);
        }

        Member member = Member.of(
                signupRequestDTO.getEmail(),
                passwordEncoder.encode(signupRequestDTO.getPassword()),
                signupRequestDTO.getNickname(),
                signupRequestDTO.getName(),
                null, // image
                "local",
                null, // providerId
                Collections.singleton(MemberRole.ROLE_USER));

        Member savedMember = memberRepository.save(member);
        log.info("회원가입 완료: {}", savedMember.getEmail());

        return SignupResponseDTO.builder()
                .id(savedMember.getMemberId())
                .email(savedMember.getEmail())
                .nickname(savedMember.getNickname())
                .name(savedMember.getName())
                .build();
    }

    // 이메일 중복 확인
    public EmailCheckResponseDTO checkEmailDuplicateAndRespond(String email) {
        if (memberRepository.existsByEmail(email)) {
            return EmailCheckResponseDTO.of(false, "이미 가입된 이메일입니다.");
        }
        return EmailCheckResponseDTO.of(true, "사용 가능한 이메일입니다.");
    }

    // 로그인
    @Transactional
    public Object login(LoginRequestDTO loginRequestDTO, HttpServletResponse response) {

        try {

            UsernamePasswordAuthenticationToken authenticationToken =
                    new UsernamePasswordAuthenticationToken(loginRequestDTO.getEmail(), loginRequestDTO.getPassword());

            // 인증 수행
            Authentication authentication = authenticationManagerBuilder.getObject().authenticate(authenticationToken);
            SecurityContextHolder.getContext().setAuthentication(authentication);


            Member member = memberRepository.findByEmail(loginRequestDTO.getEmail())
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            return handleTokenByPlatform(member, loginRequestDTO.getPlatform(), response);

        } catch (Exception e) {
            log.error("로그인 실패: {}", e.getMessage());
            throw new CustomException(AuthErrorCode.ACCESS_DENIED);
        }
    }

    private Object handleTokenByPlatform(Member member, String platform, HttpServletResponse response) {

        TokenDto tokenDto = jwtTokenProvider.generateToken(member);

        if ("mobile".equals(platform)) {
            return tokenDto;
        } else if ("web".equals(platform)) {
            // 쿠키에 토큰 추가
            CookieUtils.addCookie(response, "Authorization", tokenDto.getAccessToken(),
                    (int) (jwtConfig.getJwtAccessExpirationMs() / 1000));
            CookieUtils.addCookie(response, "refreshToken", tokenDto.getRefreshToken(),
                    (int) (jwtConfig.getJwtRefreshExpirationMs() / 1000));

            // 웹용 응답 생성
            WebTokenResponseDTO webTokenResponseDTO = jwtTokenProvider.generateAccessToken(member);
            return webTokenResponseDTO;
        }
        // 유효하지 않은 플랫폼
        else {
            throw new CustomException(AuthErrorCode.INVALID_PLATFORM);
        }
    }

    @Transactional
    public WebTokenResponseDTO refreshAccessTokenWeb(String refreshToken, HttpServletResponse response) {

        // 리프레시 토큰 검증
        if (!jwtTokenProvider.validateToken(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        // 만료된 액세스 토큰에서 memberId 추출
        Long memberId = jwtTokenProvider.getMemberIdFromToken(refreshToken);
        if (memberId == null) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // 저장된 리프레시 토큰과 비교
        if (member.getRefreshToken() == null || !member.getRefreshToken().equals(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        // 새 액세스 토큰만 발급 (Member 엔티티 변경 없음)
        WebTokenResponseDTO webTokenResponseDTO = jwtTokenProvider.generateAccessToken(member);

        // 쿠키에 새 액세스 토큰만 추가
        CookieUtils.addCookie(response, "Authorization", webTokenResponseDTO.getAccessToken(),
                (int) (jwtConfig.getJwtAccessExpirationMs() / 1000));

        return webTokenResponseDTO;
    }

    @Transactional
    public TokenDto refreshAccessTokenMobile(OnlyRefreshTokenRequestDTO request) {

        String refreshToken = request.getRefreshToken();

        if (!jwtTokenProvider.validateToken(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        Long memberId = jwtTokenProvider.getMemberIdFromToken(refreshToken);
        if (memberId == null) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        if (member.getRefreshToken() == null || !member.getRefreshToken().equals(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        // 새 액세스 토큰만 발급 (리프레시 토큰은 재사용)
        WebTokenResponseDTO webTokenResponseDTO = jwtTokenProvider.generateAccessToken(member);

        // 응답 DTO 생성
        TokenDto tokenDto = new TokenDto();
        tokenDto.setAccessToken(webTokenResponseDTO.getAccessToken());
        tokenDto.setRefreshToken(refreshToken); // 기존 리프레시 토큰 유지
        tokenDto.setAccessTokenExpiresIn(webTokenResponseDTO.getAccessTokenExpiresIn());

        return tokenDto;
    }

    // 완전히 새로운 토큰 세트 발급(웹)
    @Transactional
    public WebTokenResponseDTO refreshAllTokensWeb(String refreshToken, String accessToken, HttpServletResponse response) {

        // 리프레시 토큰 검증
        if (!jwtTokenProvider.validateToken(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        Long memberId = jwtTokenProvider.getMemberIdFromToken(refreshToken);
        if (memberId == null) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        if (member.getRefreshToken() == null || !member.getRefreshToken().equals(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        // 기존 토큰들 블랙리스트에 추가
        jwtTokenProvider.addToBlacklist(accessToken);

        redisTokenBlacklistService.addToBlacklist(refreshToken, jwtTokenProvider.getExpirationTime(refreshToken));

        TokenDto tokenDto = jwtTokenProvider.generateToken(member);

        CookieUtils.addCookie(response, "Authorization", tokenDto.getAccessToken(),
                (int) (jwtConfig.getJwtAccessExpirationMs() / 1000));
        CookieUtils.addCookie(response, "refreshToken", tokenDto.getRefreshToken(),
                (int) (jwtConfig.getJwtRefreshExpirationMs() / 1000));

        // 웹용 응답 생성
        WebTokenResponseDTO webTokenResponseDTO = WebTokenResponseDTO.builder()
                .accessToken(tokenDto.getAccessToken())
                .accessTokenExpiresIn(tokenDto.getAccessTokenExpiresIn())
                .build();

        return webTokenResponseDTO;
    }

    // 완전히 새로운 토큰 세트 발급(모바일)
    @Transactional
    public TokenDto refreshAllTokensMobile(RefreshTokenRequestDTO request) {

        String accessToken = request.getAccessToken();
        String refreshToken = request.getRefreshToken();

        // 리프레시 토큰 검증
        if (!jwtTokenProvider.validateToken(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        Long memberId = jwtTokenProvider.getMemberIdFromToken(refreshToken);
        if (memberId == null) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        if (member.getRefreshToken() == null || !member.getRefreshToken().equals(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        jwtTokenProvider.addToBlacklist(accessToken);

        redisTokenBlacklistService.addToBlacklist(refreshToken, jwtTokenProvider.getExpirationTime(refreshToken));

        return jwtTokenProvider.generateToken(member);
    }

    // 로그아웃
    @Transactional
    public void logout(String authorizationHeader) {

        if (authorizationHeader == null || authorizationHeader.isEmpty()) {
            throw new CustomException(CommonErrorCode.INVALID_INPUT_VALUE);
        }

        try {
            // Bearer 접두사 제거
            String token = authorizationHeader;
            if (authorizationHeader.startsWith("Bearer ")) {
                token = authorizationHeader.substring(7);
            }

            // 토큰에서 memberId 추출
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
            if (memberId == null) {
                throw new CustomException(AuthErrorCode.INVALID_TOKEN);
            }

            // 사용자 정보 조회
            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 액세스 토큰 블랙리스트에 추가
            jwtTokenProvider.addToBlacklist(token);

            // 리프레시 토큰이 있는 경우 함께 블랙리스트에 추가
            if (member.getRefreshToken() != null) {
                redisTokenBlacklistService.addToBlacklist(member.getRefreshToken(),
                        jwtTokenProvider.getExpirationTime(member.getRefreshToken()));

                // 멤버의 리프레시 토큰 초기화
                member.clearRefreshToken();
                memberRepository.save(member);
            }

            // 보안 컨텍스트 초기화
            SecurityContextHolder.clearContext();

        } catch (Exception e) {
            log.error("로그아웃 처리 중 오류 발생: {}", e.getMessage());
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }
    }

    // 인증 완료된 이메일 임시 비밀번호 전송
    // AuthService.java 내부
    public PasswordResetResponseDTO resetPasswordAfterVerification(String email) {
        // 1. 인증 완료 여부 확인 (Redis)
        if (!verificationCodeRepository.isVerified(email)) {
            throw new CustomException(AuthErrorCode.VERIFICATION_NOT_COMPLETED);
        }

        // 2. 사용자 조회
        Member member = memberRepository.findByEmail(email)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // 3. 임시 비밀번호 생성
        String tempPassword = UUID.randomUUID().toString().substring(0, 10);

        // 4. 비밀번호 암호화 & 저장
        member.updatePassword(passwordEncoder.encode(tempPassword));
        memberRepository.save(member);

        // 5. 이메일로 임시 비밀번호 전송 (💡 EmailService 사용!)
        emailService.sendTemporaryPassword(email, tempPassword);

        // 6. 인증 상태 제거
        verificationCodeRepository.removeVerified(email);

        return PasswordResetResponseDTO.of("임시 비밀번호가 이메일로 전송되었습니다.");
    }

    // 한글, 영어, 숫자만 가능하고, 최대 10자
    private final Pattern nicknamePattern = Pattern.compile("^[a-zA-Z0-9가-힣]{1,10}$");

    public NicknameCheckResponseDTO checkNickname(String nickname) {
        if (!nicknamePattern.matcher(nickname).matches()) {
            throw new CustomException(AuthErrorCode.INVALID_NICKNAME_FORMAT);
        }

        boolean isDuplicate = memberRepository.existsByNickname(nickname);
        String message = isDuplicate ? "이미 사용 중인 닉네임입니다." : "사용 가능한 닉네임입니다.";
        return NicknameCheckResponseDTO.of(isDuplicate, message);
    }


}