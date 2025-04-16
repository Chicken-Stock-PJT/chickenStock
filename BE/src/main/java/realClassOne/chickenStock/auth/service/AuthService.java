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
import realClassOne.chickenStock.auth.dto.request.ExchangeRequestDTO;
import realClassOne.chickenStock.auth.dto.request.LoginRequestDTO;
import realClassOne.chickenStock.auth.dto.request.SignupRequestDTO;
import realClassOne.chickenStock.auth.dto.response.SignupResponseDTO;
import realClassOne.chickenStock.auth.exception.AuthErrorCode;
import realClassOne.chickenStock.common.exception.CommonErrorCode;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.common.util.CookieUtils;
import realClassOne.chickenStock.config.security.JwtConfig;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.entity.MemberRole;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;

import java.util.Collections;

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
                .name(savedMember.getName())
                .build();
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
            CookieUtils.addCookie(response, "accessToken", tokenDto.getAccessToken(),
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
    public WebTokenResponseDTO refreshAccessTokenWeb(String refreshToken, String accessToken, HttpServletResponse response) {

        // 리프레시 토큰 검증
        if (!jwtTokenProvider.validateToken(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        // 만료된 액세스 토큰에서 memberId 추출
        Long memberId = jwtTokenProvider.getMemberIdFromToken(accessToken);
        System.out.println(memberId);
        if (memberId == null) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        TokenDto tokenDto = jwtTokenProvider.generateToken(member);

        CookieUtils.addCookie(response, "accessToken", tokenDto.getAccessToken(),
                (int) (jwtConfig.getJwtAccessExpirationMs() / 1000));
        CookieUtils.addCookie(response, "refreshToken", tokenDto.getRefreshToken(),
                (int) (jwtConfig.getJwtRefreshExpirationMs() / 1000));

        // 웹용 응답 생성
        WebTokenResponseDTO webTokenResponseDTO = jwtTokenProvider.generateAccessToken(member);
        return webTokenResponseDTO;
    }

    @Transactional
    public TokenDto refreshAccessTokenMobile(String refreshToken) {

        if (!jwtTokenProvider.validateToken(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        Long memberId = jwtTokenProvider.getMemberIdFromToken(refreshToken);
        if (memberId == null) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // 새 액세스 토큰만 발급 (리프레시 토큰은 재사용)
        WebTokenResponseDTO webTokenResponseDTO = jwtTokenProvider.generateAccessToken(member);

        // 응답 DTO 생성
        TokenDto tokenDto = new TokenDto();
        tokenDto.setAccessToken(webTokenResponseDTO.getAccessToken());
        tokenDto.setRefreshToken(refreshToken); // 기존 리프레시 토큰 유지

        return tokenDto;
    }

    // 완전히 새로운 토큰 세트 발급(웹)
    @Transactional
    public WebTokenResponseDTO refreshAllTokensWeb(String refreshToken, HttpServletResponse response) {

        System.out.println("1");
        // 리프레시 토큰 검증
        if (!jwtTokenProvider.validateToken(refreshToken)) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }

        System.out.println("2");

        Long memberId = jwtTokenProvider.getMemberIdFromToken(refreshToken);
        if (memberId == null) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }
        System.out.println("3");
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));
        System.out.println("4");
        // 기존 리프레시 토큰을 블랙리스트에 추가
        redisTokenBlacklistService.addToBlacklist(refreshToken, jwtTokenProvider.getExpirationTime(refreshToken));
        System.out.println("5");
        TokenDto tokenDto = jwtTokenProvider.generateToken(member);
        System.out.println("6");
        CookieUtils.addCookie(response, "accessToken", tokenDto.getAccessToken(),
                (int) (jwtConfig.getJwtAccessExpirationMs() / 1000));
        CookieUtils.addCookie(response, "refreshToken", tokenDto.getRefreshToken(),
                (int) (jwtConfig.getJwtRefreshExpirationMs() / 1000));

        WebTokenResponseDTO webTokenResponseDTO = jwtTokenProvider.generateAccessToken(member);
        return webTokenResponseDTO;
    }

    // 완전히 새로운 토큰 세트 발급(모바일)
    @Transactional
    public TokenDto refreshAllTokensMobile(String refreshToken) {

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

        redisTokenBlacklistService.addToBlacklist(refreshToken, jwtTokenProvider.getExpirationTime(refreshToken));

        return jwtTokenProvider.generateToken(member);
    }

    // 로그아웃
    @Transactional
    public void logout(String authorizationHeader) {
        if (authorizationHeader == null || authorizationHeader.isEmpty()) {
            throw new CustomException(CommonErrorCode.INVALID_INPUT_VALUE);
        }

        String token = authorizationHeader.substring(7);

        try {
            // 토큰을 블랙리스트에 추가
            jwtTokenProvider.addToBlacklist(token);

            // 토큰에서 직접 memberId 추출
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);
            if (memberId != null) {
                memberRepository.findById(memberId)
                        .ifPresent(member -> {
                            member.clearRefreshToken();
                            memberRepository.save(member);
                            System.out.println("5");
                        });
            }

            // 보안 컨텍스트 초기화
            SecurityContextHolder.clearContext();

        } catch (Exception e) {
            throw new CustomException(AuthErrorCode.INVALID_TOKEN);
        }
    }
}