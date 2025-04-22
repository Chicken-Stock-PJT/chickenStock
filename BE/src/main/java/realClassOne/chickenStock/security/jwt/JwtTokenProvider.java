package realClassOne.chickenStock.security.jwt;

import io.jsonwebtoken.*;
import io.jsonwebtoken.security.Keys;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.auth.dto.common.TokenDto;
import realClassOne.chickenStock.auth.dto.common.WebTokenResponseDTO;
import realClassOne.chickenStock.auth.service.RedisTokenBlacklistService;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.config.security.JwtConfig;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.excpetion.SecurityErrorCode;

import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class JwtTokenProvider {

    private final JwtConfig jwtConfig;
    private final MemberRepository memberRepository;
    private final RedisTokenBlacklistService redisTokenBlacklistService;

    private Key getSigningKey() {
        // 시크릿 키를 바이트 배열로 변환
        byte[] keyBytes = jwtConfig.getJwtSecret().getBytes(StandardCharsets.UTF_8);

        // 키 길이가 충분한지 확인 (HS512의 경우 최소 64바이트)
        if (keyBytes.length < 64) {
            throw new IllegalArgumentException("JWT 시크릿 키가 너무 짧습니다. HS512 알고리즘에는 최소 64바이트가 필요합니다.");
        }

        // 또는 기존 시크릿을 사용하려면:
         return Keys.hmacShaKeyFor(keyBytes);
    }

    // 1회용 토큰 코드 생성
    public String generateOneTimeTokenCode(Long memberId) {
        String oneTimeCode = UUID.randomUUID().toString();

        redisTokenBlacklistService.storeOneTimeCode(oneTimeCode, memberId);

        return oneTimeCode;
    }

    // 1회용 코드로 실제 토큰 조회
    public String getTokenFromOneTimeCode(String oneTimeCode) {
        return redisTokenBlacklistService.getTokenFromOneTimeCode(oneTimeCode);
    }

    // 전체 토큰 생성
    public TokenDto generateToken(Member member) {

        Map<String, Object> claims = new HashMap<>();
        claims.put("memberPoint", member.getMemberMoney() != null ? member.getMemberMoney() : 0L);
        claims.put("memberId", member.getMemberId());
        claims.put("nickname", member.getNickname() == null ? "" : member.getNickname());

        long now = (new Date()).getTime();
        Date accessTokenExpiresIn = new Date(now + jwtConfig.getJwtAccessExpirationMs());
        Date refreshTokenExpiresIn = new Date(now + jwtConfig.getJwtRefreshExpirationMs());

        // Access Token 생성 (JJWT 0.12.x 버전 사용)
        String accessToken = Jwts.builder()
                .subject(member.getMemberId().toString())
                .claims(claims)
                .issuedAt(new Date())
                .expiration(accessTokenExpiresIn)
                .signWith(getSigningKey())
                .compact();

        // Refresh Token 생성
        String refreshToken = Jwts.builder()
                .subject(member.getMemberId().toString())
                .issuedAt(new Date())
                .expiration(refreshTokenExpiresIn)
                .signWith(getSigningKey())
                .compact();

        LocalDateTime expiryDate = Instant.ofEpochMilli(refreshTokenExpiresIn.getTime())
                .atZone(ZoneId.systemDefault())
                .toLocalDateTime();

        member.updateRefreshToken(refreshToken, expiryDate);
        memberRepository.save(member);

        return TokenDto.builder()
                .accessToken(accessToken)
                .refreshToken(refreshToken)
                .accessTokenExpiresIn(accessTokenExpiresIn.getTime())
                .build();
    }

    // 엑세스 토큰만 생성
    public WebTokenResponseDTO generateAccessToken(Member member) {
        // 액세스 토큰에 담을 클레임 설정
        Map<String, Object> claims = new HashMap<>();
        claims.put("memberPoint", member.getMemberMoney() != null ? member.getMemberMoney() : 0L);
        claims.put("memberId", member.getMemberId());
        claims.put("nickname", member.getNickname() != null ? member.getNickname() : "");

        long now = (new Date()).getTime();
        Date accessTokenExpiresIn = new Date(now + jwtConfig.getJwtAccessExpirationMs());

        String accessToken = Jwts.builder()
                .subject(member.getMemberId().toString())
                .claims(claims)
                .issuedAt(new Date())
                .expiration(accessTokenExpiresIn)
                .signWith(getSigningKey())
                .compact();

        return WebTokenResponseDTO.builder()
                .accessToken(accessToken)
                .accessTokenExpiresIn(accessTokenExpiresIn.getTime())
                .build();
    }

    // 토큰 인증 정보 추출
    public Authentication getAuthentication(String token) {
        // 토큰 복호화
        Claims claims = parseClaims(token);

        Collection<? extends GrantedAuthority> authorities;

        // auth 클레임이 있으면 사용, 없으면 기본 권한 설정
        if (claims.get("auth") != null) {
            authorities = Arrays.stream(claims.get("auth").toString().split(","))
                    .map(SimpleGrantedAuthority::new)
                    .collect(Collectors.toList());
        } else {
            // 기본 권한 설정 (예: ROLE_USER)
            authorities = Collections.singletonList(new SimpleGrantedAuthority("ROLE_USER"));
        }

        // UserDetails 객체를 만들어서 Authentication 리턴
        UserDetails principal = new User(claims.getSubject(), "", authorities);
        return new UsernamePasswordAuthenticationToken(principal, "", authorities);
    }

    // 리프레쉬 토큰에서 인증 정보 추출
    public Authentication getAuthenticationFromRefreshToken(String token) {
        // 토큰 복호화
        Claims claims = parseClaims(token);

        // DB에서 사용자 조회
        Member member = memberRepository.findByEmail(claims.getSubject())
                .orElseThrow(() -> new CustomException(SecurityErrorCode.INVALID_JWT_TOKEN));

        // 저장된 refreshToken과 일치하는지 확인
        if (member.getRefreshToken() == null || !member.getRefreshToken().equals(token)) {
            throw new CustomException(SecurityErrorCode.INVALID_JWT_TOKEN);
        }

        // UserDetails 객체를 만들어서 Authentication 리턴
        UserDetails principal = new User(claims.getSubject(), "", member.getRoles().stream()
                .map(role -> new SimpleGrantedAuthority(role.name()))
                .collect(Collectors.toList()));

        return new UsernamePasswordAuthenticationToken(principal, "", principal.getAuthorities());
    }

    // 토큰 유효성 검증
    public boolean validateToken(String token) {
        try {
            // 토큰이 블랙리스트에 있는지 확인
            if (redisTokenBlacklistService.isBlacklisted(token)) {
                throw new CustomException(SecurityErrorCode.INVALID_JWT_TOKEN);
            }
            Jwts.parser()
                    .setSigningKey(getSigningKey())
                    .build()
                    .parseClaimsJws(token);
            return true;
        } catch (SecurityException | MalformedJwtException e) {
            throw new CustomException(SecurityErrorCode.INVALID_JWT_SIGNATURE);
        } catch (ExpiredJwtException e) {
            throw new CustomException(SecurityErrorCode.EXPIRED_JWT_TOKEN);
        } catch (UnsupportedJwtException e) {
            throw new CustomException(SecurityErrorCode.UNSUPPORTED_JWT_TOKEN);
        } catch (IllegalArgumentException e) {
            throw new CustomException(SecurityErrorCode.INVALID_JWT_TOKEN);
        }
    }

    public void addToBlacklist(String token) {
        try {
            Claims claims = parseClaims(token);
            Date expiryDate = claims.getExpiration();

            // 만료 시간 계산 (현재 시간부터 토큰 만료시간까지)
            long timeToLiveMillis = Math.max(0, expiryDate.getTime() - System.currentTimeMillis());

            // Redis에 토큰 추가
            redisTokenBlacklistService.addToBlacklist(token, timeToLiveMillis);

        } catch (ExpiredJwtException e) {
            // 이미 만료된 토큰은 블랙리스트에 추가할 필요 없음
        }
    }

    // 클레임 파싱
    private Claims parseClaims(String token) {
        try {
            return Jwts.parser()
                    .setSigningKey(getSigningKey())
                    .build()
                    .parseClaimsJws(token)
                    .getBody();
        } catch (ExpiredJwtException e) {
            // 만료된 토큰이어도 일단 클레임 정보는 꺼내서 리턴 가능하도록.
            return e.getClaims();
        }
    }

    public Long getMemberIdFromToken(String token) {
        try {
            Claims claims = Jwts.parser()
                    .setSigningKey(getSigningKey())
                    .build()
                    .parseSignedClaims(token)
                    .getPayload();

            // memberId 대신 subject 사용
            return Long.parseLong(claims.getSubject());
        } catch (ExpiredJwtException e) {
            // 만료된 토큰인 경우에도 subject 값을 추출
            return Long.parseLong(e.getClaims().getSubject());
        } catch (Exception e) {
            return null;
        }
    }

    public long getExpirationTime(String token) {
        try {
            Claims claims = Jwts.parser()
                    .setSigningKey(getSigningKey())
                    .build()
                    .parseClaimsJws(token)
                    .getBody();

            Date expiration = claims.getExpiration();
            return (expiration.getTime() - new Date().getTime()) / 1000; // 초 단위로 반환
        } catch (Exception e) {
            return 0;
        }
    }

    public String resolveToken(String authorizationHeader) {
        if (authorizationHeader != null && authorizationHeader.startsWith("Bearer ")) {
            return authorizationHeader.substring(7);
        }
        return authorizationHeader;
    }

}
