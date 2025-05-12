package realClassOne.chickenStock.security.excpetion;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import realClassOne.chickenStock.common.exception.ErrorCode;

@Getter
@RequiredArgsConstructor
public enum SecurityErrorCode implements ErrorCode {

    INVALID_JWT_SIGNATURE("SEC-001", HttpStatus.UNAUTHORIZED, "잘못된 JWT 서명입니다."),
    EXPIRED_JWT_TOKEN("SEC-002", HttpStatus.UNAUTHORIZED, "만료된 JWT 토큰입니다."),
    UNSUPPORTED_JWT_TOKEN("SEC-003", HttpStatus.UNAUTHORIZED, "지원되지 않는 JWT 토큰입니다."),
    INVALID_JWT_TOKEN("SEC-004", HttpStatus.UNAUTHORIZED, "JWT 토큰이 잘못되었습니다."),
    MISSING_AUTHORITY("SEC-005", HttpStatus.UNAUTHORIZED, "권한 정보가 없는 토큰입니다."),
    BLACKLISTED_JWT_TOKEN("SEC-006", HttpStatus.UNAUTHORIZED, "블랙리스트에 등록된 JWT 토큰입니다.");

    private final String code;
    private final HttpStatus httpStatus;
    private final String message;
}
