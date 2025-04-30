package realClassOne.chickenStock.auth.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import realClassOne.chickenStock.common.exception.ErrorCode;

@Getter
@RequiredArgsConstructor
public enum AuthErrorCode implements ErrorCode {

    INVALID_CREDENTIALS(HttpStatus.UNAUTHORIZED, "AUTH-E001", "아이디 또는 비밀번호가 일치하지 않습니다"),
    EXPIRED_TOKEN(HttpStatus.UNAUTHORIZED, "AUTH-E002", "만료된 토큰입니다"),
    INVALID_TOKEN(HttpStatus.UNAUTHORIZED, "AUTH-E003", "유효하지 않은 토큰입니다"),
    ACCESS_DENIED(HttpStatus.FORBIDDEN, "AUTH-E004", "접근 권한이 없습니다"),
    INVALID_PLATFORM(HttpStatus.BAD_REQUEST, "AUTH-E005", "유효하지 않은 플랫폼입니다"),
    REDIS_OPERATION_FAILED(HttpStatus.FORBIDDEN, "AUTH-E006", "레디스 값이 비었습니다g"),
    VERIFICATION_NOT_COMPLETED(HttpStatus.BAD_REQUEST, "AUTH-E007", "이메일 인증이 완료되지 않았습니다."),
    INVALID_NICKNAME_FORMAT(HttpStatus.BAD_REQUEST, "AUTH-E008", "닉네임은 최대 10자, 한글/영어/숫자만 사용할 수 있습니다.");

    private final HttpStatus httpStatus;
    private final String code;
    private final String message;
}