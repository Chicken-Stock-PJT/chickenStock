package realClassOne.chickenStock.common.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;

@Getter
@RequiredArgsConstructor
public enum CommonErrorCode implements ErrorCode {
    INTERNAL_SERVER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "SYSTEM-E001", "서버 내부 오류가 발생했습니다"),
    INVALID_INPUT_VALUE(HttpStatus.BAD_REQUEST, "SYSTEM-E002", "잘못된 입력값입니다"),
    METHOD_NOT_ALLOWED(HttpStatus.METHOD_NOT_ALLOWED, "SYSTEM-E003", "지원하지 않는 HTTP 메서드입니다"),
    RESOURCE_NOT_FOUND(HttpStatus.NOT_FOUND, "SYSTEM-E004", "요청한 리소스를 찾을 수 없습니다"),
    INVALID_PARAMETER(HttpStatus.BAD_REQUEST, "SYSTEM-E005", "유효하지 않은 요청 파라미터입니다");

    private final HttpStatus httpStatus;
    private final String code;
    private final String message;
}