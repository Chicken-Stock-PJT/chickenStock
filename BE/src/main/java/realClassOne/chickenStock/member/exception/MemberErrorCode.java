package realClassOne.chickenStock.member.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import realClassOne.chickenStock.common.exception.ErrorCode;

@Getter
@RequiredArgsConstructor
public enum MemberErrorCode implements ErrorCode {
    ALREADY_REGISTERED_EMAIL(HttpStatus.BAD_REQUEST, "MEMBER-E001", "이미 가입된 이메일입니다"),
    MEMBER_NOT_FOUND(HttpStatus.NOT_FOUND, "MEMBER-E002", "회원을 찾을 수 없습니다"),
    INVALID_PASSWORD(HttpStatus.BAD_REQUEST, "MEMBER-E003", "비밀번호가 일치하지 않습니다"),
    EMAIL_VERIFICATION_FAILED(HttpStatus.BAD_REQUEST, "MEMBER-E004", "이메일 인증에 실패했습니다");

    private final HttpStatus httpStatus;
    private final String code;
    private final String message;
}