package realClassOne.chickenStock.common.exception;

import org.springframework.beans.TypeMismatchException;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.validation.BindException;
import org.springframework.validation.FieldError;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.ServletWebRequest;
import org.springframework.web.context.request.WebRequest;
import realClassOne.chickenStock.auth.exception.AuthErrorCode;

/*
 * 애플리케이션 전체에서 발생하는 예외를 처리하는 전역 예외 처리기
 * @RestControllerAdvice 어노테이션을 통해 모든 컨트롤러에 적용
 * Exception에서 -> SRP / OCP / ISP / DIP 반영하도록 코드 변경함.
 */
@RestControllerAdvice
public class GlobalExceptionHandler {

    //도메인별 ErrorCode를 포함해서 CustomException 처리
    @ExceptionHandler(CustomException.class)
    public ResponseEntity<ErrorResponse> handleCustomException(CustomException ex, WebRequest request) {
        String path = ((ServletWebRequest) request).getRequest().getRequestURI();
        ErrorResponse errorResponse = ErrorResponse.of(ex.getErrorCode(), ex.getMessage(), path);
        return new ResponseEntity<>(errorResponse, ex.getErrorCode().getHttpStatus());
    }

    @ExceptionHandler(AccessDeniedException.class)
    public ResponseEntity<ErrorResponse> handleAccessDeniedException(AccessDeniedException ex, WebRequest request) {
        String path = ((ServletWebRequest) request).getRequest().getRequestURI();
        ErrorResponse errorResponse = ErrorResponse.of(AuthErrorCode.ACCESS_DENIED, path);
        return new ResponseEntity<>(errorResponse, AuthErrorCode.ACCESS_DENIED.getHttpStatus());
    }

    @ExceptionHandler(BadCredentialsException.class)
    public ResponseEntity<ErrorResponse> handleBadCredentialsException(BadCredentialsException ex, WebRequest request) {
        String path = ((ServletWebRequest) request).getRequest().getRequestURI();
        ErrorResponse errorResponse = ErrorResponse.of(AuthErrorCode.INVALID_CREDENTIALS, path);
        return new ResponseEntity<>(errorResponse, AuthErrorCode.INVALID_CREDENTIALS.getHttpStatus());
    }

    // validException 핸들링
    @ExceptionHandler({MethodArgumentNotValidException.class,  // @Valid 검증 실패
            BindException.class,                    // 요청 파라미터 바인딩 실패
            HttpMessageNotReadableException.class,  // JSON 파싱 실패
            TypeMismatchException.class             // 타입 변환 실패
    })
    public ResponseEntity<ErrorResponse> handleValidationExceptions(Exception ex, WebRequest request) {
        String path = ((ServletWebRequest) request).getRequest().getRequestURI();
        String message;

        // 예외 유형별로 적절한 메시지 추출
        if (ex instanceof MethodArgumentNotValidException) {
            message = ((MethodArgumentNotValidException) ex).getBindingResult().getFieldErrors().stream().findFirst().map(FieldError::getDefaultMessage).orElse(CommonErrorCode.INVALID_PARAMETER.getMessage());
        } else if (ex instanceof BindException) {
            message = ((BindException) ex).getBindingResult().getAllErrors().stream().findFirst().map(ObjectError::getDefaultMessage).orElse(CommonErrorCode.INVALID_PARAMETER.getMessage());
        } else {
            message = CommonErrorCode.INVALID_PARAMETER.getMessage();
        }

        ErrorResponse errorResponse = ErrorResponse.of(CommonErrorCode.INVALID_PARAMETER, message, path);
        return new ResponseEntity<>(errorResponse, CommonErrorCode.INVALID_PARAMETER.getHttpStatus());
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorResponse> handleGlobalException(Exception ex, WebRequest request) {
        String path = ((ServletWebRequest) request).getRequest().getRequestURI();
        ErrorResponse errorResponse = ErrorResponse.of(CommonErrorCode.INTERNAL_SERVER_ERROR, ex.getMessage(), path);
        return new ResponseEntity<>(errorResponse, CommonErrorCode.INTERNAL_SERVER_ERROR.getHttpStatus());
    }
}
