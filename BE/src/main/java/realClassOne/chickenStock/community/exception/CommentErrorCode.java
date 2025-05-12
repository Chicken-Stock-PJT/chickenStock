package realClassOne.chickenStock.community.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import realClassOne.chickenStock.common.exception.ErrorCode;

@Getter
@RequiredArgsConstructor
public enum CommentErrorCode implements ErrorCode {
    COMMENT_NOT_FOUND(HttpStatus.NOT_FOUND, "COMMENT-E001", "댓글을 찾을 수 없습니다."),
    NOT_AUTHOR(HttpStatus.BAD_REQUEST, "COMMENT-E002", "작성자가 아닙니다."),
    COMMENT_STOCK_MISMATCH(HttpStatus.BAD_REQUEST, "COMMENT-E003", "댓글이 해당 종목에 속하지 않습니다."),
    UNAUTHORIZED_COMMENT_DELETE(HttpStatus.NOT_ACCEPTABLE, "COMMENT-E004", "댓글을 삭제할 권한이 없습니다.");

    private final HttpStatus httpStatus;
    private final String message;
    private final String code;
}
