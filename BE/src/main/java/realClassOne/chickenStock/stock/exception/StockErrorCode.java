package realClassOne.chickenStock.stock.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import realClassOne.chickenStock.common.exception.ErrorCode;

@Getter
@RequiredArgsConstructor
public enum StockErrorCode implements ErrorCode {
    STOCK_NOT_FOUND(HttpStatus.NOT_FOUND, "STOCK-E001", "해당 종목을 찾을 수 없습니다"),
    STOCK_NOT_FOUND_BY_NAME(HttpStatus.NOT_FOUND, "STOCK-E002", "해당 이름의 종목을 찾을 수 없습니다"),
    SUBSCRIPTION_FAILED(HttpStatus.BAD_REQUEST, "STOCK-E003", "종목 구독에 실패했습니다"),
    UNSUBSCRIPTION_FAILED(HttpStatus.BAD_REQUEST, "STOCK-E004", "종목 구독 해제에 실패했습니다"),
    INVALID_STOCK_CODE(HttpStatus.BAD_REQUEST, "STOCK-E005", "유효하지 않은 종목 코드입니다");

    private final HttpStatus httpStatus;
    private final String code;
    private final String message;
}
