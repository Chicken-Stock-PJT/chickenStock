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
    INVALID_STOCK_CODE(HttpStatus.BAD_REQUEST, "STOCK-E005", "유효하지 않은 종목 코드입니다"),
    CHART_DATA_NOT_FOUND(HttpStatus.NOT_FOUND, "STOCK-E006", "차트 데이터를 찾을 수 없습니다"),
    API_REQUEST_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "STOCK-E007", "API 요청 처리에 실패했습니다"),
    CHART_DATA_PROCESSING_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "STOCK-E008", "차트 데이터 처리에 실패했습니다"),
    AUTHENTICATION_FAILED(HttpStatus.UNAUTHORIZED, "STOCK-E009", "API 인증에 실패했습니다"),
    INVALID_CHART_TYPE(HttpStatus.BAD_REQUEST, "STOCK-E010", "유효하지 않은 차트 타입입니다"),
    INVALID_RANKING_TYPE(HttpStatus.BAD_REQUEST, "STOCK-E011", "유효하지 않은 랭킹 타입입니다"),
    INSUFFICIENT_BALANCE(HttpStatus.BAD_REQUEST, "STOCK-E012", "잔액이 부족합니다"),
    INSUFFICIENT_STOCK(HttpStatus.BAD_REQUEST, "STOCK-E013", "보유 수량이 부족합니다"),
    ORDER_NOT_FOUND(HttpStatus.NOT_FOUND, "STOCK-E014", "주문을 찾을 수 없습니다"),
    PRICE_DATA_NOT_AVAILABLE(HttpStatus.SERVICE_UNAVAILABLE, "STOCK-E015", "현재 시장 가격을 조회할 수 없습니다"),
    ORDER_EXECUTION_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "STOCK-E016", "주문 체결 처리에 실패했습니다"),
    INVALID_ORDER_OPERATION(HttpStatus.BAD_REQUEST, "STOCK-E017", "유효하지 않은 주문 처리입니다"),
    INVALID_REQUEST(HttpStatus.BAD_REQUEST, "STOCK-E018", "유효하지 않은 요청입니다."),
    POSITION_UPDATE_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "STOCK-E019", "포지션 업데이트에 실패했습니다"),
    OPERATION_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "STOCK-E020", "관심종목 삭제 중 오류가 발생했습니다"),
    INVALID_PRICE(HttpStatus.BAD_REQUEST, "STOCK-E021", "유효하지 않은 가격입니다"),
    INVALID_QUANTITY(HttpStatus.BAD_REQUEST, "STOCK-E022", "유효하지 않은 수량입니다"),
    TRADE_PROCESSING_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "STOCK-E023", "거래 처리 중 오류가 발생했습니다"),
    INSUFFICIENT_FUNDS(HttpStatus.BAD_REQUEST, "STOCK-E024", "잔액이 부족합니다"),
    NO_HOLDING_POSITION(HttpStatus.NOT_FOUND, "STOCK-E025", "보유하지 않은 종목입니다"),
    INSUFFICIENT_STOCK_QUANTITY(HttpStatus.BAD_REQUEST, "STOCK-E026", "보유 수량이 부족합니다"),
    UNAUTHORIZED_ORDER_ACCESS(HttpStatus.FORBIDDEN, "STOCK-E027", "해당 주문에 접근 권한이 없습니다"),
    ORDER_NOT_CANCELABLE(HttpStatus.BAD_REQUEST, "STOCK-E028", "취소할 수 없는 주문입니다"),
    ORDER_INQUIRY_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "STOCK-E029", "주문 조회 중 오류가 발생했습니다"),
    TRADE_RESTRICTED(HttpStatus.BAD_REQUEST, "STOCK-E030", "비정상적인 요청");

    private final HttpStatus httpStatus;
    private final String code;
    private final String message;
}
