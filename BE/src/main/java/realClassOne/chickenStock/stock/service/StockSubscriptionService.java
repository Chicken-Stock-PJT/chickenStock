package realClassOne.chickenStock.stock.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.util.Set;

@Service
@RequiredArgsConstructor
@Slf4j
public class StockSubscriptionService {

    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final StockDataRepository stockDataRepository;

    /**
     * 종목 코드를 등록하여 실시간 데이터 구독을 시작합니다.
     * 실패 시 CustomExcepbroadcastToSubscribers tion 예외 발생
     *
     * @param stockCode 종목 코드 (예: 005930)
     */
    @Transactional
    public void registerStockForSubscription(String stockCode, String purpose) {
        validateStockCode(stockCode);

        boolean result = kiwoomWebSocketClient.subscribeStockWithPurpose(stockCode, purpose);

        if (result) {
            log.info("종목 {} 구독 등록 성공 (목적: {})", stockCode, purpose);
        } else {
            log.error("종목 {} 구독 등록 실패 (목적: {})", stockCode, purpose);
            throw new CustomException(StockErrorCode.SUBSCRIPTION_FAILED);
        }
    }

    public void registerStockForSubscription(String stockCode) {
        registerStockForSubscription(stockCode, "DEFAULT");
    }

    @Transactional
    public void unregisterStockForSubscription(String stockCode, String purpose) {
        validateStockCode(stockCode);

        boolean result = kiwoomWebSocketClient.unsubscribeStockForPurpose(stockCode, purpose);

        if (result) {
            log.info("종목 {} 구독 해제 성공 (목적: {})", stockCode, purpose);
        } else {
            log.error("종목 {} 구독 해제 실패 (목적: {})", stockCode, purpose);
            throw new CustomException(StockErrorCode.UNSUBSCRIPTION_FAILED);
        }
    }

    /**
     * 종목 코드의 실시간 데이터 구독을 해제합니다.
     * 실패 시 CustomException 예외 발생
     *
     * @param stockCode 종목 코드 (예: 005930)
     */
    public void unregisterStockForSubscription(String stockCode) {
        unregisterStockForSubscription(stockCode, "DEFAULT");
    }

    /**
     * 현재 구독 중인 종목 코드 목록을 반환합니다.
     *
     * @return 구독 중인 종목 코드 Set
     */
    public Set<String> getSubscribedStocks() {
        return kiwoomWebSocketClient.getSubscribedStockCodes();
    }

    /**
     * 종목 코드 유효성 검사
     */
    private void validateStockCode(String stockCode) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
            log.error("유효하지 않은 종목 코드: {}", stockCode);
            throw new CustomException(StockErrorCode.INVALID_STOCK_CODE);
        }
    }
}
