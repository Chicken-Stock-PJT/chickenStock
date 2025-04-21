package realClassOne.chickenStock.stock.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.stock.entity.Stock;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockRepository;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class StockSubscriptionService {

    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final StockRepository stockRepository;

    /**
     * 종목 코드를 등록하여 실시간 데이터 구독을 시작합니다.
     * 실패 시 CustomException 예외 발생
     *
     * @param stockCode 종목 코드 (예: 005930)
     */
    public void registerStockForSubscription(String stockCode) {
        validateStockCode(stockCode);

        boolean result = kiwoomWebSocketClient.subscribeStock(stockCode);

        if (result) {
            log.info("종목 {} 구독 등록 성공", stockCode);
        } else {
            log.error("종목 {} 구독 등록 실패", stockCode);
            throw new CustomException(StockErrorCode.SUBSCRIPTION_FAILED);
        }
    }

    /**
     * 종목 코드의 실시간 데이터 구독을 해제합니다.
     * 실패 시 CustomException 예외 발생
     *
     * @param stockCode 종목 코드 (예: 005930)
     */
    public void unregisterStockForSubscription(String stockCode) {
        validateStockCode(stockCode);

        boolean result = kiwoomWebSocketClient.unsubscribeStock(stockCode);

        if (result) {
            log.info("종목 {} 구독 해제 성공", stockCode);
        } else {
            log.error("종목 {} 구독 해제 실패", stockCode);
            throw new CustomException(StockErrorCode.UNSUBSCRIPTION_FAILED);
        }
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

//    /**
//     * 모든 종목을 일괄 구독합니다.
//     *
//     * @return 구독에 성공한 종목 수
//     */
//    public int registerAllStocksForSubscription() {
//        log.info("모든 종목 일괄 구독 시작");
//
//        // StockRepository에서 모든 종목 코드 조회
//        List<String> allStockCodes = stockRepository.findAll().stream()
//                .map(Stock::getShortCode)
//                .collect(Collectors.toList());
//
//        log.info("총 {}개 종목 구독 시도", allStockCodes.size());
//
//        int successCount = 0;
//        int batchSize = 100; // 한 번에 처리할 배치 크기
//
//        // 배치 처리를 통한 구독
//        for (int i = 0; i < allStockCodes.size(); i += batchSize) {
//            int endIndex = Math.min(i + batchSize, allStockCodes.size());
//            List<String> batchCodes = allStockCodes.subList(i, endIndex);
//
//            log.info("배치 구독 진행 중: {}/{} (현재 배치 크기: {})",
//                    i + batchCodes.size(), allStockCodes.size(), batchCodes.size());
//
//            for (String stockCode : batchCodes) {
//                try {
//                    boolean result = kiwoomWebSocketClient.subscribeStock(stockCode);
//                    if (result) {
//                        successCount++;
//                    } else {
//                        log.warn("종목 {} 구독 실패", stockCode);
//                    }
//                } catch (Exception e) {
//                    log.error("종목 {} 구독 중 오류 발생", stockCode, e);
//                }
//            }
//
//            // 서버 부하 방지를 위한 잠시 대기
//            try {
//                Thread.sleep(500);
//            } catch (InterruptedException e) {
//                Thread.currentThread().interrupt();
//                log.error("배치 처리 중 인터럽트 발생", e);
//            }
//        }
//
//        log.info("일괄 구독 완료: 총 {}개 중 {}개 성공", allStockCodes.size(), successCount);
//        return successCount;
//    }
}
