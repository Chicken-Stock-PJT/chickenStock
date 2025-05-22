package realClassOne.chickenStock.stock.service;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.stock.dto.request.StockPriceRequestDTO;
import realClassOne.chickenStock.stock.dto.response.StockPriceResponseDTO;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

@Service
@RequiredArgsConstructor
@Slf4j
public class StockPriceService {

    private final KiwoomWebSocketClient kiwoomWebSocketClient;

    /**
     * 여러 종목의 현재가를 조회하는 메서드
     *
     * @param requestDTO 조회할 종목 코드 목록을 포함한 요청 DTO
     * @return 종목별 현재가 정보가 담긴 응답 DTO
     */
    public StockPriceResponseDTO getQuickStockPrices(StockPriceRequestDTO requestDTO) {
        try {
            // 요청 검증
            if (requestDTO.getStockCodes() == null || requestDTO.getStockCodes().isEmpty()) {
                throw new CustomException(StockErrorCode.INVALID_REQUEST, "종목 코드 목록이 필요합니다.");
            }

            // 최대 30개로 제한
            if (requestDTO.getStockCodes().size() > 30) {
                throw new CustomException(StockErrorCode.INVALID_REQUEST, "최대 30개 종목까지만 조회 가능합니다.");
            }

            // 웹소켓 연결 상태 확인
            if (!kiwoomWebSocketClient.isConnected()) {
                throw new CustomException(StockErrorCode.API_REQUEST_FAILED, "WebSocket 서버에 연결되어 있지 않습니다.");
            }

            // 실제 종목 데이터 조회 기능 수행
            return processStockPriceRequest(requestDTO.getStockCodes());

        } catch (CustomException e) {
            // 이미 정의된 에러 코드로 예외가 발생한 경우 그대로 전파
            throw e;
        } catch (Exception e) {
            // 기타 예외는 API_REQUEST_FAILED로 래핑하여 전파
            log.error("주식 가격 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.API_REQUEST_FAILED, "주식 가격 조회 중 오류가 발생했습니다: " + e.getMessage());
        }
    }

    /**
     * 오류 응답 생성을 위한 헬퍼 메서드
     *
     * @param errorMessage 오류 메시지
     * @return 오류 정보가 담긴 응답 DTO
     */
    public StockPriceResponseDTO createErrorResponse(String errorMessage) {
        return StockPriceResponseDTO.builder()
                .errorMessage(errorMessage)
                .timestamp(System.currentTimeMillis())
                .prices(Collections.emptyMap())
                .build();
    }

    /**
     * 여러 종목의 현재가를 빠르게 조회하는 메서드
     * 각 종목을 구독하여 최신 가격을 수집한 후, 구독 해제
     *
     * @param stockCodes 조회할 종목 코드 목록
     * @return 종목별 현재가 정보
     */
    private StockPriceResponseDTO processStockPriceRequest(List<String> stockCodes) {
        // 응답 결과를 저장할 맵
        Map<String, String> resultMap = new ConcurrentHashMap<>();

        // 작업 완료를 추적하기 위한 Future
        CompletableFuture<Map<String, String>> futureResult = new CompletableFuture<>();

        // 완료된 종목 수 추적
        int[] completedCount = {0};

        // 데이터 수신을 위한 리스너 정의
        KiwoomWebSocketClient.StockDataListener listener = new KiwoomWebSocketClient.StockDataListener() {
            @Override
            @Transactional
            public void onStockPriceUpdate(String stockCode, JsonNode data) {
                // 요청한 종목 코드에 포함된 경우에만 처리
                if (stockCodes.contains(stockCode)) {
                    String currentPrice = data.get("10").asText();
                    resultMap.put(stockCode, currentPrice);

                    log.info("종목 {} 가격 수신: {}", stockCode, currentPrice);

                    // 해당 종목 구독 해제
                    kiwoomWebSocketClient.unsubscribeStock(stockCode);

                    // 완료 카운트 증가 및 모든 종목 수집 완료 체크
                    synchronized (completedCount) {
                        completedCount[0]++;
                        if (completedCount[0] >= stockCodes.size()) {
                            // 모든 종목 데이터 수집 완료
                            kiwoomWebSocketClient.removeListener(this);
                            futureResult.complete(resultMap);
                        }
                    }
                }
            }

            @Override
            public void onStockBidAskUpdate(String stockCode, JsonNode data) {
                // 호가 데이터는 여기서 사용하지 않음
            }
        };

        try {
            // 리스너 등록
            kiwoomWebSocketClient.addListener(listener);

            // 각 종목에 대해 구독 시작
            for (String stockCode : stockCodes) {
                // 캐시된 데이터 확인
                JsonNode cachedData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);

                if (cachedData != null) {
                    // 캐시된 데이터가 있으면 바로 사용
                    String currentPrice = cachedData.get("10").asText();
                    resultMap.put(stockCode, currentPrice);

                    log.info("종목 {} 캐시된 가격 사용: {}", stockCode, currentPrice);

                    synchronized (completedCount) {
                        completedCount[0]++;
                    }
                } else {
                    // 캐시된 데이터가 없으면 구독
                    kiwoomWebSocketClient.subscribeStock(stockCode);
                }
            }

            // 이미 모든 종목이 캐시에서 처리된 경우
            synchronized (completedCount) {
                if (completedCount[0] >= stockCodes.size()) {
                    kiwoomWebSocketClient.removeListener(listener);
                    futureResult.complete(resultMap);
                }
            }

            // 최대 3초 대기
            Map<String, String> result = futureResult.get(3, TimeUnit.SECONDS);

            // 응답 생성
            return StockPriceResponseDTO.builder()
                    .timestamp(System.currentTimeMillis())
                    .prices(result)
                    .build();

        } catch (TimeoutException e) {
            // 시간 초과 시 부분 결과라도 반환
            log.warn("일부 종목 데이터만 수집됨 (시간 초과): 요청 {} 종목, 수집 {} 종목",
                    stockCodes.size(), resultMap.size());

            // 리스너 정리 및 구독 해제 처리
            cleanupResources(listener, stockCodes);

            return StockPriceResponseDTO.builder()
                    .timestamp(System.currentTimeMillis())
                    .prices(resultMap)
                    .errorMessage("일부 종목 데이터만 수집되었습니다 (시간 초과)")
                    .build();

        } catch (Exception e) {
            // 예외 발생 시 리스너 정리 및 구독 해제 처리
            cleanupResources(listener, stockCodes);

            // 일부 데이터라도 수집된 경우
            if (!resultMap.isEmpty()) {
                log.warn("일부 종목 데이터만 수집됨 (요청: {}, 수집: {})", stockCodes.size(), resultMap.size());
                return StockPriceResponseDTO.builder()
                        .timestamp(System.currentTimeMillis())
                        .prices(resultMap)
                        .errorMessage("일부 종목 데이터만 수집되었습니다")
                        .build();
            }

            throw new CustomException(StockErrorCode.API_REQUEST_FAILED, "주식 가격 조회 중 오류가 발생했습니다: " + e.getMessage());
        }
    }

    /**
     * 리소스 정리 메서드 - 리스너 제거 및 남은 구독 해제
     *
     * @param listener 제거할 리스너
     * @param stockCodes 구독 해제할 종목 코드 목록
     */
    private void cleanupResources(KiwoomWebSocketClient.StockDataListener listener, List<String> stockCodes) {
        // 리스너 제거
        kiwoomWebSocketClient.removeListener(listener);

        // 구독 중인 종목들 해제
        for (String stockCode : stockCodes) {
            if (kiwoomWebSocketClient.isSubscribed(stockCode)) {
                kiwoomWebSocketClient.unsubscribeStock(stockCode);
            }
        }
    }
}