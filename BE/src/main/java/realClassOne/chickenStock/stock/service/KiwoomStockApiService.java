package realClassOne.chickenStock.stock.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.stock.dto.common.StockResponse;
import realClassOne.chickenStock.stock.dto.response.StockAskBidResponseDTO;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;

@Service
@Slf4j
@RequiredArgsConstructor
public class KiwoomStockApiService {

    @Value("${kiwoom.api.url}")
    private String apiUrl;

    private static final String API_ENDPOINT = "/api/dostk/mrkcond";


    private final KiwoomAuthService authService;
    private final ObjectMapper objectMapper;
    private final StockDataRepository stockDataRepository;

    /**
     * 키움증권 API를 통해 주식 기본 정보 조회
     */
    public JsonNode getStockBasicInfo(String stockCode) {

        try {
            String endpoint = "/api/dostk/stkinfo";
            String url = apiUrl + endpoint;

            WebClient webClient = WebClient.builder()
                    .baseUrl(url)
                    .defaultHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .defaultHeader("authorization", "Bearer " + authService.getAccessToken())
                    .defaultHeader("api-id", "ka10001")  // TR명: 주식기본정보요청
                    .build();

            // 요청 본문 생성
            String requestBody = String.format("{\"stk_cd\":\"%s\"}", stockCode);

            // API 호출
            String response = webClient.post()
                    .bodyValue(requestBody)
                    .retrieve()
                    .bodyToMono(String.class)
                    .block();

            // 응답 파싱
            JsonNode jsonNode = objectMapper.readTree(response);

            // 응답 코드 확인
            if (jsonNode.has("return_code") && jsonNode.get("return_code").asInt() == 0) {
                log.info("키움증권 종목정보 조회 성공: {}", stockCode);

                return jsonNode;
            } else {
                String errorMsg = jsonNode.has("return_msg") ?
                        jsonNode.get("return_msg").asText() : "알 수 없는 오류";
                log.error("키움증권 종목정보 조회 실패: {}, {}", stockCode, errorMsg);
                return null;
            }
        } catch (Exception e) {
            log.error("키움증권 종목정보 조회 중 예외 발생: {}", stockCode, e);
            return null;
        }
    }

    /**
     * 종목 코드를 기반으로 주식 정보와 실시간 데이터를 조합하여 응답 생성
     */
    public StockResponse getEnhancedStockByCode(String code) {
        // 정규화 과정 없이 직접 사용
        log.info("종목 코드로 주식 정보 요청: {}", code);

        // 기본 종목 정보 조회 - 정확히 입력된 코드 그대로 사용
        StockData stockData = stockDataRepository.findByShortCode(code)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

        log.info("종목 정보 조회 성공: {} ({})", stockData.getShortName(), stockData.getShortCode());

        // 기본 응답 객체 생성
        StockResponse.StockResponseBuilder builder = StockResponse.builder()
                .shortCode(stockData.getShortCode())
                .shortName(stockData.getShortName())
                .market(stockData.getMarket())
                .stockType(stockData.getStockType())
                .faceValue(stockData.getFaceValue());

        // 키움증권 API에서 최신 데이터 조회 - 정확히 입력된 코드 그대로 사용
        JsonNode stockInfo = getStockBasicInfo(code);
        if (stockInfo != null) {
            // 전일대비와 등락률 설정
            builder
                    .prevDayCompare(stockInfo.get("pred_pre").asText())
                    .fluctuationRate(stockInfo.get("flu_rt").asText());

            log.info("키움증권 API 데이터 조회 성공: 전일대비={}, 등락률={}",
                    stockInfo.get("pred_pre").asText(), stockInfo.get("flu_rt").asText());
        } else {
            // API 호출 실패 시 예외 발생
            log.error("키움증권 API에서 종목 {}의 가격 데이터를 가져올 수 없습니다.", code);
            throw new CustomException(StockErrorCode.PRICE_DATA_NOT_AVAILABLE,
                    "키움증권 API에서 종목 " + code + "의 가격 데이터를 가져올 수 없습니다.");
        }

        return builder.build();
    }


    /**
     * 특정 종목의 호가 정보를 조회합니다.
     * @param stockCode 종목 코드
     * @return 호가 정보 응답 DTO
     */
    public StockAskBidResponseDTO getStockAskBidInfo(String stockCode) {
        try {
            log.info("키움 API를 통해 종목 [{}]의 호가 정보 조회 시작", stockCode);

            // 1. 접근 토큰 가져오기
            String accessToken = authService.getAccessToken();

            // 2. WebClient 구성
            WebClient webClient = WebClient.builder()
                    .baseUrl(apiUrl)
                    .defaultHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .defaultHeader("authorization", "Bearer " + accessToken)
                    .defaultHeader("api-id", "ka10004")
                    .defaultHeader("cont-yn", "N")
                    .defaultHeader("next-key", "")
                    .build();

            // 3. 요청 본문 생성
            String requestBody = String.format("{\"stk_cd\":\"%s\"}", stockCode);

            // 4. API 호출 및 응답 처리
            String responseBody = webClient.post()
                    .uri(API_ENDPOINT)
                    .bodyValue(requestBody)
                    .retrieve()
                    .bodyToMono(String.class)
                    .block();

            // 5. JSON 응답을 DTO로 변환
            StockAskBidResponseDTO responseDTO = objectMapper.readValue(responseBody, StockAskBidResponseDTO.class);

            // 6. 응답 코드 확인 및 로깅
            if (responseDTO.getReturnCode() != 0) {
                log.error("키움 API 오류 응답: 코드={}, 메시지={}",
                        responseDTO.getReturnCode(), responseDTO.getReturnMsg());

                // API 응답에 오류가 있는 경우 커스텀 예외 발생
                throw new CustomException(StockErrorCode.API_REQUEST_FAILED,
                        "키움 API 응답 오류: " + responseDTO.getReturnMsg());
            }

            log.info("종목 [{}]의 호가 정보 조회 성공: 기준시간={}, 최우선매도호가={}, 최우선매수호가={}",
                    stockCode,
                    responseDTO.getBidReqBaseTm(),
                    responseDTO.getSelFprBid(),
                    responseDTO.getBuyFprBid());

            return responseDTO;

        } catch (CustomException ce) {
            // 이미 생성된 CustomException은 그대로 전파
            throw ce;
        } catch (Exception e) {
            log.error("종목 [{}]의 호가 정보 조회 중 예외 발생", stockCode, e);

            // 일반적인 예외는 API_REQUEST_FAILED로 변환
            throw new CustomException(StockErrorCode.API_REQUEST_FAILED,
                    "호가 정보 조회 중 오류: " + e.getMessage());
        }
    }

    /**
     * 오류 응답 DTO 생성 메서드
     */
    private StockAskBidResponseDTO createErrorResponse(String errorMessage) {
        StockAskBidResponseDTO errorResponse = new StockAskBidResponseDTO();
        errorResponse.setReturnCode(-1);
        errorResponse.setReturnMsg(errorMessage);
        return errorResponse;
    }
}