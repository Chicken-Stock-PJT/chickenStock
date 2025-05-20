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
import realClassOne.chickenStock.stock.dto.response.StockInfoResponseDTO;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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

    private String convertStockCode(String stockCode) {
        if (stockCode == null || stockCode.trim().isEmpty()) {
            return stockCode;
        }

        return stockCode.trim() + "_AL";
    }

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
            String requestBody = String.format("{\"stk_cd\":\"%s\"}", convertStockCode(stockCode));

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

            // 1. 접근 토큰 가져오기
            String accessToken = authService.getAccessToken();

            // 종목코드 변환 추가 (SOR 방식 사용을 위해 _AL 추가)
            String stockCodeForAPI = convertStockCode(stockCode);

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
            String requestBody = String.format("{\"stk_cd\":\"%s\"}", stockCodeForAPI);

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

    // 주식 종목코드로 현재가 정보를 조회합니다.
    public StockInfoResponseDTO getStockInfo(String stockCode) {
        try {

            // 데이터베이스에서 해당 종목 기본 정보 조회
            StockData stockData = stockDataRepository.findByShortCode(stockCode)
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND,
                            "종목 코드 " + stockCode + "에 해당하는 종목을 찾을 수 없습니다."));

            // 키움증권 API 호출을 위한 요청 본문 생성 - SOR 방식 사용을 위해 _AL 추가
            String stockCodeForAPI = convertStockCode(stockCode); // _AL 붙이는 함수 사용
            String requestBody = String.format("{\"stk_cd\":\"%s\"}", stockCodeForAPI);


            // API 응답 받아오기
            String endpoint = "/api/dostk/stkinfo";
            String url = apiUrl + endpoint;

            WebClient webClient = WebClient.builder()
                    .baseUrl(url)
                    .defaultHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .defaultHeader("authorization", "Bearer " + authService.getAccessToken())
                    .defaultHeader("api-id", "ka10001")  // TR명: 주식기본정보요청
                    .defaultHeader("cont-yn", "N")
                    .defaultHeader("next-key", "")
                    .build();

            String responseString = webClient.post()
                    .bodyValue(requestBody)
                    .retrieve()
                    .bodyToMono(String.class)
                    .block();

            // 응답 파싱
            JsonNode responseJson = objectMapper.readTree(responseString);

            // 응답 코드 확인
            if (responseJson.has("return_code") && responseJson.get("return_code").asInt() != 0) {
                String errorMsg = responseJson.has("return_msg") ?
                        responseJson.get("return_msg").asText() : "API 요청 실패";
                log.error("키움증권 API 요청 실패: {}", errorMsg);
                throw new CustomException(StockErrorCode.API_REQUEST_FAILED, errorMsg);
            }

            // 현재가, 전일대비, 등락률 정보 추출
            String currentPrice = responseJson.has("cur_prc") ? responseJson.get("cur_prc").asText() : "0";
            String priceChange = responseJson.has("pred_pre") ? responseJson.get("pred_pre").asText() : "0";
            String changeRate = responseJson.has("flu_rt") ? responseJson.get("flu_rt").asText() : "0.0";

            // 응답 DTO 생성
            StockInfoResponseDTO responseDTO = StockInfoResponseDTO.builder()
                    .stockCode(stockData.getShortCode())
                    .stockName(stockData.getShortName())
                    .currentPrice(currentPrice)
                    .priceChange(priceChange)
                    .changeRate(changeRate)
                    .build();

            return responseDTO;

        } catch (CustomException ce) {
            throw ce;
        } catch (Exception e) {
            log.error("종목 정보 조회 중 예외 발생: {}", stockCode, e);
            throw new CustomException(StockErrorCode.API_REQUEST_FAILED,
                    "종목 정보 조회 중 오류가 발생했습니다: " + e.getMessage());
        }
    }

    /**
     * 키움증권 API를 통해 관심종목 정보를 조회합니다.
     *
     * @param stockCodes '|'로 구분된 종목 코드 문자열
     * @return API 응답 JsonNode
     */
    public JsonNode getWatchListInfo(String stockCodes) {
        try {

            String endpoint = "/api/dostk/stkinfo";
            String url = apiUrl + endpoint;

            WebClient webClient = WebClient.builder()
                    .baseUrl(url)
                    .defaultHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .defaultHeader("authorization", "Bearer " + authService.getAccessToken())
                    .defaultHeader("api-id", "ka10095") // 관심종목정보요청 TR
                    .build();

            // 각 종목코드에 _AL을 붙이는 처리 추가
            String formattedStockCodes = Arrays.stream(stockCodes.split("\\|"))
                    .map(this::convertStockCode)  // _AL을 붙이는 함수 사용
                    .collect(Collectors.joining("|"));


            // 요청 본문 생성
            String requestBody = String.format("{\"stk_cd\":\"%s\"}", formattedStockCodes);

            // API 호출 및 응답 처리
            String response = webClient.post()
                    .bodyValue(requestBody)
                    .retrieve()
                    .bodyToMono(String.class)
                    .block();

            JsonNode jsonNode = objectMapper.readTree(response);

            // 응답 코드 확인
            if (jsonNode.has("return_code") && jsonNode.get("return_code").asInt() == 0) {
                return jsonNode;
            } else {
                String errorMsg = jsonNode.has("return_msg") ?
                        jsonNode.get("return_msg").asText() : "API 요청 실패";
                log.error("키움증권 관심종목 정보 조회 실패: {}", errorMsg);
                throw new CustomException(StockErrorCode.API_REQUEST_FAILED, errorMsg);
            }
        } catch (CustomException ce) {
            throw ce;
        } catch (Exception e) {
            log.error("키움증권 관심종목 정보 조회 중 예외 발생: {}", stockCodes, e);
            throw new CustomException(StockErrorCode.API_REQUEST_FAILED,
                    "키움증권 API 요청 중 오류가 발생했습니다: " + e.getMessage());
        }
    }

    /**
     * 관심종목 정보를 조회합니다.
     * @param stockCodes '|'로 구분된 종목 코드 문자열
     * @return 관심종목 정보 맵 (종목코드 -> JsonNode)
     */
    public Map<String, JsonNode> getWatchListInfoMap(List<String> stockCodes) {
        if (stockCodes.isEmpty()) {
            return new HashMap<>();
        }

        // 종목 코드 문자열 생성 (여러 종목 코드를 | 로 구분)
        String stockCodeParam = String.join("|", stockCodes);

        // API 호출 결과를 저장할 맵
        Map<String, JsonNode> stockDataMap = new HashMap<>();

        try {
            // 기존의 getWatchListInfo 메서드 활용
            JsonNode responseJson = getWatchListInfo(stockCodeParam);

            // 관심종목정보 리스트 조회
            if (responseJson.has("atn_stk_infr") && responseJson.get("atn_stk_infr").isArray()) {
                JsonNode stockInfoList = responseJson.get("atn_stk_infr");

                // 종목별 데이터 맵 생성
                for (JsonNode stockInfo : stockInfoList) {
                    String stockCode = stockInfo.get("stk_cd").asText();
                    stockDataMap.put(stockCode, stockInfo);
                }

                log.info("관심종목정보요청 API 호출 성공: {} 종목", stockDataMap.size());
            }

            return stockDataMap;
        } catch (Exception e) {
            log.error("관심종목 정보 조회 중 오류 발생", e);
            return new HashMap<>();
        }
    }

}