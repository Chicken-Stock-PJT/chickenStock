package realClassOne.chickenStock.stock.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.stock.dto.common.ChartDataDTO;
import realClassOne.chickenStock.stock.dto.request.ChartRequestDTO;
import realClassOne.chickenStock.stock.dto.response.ChartResponseDTO;
import realClassOne.chickenStock.stock.exception.StockErrorCode;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

@Service
@Slf4j
@RequiredArgsConstructor
public class StockChartService {

    private final KiwoomAuthService kiwoomAuthService;
    private final ObjectMapper objectMapper;
    private final WebClient kiwoomWebClient;
    private final RedisTemplate<String, Object> redisTemplate;

    // 차트 종류별 API ID 매핑
    private static final String API_URL = "/api/dostk/chart";
    private static final String DAILY_CHART_API_ID = "ka10081"; // 일봉
    private static final String WEEKLY_CHART_API_ID = "ka10082"; // 주봉
    private static final String MONTHLY_CHART_API_ID = "ka10083"; // 월봉
    private static final String YEARLY_CHART_API_ID = "ka10094"; // 년봉
    private static final String MINUTE_CHART_API_ID = "ka10080"; // 분봉

    // Redis 캐시 키 접두사
    private static final String REDIS_CHART_KEY_PREFIX = "chart:";
    // 캐시 만료 시간 (1시간)
    private static final long CACHE_TTL_HOURS = 1;

    /**
     * 주식 차트 데이터를 조회합니다.
     *
     * @param request 차트 조회 요청 DTO
     * @return 차트 조회 응답 DTO
     */
    public ChartResponseDTO getStockChart(ChartRequestDTO request) {

        // 종목 코드 유효성 검사
        if (request.getStockCode() == null || request.getStockCode().isEmpty()) {
            throw new CustomException(StockErrorCode.INVALID_STOCK_CODE, "종목 코드는 필수 입력값입니다.");
        }

        // 차트 타입 유효성 검사
        if (request.getChartType() != null &&
                !isValidChartType(request.getChartType())) {
            throw new CustomException(StockErrorCode.INVALID_CHART_TYPE, "유효하지 않은 차트 타입입니다.");
        }

        // 1. 캐시 키 생성
        String cacheKey = buildCacheKey(request);

        // 2. 캐시에서 조회
        ChartResponseDTO cachedResponse = getCachedResponse(cacheKey);
        if (cachedResponse != null && ("N".equals(request.getContYn()) || request.getContYn() == null)) {
            log.info("차트 데이터 캐시 히트: {}", cacheKey);
            return cachedResponse;
        }

        // 3. API 호출 및 결과 처리
        try {
            String apiId = getApiIdByChartType(request.getChartType());
            String requestBody = buildRequestBody(request);

            // API 요청을 위한 헤더와 바디 설정
            String accessToken = kiwoomAuthService.getAccessToken();

            // 응답 받기
            Map<String, Object> apiResponse = makeApiRequest(
                    apiId, accessToken, requestBody, request.getContYn(), request.getNextKey());

            // 응답 처리
            ChartResponseDTO response = processResponse(apiResponse, request.getStockCode(), request.getChartType());

            // 4. 캐시에 저장 (연속조회가 아닐 경우에만)
            if (("N".equals(request.getContYn()) || request.getContYn() == null) && response.getCode() == 0) {
                cacheResponse(cacheKey, response);
            }

            return response;

        } catch (CustomException e) {
            // 이미 CustomException으로 처리된 예외는 그대로 전파
            throw e;
        } catch (Exception e) {
            log.error("키움 API 호출 중 오류 발생: {}", e.getMessage(), e);
            throw new CustomException(StockErrorCode.API_REQUEST_FAILED, "차트 데이터 조회 실패: " + e.getMessage());
        }
    }

    /**
     * 요청 파라미터로 주식 차트 데이터를 조회합니다.
     */
    public ChartResponseDTO getStockChartByType(
            String chartType, String stockCode, String baseDate,
            String timeInterval, String modifiedPriceType, String contYn, String nextKey) {

        if (stockCode == null || stockCode.isEmpty()) {
            throw new CustomException(StockErrorCode.INVALID_STOCK_CODE, "종목 코드는 필수 입력값입니다.");
        }

        ChartRequestDTO request = ChartRequestDTO.builder()
                .chartType(chartType)
                .stockCode(stockCode)
                .baseDate(baseDate != null ? baseDate : getTodayDate())
                .timeInterval(timeInterval)
                .modifiedPriceType(modifiedPriceType != null ? modifiedPriceType : "1")
                .contYn(contYn)
                .nextKey(nextKey)
                .build();

        return getStockChart(request);
    }

    /**
     * 종목의 모든 차트 데이터를 한 번에 조회합니다.
     * 여러 페이지에 걸친 데이터를 모두 가져옵니다.
     */
    public ChartResponseDTO getAllChartData(
            String stockCode, String baseDate, String chartType, String modifiedPriceType) {

        if (stockCode == null || stockCode.isEmpty()) {
            throw new CustomException(StockErrorCode.INVALID_STOCK_CODE, "종목 코드는 필수 입력값입니다.");
        }

        // 캐시 키 생성
        String cacheKey = buildAllDataCacheKey(stockCode, baseDate, chartType, modifiedPriceType);

        // 캐시에서 조회
        ChartResponseDTO cachedResponse = getCachedResponse(cacheKey);
        if (cachedResponse != null) {
            log.info("모든 차트 데이터 캐시 히트: {}", cacheKey);
            return cachedResponse;
        }

        ChartRequestDTO initialRequest = ChartRequestDTO.builder()
                .chartType(chartType)
                .stockCode(stockCode)
                .baseDate(baseDate != null ? baseDate : getTodayDate())
                .modifiedPriceType(modifiedPriceType != null ? modifiedPriceType : "1")
                .contYn("N")
                .build();

        ChartResponseDTO firstResponse = getStockChart(initialRequest);

        // API 호출 실패 시 바로 반환
        if (firstResponse.getCode() != 0) {
            return firstResponse;
        }

        // 연속조회가 없으면 바로 반환
        if (!firstResponse.isHasNext()) {
            // 캐시에 저장
            cacheResponse(cacheKey, firstResponse);
            return firstResponse;
        }

        // 연속조회 결과를 첫 응답에 합치기
        List<ChartDataDTO> allChartData = new ArrayList<>(firstResponse.getChartData());
        String nextKey = firstResponse.getNextKey();

        while (nextKey != null && !nextKey.isEmpty()) {
            ChartRequestDTO nextRequest = ChartRequestDTO.builder()
                    .chartType(chartType)
                    .stockCode(stockCode)
                    .baseDate(baseDate != null ? baseDate : getTodayDate())
                    .modifiedPriceType(modifiedPriceType != null ? modifiedPriceType : "1")
                    .contYn("Y")
                    .nextKey(nextKey)
                    .build();

            ChartResponseDTO nextResponse = getStockChart(nextRequest);

            // API 호출 실패 시 중단
            if (nextResponse.getCode() != 0) {
                break;
            }

            // 결과를 합치기
            if (nextResponse.getChartData() != null && !nextResponse.getChartData().isEmpty()) {
                allChartData.addAll(nextResponse.getChartData());
            }

            // 더 이상 데이터가 없으면 종료
            if (!nextResponse.isHasNext()) {
                break;
            }

            // 다음 요청 설정
            nextKey = nextResponse.getNextKey();
        }

        // 최종 응답 생성
        ChartResponseDTO finalResponse = ChartResponseDTO.builder()
                .stockCode(revertStockCode(stockCode)) // ✅ 여기서 _AL 제거
                .chartType(chartType)
                .chartData(allChartData)
                .hasNext(false)
                .nextKey("")
                .code(0)
                .message("성공")
                .build();

        // 캐시에 저장
        cacheResponse(cacheKey, finalResponse);

        return finalResponse;
    }

    /**
     * 차트 타입의 유효성을 검증합니다.
     */
    private boolean isValidChartType(String chartType) {
        if (chartType == null) {
            return true; // null은 기본값(일봉)을 사용하므로 유효함
        }

        String upperCaseType = chartType.toUpperCase();
        return "DAILY".equals(upperCaseType) ||
                "WEEKLY".equals(upperCaseType) ||
                "MONTHLY".equals(upperCaseType) ||
                "YEARLY".equals(upperCaseType) ||
                "MINUTE".equals(upperCaseType);
    }

    /**
     * 차트 타입에 따른 API ID를 반환합니다.
     */
    private String getApiIdByChartType(String chartType) {
        if (chartType == null) {
            return DAILY_CHART_API_ID; // 기본값: 일봉
        }

        switch (chartType.toUpperCase()) {
            case "DAILY":
                return DAILY_CHART_API_ID;
            case "WEEKLY":
                return WEEKLY_CHART_API_ID;
            case "MONTHLY":
                return MONTHLY_CHART_API_ID;
            case "YEARLY":
                return YEARLY_CHART_API_ID;
            case "MINUTE":
                return MINUTE_CHART_API_ID;
            default:
                throw new CustomException(StockErrorCode.INVALID_CHART_TYPE, "지원하지 않는 차트 타입입니다: " + chartType);
        }
    }

    /**
     * 요청 바디 JSON을 생성합니다.
     */
    private String buildRequestBody(ChartRequestDTO request) {
        try {
            String stockCodeWithAL = convertStockCode(request.getStockCode()); // ✅ 여기서 _AL 붙이기

            if ("MINUTE".equalsIgnoreCase(request.getChartType())) {
                return String.format(
                        "{\"stk_cd\":\"%s\",\"tic_scope\":\"%s\",\"upd_stkpc_tp\":\"%s\"}",
                        stockCodeWithAL,
                        request.getTimeInterval() != null ? request.getTimeInterval() : "1",
                        request.getModifiedPriceType() != null ? request.getModifiedPriceType() : "1"
                );
            } else {
                return String.format(
                        "{\"stk_cd\":\"%s\",\"base_dt\":\"%s\",\"upd_stkpc_tp\":\"%s\"}",
                        stockCodeWithAL,
                        request.getBaseDate() != null ? request.getBaseDate() : getTodayDate(),
                        request.getModifiedPriceType() != null ? request.getModifiedPriceType() : "1"
                );
            }
        } catch (Exception e) {
            log.error("요청 바디 생성 중 오류 발생", e);
            throw new CustomException(StockErrorCode.INVALID_STOCK_CODE, "요청 바디 생성 실패");
        }
    }

    /**
     * 현재 날짜를 YYYYMMDD 형식으로 반환합니다.
     */
    private String getTodayDate() {
        return LocalDate.now().format(DateTimeFormatter.ofPattern("yyyyMMdd"));
    }

    /**
     * 키움 API를 호출하여 응답을 받습니다.
     */
    private Map<String, Object> makeApiRequest(
            String apiId, String accessToken, String requestBody, String contYn, String nextKey) {
        try {
            // WebClient를 사용하여 API 요청
            WebClient.ResponseSpec responseSpec = kiwoomWebClient.post()
                    .uri(API_URL)
                    .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .header("authorization", "Bearer " + accessToken)
                    .header("api-id", apiId)
                    .header("cont-yn", contYn != null ? contYn : "N")
                    .header("next-key", nextKey != null ? nextKey : "")
                    .bodyValue(requestBody)
                    .retrieve();

            // 응답 헤더 정보 추출
            Mono<Map<String, Object>> responseMono = responseSpec
                    .toEntity(String.class)
                    .map(response -> {
                        try {
                            // 응답 본문을 JsonNode로 변환
                            JsonNode responseBody = objectMapper.readTree(response.getBody());

                            // 응답 헤더에서 연속조회 관련 정보 추출
                            String contYnHeader = response.getHeaders().getFirst("cont-yn");
                            String nextKeyHeader = response.getHeaders().getFirst("next-key");

                            // 결과를 Map으로 구성하여 반환
                            Map<String, Object> result = objectMapper.convertValue(responseBody, Map.class);
                            result.put("_contYn", contYnHeader != null ? contYnHeader : "N");
                            result.put("_nextKey", nextKeyHeader != null ? nextKeyHeader : "");

                            return result;
                        } catch (Exception e) {
                            throw new CustomException(StockErrorCode.API_REQUEST_FAILED, "API 응답 처리 실패");
                        }
                    });

            return responseMono.block();
        } catch (Exception e) {
            log.error("키움 API 호출 중 오류: {}", e.getMessage(), e);
            throw new CustomException(StockErrorCode.API_REQUEST_FAILED, "API 호출 실패: " + e.getMessage());
        }
    }

    /**
     * API 응답을 ChartResponseDTO로 변환합니다.
     */
    private ChartResponseDTO processResponse(Map<String, Object> response, String stockCode, String chartType) {
        try {
            int returnCode = (int) response.get("return_code");
            String returnMsg = (String) response.get("return_msg");

            if (returnCode != 0) {
                log.error("키움 API 오류: {} - {}", returnCode, returnMsg);
                return ChartResponseDTO.builder()
                        .code(returnCode)
                        .message(returnMsg)
                        .build();
            }

            // 연속조회 정보
            String contYn = (String) response.getOrDefault("_contYn", "N");
            String nextKey = (String) response.getOrDefault("_nextKey", "");
            boolean hasNext = "Y".equals(contYn);

            // 차트 데이터 필드명 결정
            String chartDataField = getChartDataFieldByType(chartType);
            List<Map<String, Object>> chartDataList = (List<Map<String, Object>>) response.get(chartDataField);

            List<ChartDataDTO> dataList = new ArrayList<>();

            if (chartDataList != null && !chartDataList.isEmpty()) {
                for (Map<String, Object> item : chartDataList) {
                    dataList.add(mapToChartDataDto(item, chartType));
                }
            }

            return ChartResponseDTO.builder()
                    .stockCode(stockCode)
                    .chartType(chartType)
                    .chartData(dataList)
                    .hasNext(hasNext)
                    .nextKey(nextKey)
                    .code(returnCode)
                    .message(returnMsg)
                    .build();

        } catch (Exception e) {
            log.error("응답 처리 중 오류: {}", e.getMessage(), e);
            throw new CustomException(StockErrorCode.CHART_DATA_PROCESSING_FAILED, "차트 데이터 처리 중 오류 발생: " + e.getMessage());
        }
    }

    /**
     * 차트 타입에 따른 응답 필드명을 반환합니다.
     */
    private String getChartDataFieldByType(String chartType) {
        if (chartType == null) {
            return "stk_dt_pole_chart_qry"; // 기본값: 일봉
        }

        switch (chartType.toUpperCase()) {
            case "DAILY":
                return "stk_dt_pole_chart_qry";
            case "WEEKLY":
                return "stk_wk_pole_chart_qry";
            case "MONTHLY":
                return "stk_mt_pole_chart_qry";
            case "YEARLY":
                return "stk_yr_pole_chart_qry";
            case "MINUTE":
                return "stk_min_pole_chart_qry";
            default:
                throw new CustomException(StockErrorCode.INVALID_CHART_TYPE, "지원하지 않는 차트 타입입니다: " + chartType);
        }
    }

    /**
     * API 응답 데이터를 ChartDataDTO로 변환합니다.
     */
    private ChartDataDTO mapToChartDataDto(Map<String, Object> item, String chartType) {
        try {
            ChartDataDTO.ChartDataDTOBuilder builder = ChartDataDTO.builder();

            // 공통 필드
            builder.currentPrice(getString(item, "cur_prc"));
            builder.openPrice(getString(item, "open_pric"));
            builder.highPrice(getString(item, "high_pric"));
            builder.lowPrice(getString(item, "low_pric"));
            builder.volume(getString(item, "trde_qty"));
            builder.tradingValue(getString(item, "trde_prica"));
            builder.modifiedRatio(getString(item, "upd_rt"));
            builder.previousClosePrice(getString(item, "pred_close_pric"));

            // 차트 타입에 따른 날짜/시간 필드 설정
            if ("MINUTE".equalsIgnoreCase(chartType)) {
                builder.date(getString(item, "cntr_tm")); // YYYYMMDDHHmmss
            } else {
                builder.date(getString(item, "dt")); // YYYYMMDD
            }

            return builder.build();
        } catch (Exception e) {
            log.error("차트 데이터 매핑 중 오류: {}", e.getMessage(), e);
            throw new CustomException(StockErrorCode.CHART_DATA_PROCESSING_FAILED, "차트 데이터 변환 중 오류 발생");
        }
    }

    /**
     * Map에서 키에 해당하는 값을 문자열로 반환합니다.
     */
    private String getString(Map<String, Object> map, String key) {
        Object value = map.get(key);
        return value != null ? value.toString() : "";
    }

    // 캐시 관련 메서드

    /**
     * 요청에 대한 캐시 키를 생성합니다.
     */
    private String buildCacheKey(ChartRequestDTO request) {
        return REDIS_CHART_KEY_PREFIX +
                request.getChartType() + ":" +
                request.getStockCode() + ":" +
                (request.getBaseDate() != null ? request.getBaseDate() : getTodayDate()) + ":" +
                (request.getTimeInterval() != null ? request.getTimeInterval() : "") + ":" +
                (request.getModifiedPriceType() != null ? request.getModifiedPriceType() : "1");
    }

    /**
     * 전체 데이터 조회에 대한 캐시 키를 생성합니다.
     */
    private String buildAllDataCacheKey(String stockCode, String baseDate, String chartType, String modifiedPriceType) {
        return REDIS_CHART_KEY_PREFIX + "all:" +
                chartType + ":" +
                stockCode + ":" +
                (baseDate != null ? baseDate : getTodayDate()) + ":" +
                (modifiedPriceType != null ? modifiedPriceType : "1");
    }

    /**
     * Redis 캐시에서 데이터를 조회합니다.
     */
    @SuppressWarnings("unchecked")
    private ChartResponseDTO getCachedResponse(String cacheKey) {
        try {
            return (ChartResponseDTO) redisTemplate.opsForValue().get(cacheKey);
        } catch (Exception e) {
            log.warn("Redis 캐시 조회 실패: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Redis 캐시에 데이터를 저장합니다.
     */
    private void cacheResponse(String cacheKey, ChartResponseDTO response) {
        try {
            redisTemplate.opsForValue().set(
                    cacheKey,
                    response,
                    CACHE_TTL_HOURS,
                    TimeUnit.HOURS
            );
            log.debug("Redis 캐시 저장 성공: {}", cacheKey);
        } catch (Exception e) {
            log.warn("Redis 캐시 저장 실패: {}", e.getMessage());
        }
    }

    // _AL 붙이는 함수 추가
    private String convertStockCode(String code) {
        return code.endsWith("_AL") ? code : code + "_AL";
    }

    private String revertStockCode(String code) {
        return code.endsWith("_AL") ? code.replace("_AL", "") : code;
    }

}