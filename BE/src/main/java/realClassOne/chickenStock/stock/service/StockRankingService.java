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
import realClassOne.chickenStock.stock.dto.common.StockRankingItemDTO;
import realClassOne.chickenStock.stock.dto.request.StockRankingRequestDTO;
import realClassOne.chickenStock.stock.dto.response.StockRankingResponseDTO;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;

import java.util.*;
import java.util.concurrent.TimeUnit;

@Service
@Slf4j
@RequiredArgsConstructor
public class StockRankingService {

    private final KiwoomAuthService kiwoomAuthService;
    private final ObjectMapper objectMapper;
    private final WebClient kiwoomWebClient;
    private final RedisTemplate<String, Object> redisTemplate;
    private final StockDataRepository stockDataRepository;

    // API URL 및 ID 상수
    private static final String API_URL = "/api/dostk/rkinfo";
    private static final String TRADE_AMOUNT_API_ID = "ka10032"; // 거래대금상위
    private static final String FLUCTUATION_RATE_API_ID = "ka10027"; // 전일대비등락률상위
    private static final String TRADE_VOLUME_API_ID = "ka10030"; // 당일거래량상위

    // Redis 캐시 키 접두사 및 만료 시간
    private static final String REDIS_RANKING_KEY_PREFIX = "ranking:";
    // Redis 캐시 만료 시간
    private static final long CACHE_TTL_SECONDS = 20; // 20초 캐시 유효시간

    // 기본 요청 파라미터 - 주요 조회 케이스에 대한 기본값
    private static final String DEFAULT_MARKET_TYPE = "000"; // 전체 시장
    private static final String DEFAULT_INCLUDE_MANAGEMENT = "1"; // 관리종목 포함
    private static final String DEFAULT_SORT_TYPE = "1"; // 기본 정렬 타입
    private static final String DEFAULT_EXCHANGE_TYPE = "3"; // 통합

    /**
     * 거래대금 상위 종목 목록을 조회합니다.
     *
     * @param marketType        시장구분 (000:전체, 001:코스피, 101:코스닥)
     * @param includeManagement 관리종목포함 여부 (0:미포함, 1:포함)
     * @return 거래대금 상위 종목 목록
     */
    public StockRankingResponseDTO getTradeAmountTopList(String marketType, String includeManagement) {
        String cacheKey = buildTradeAmountCacheKey(
                marketType != null ? marketType : DEFAULT_MARKET_TYPE,
                includeManagement != null ? includeManagement : DEFAULT_INCLUDE_MANAGEMENT
        );

        // 캐시에서 데이터 조회
        StockRankingResponseDTO cachedResponse = getCachedResponse(cacheKey);
        if (cachedResponse != null) {
            log.debug("캐시에서 거래대금 상위 데이터 반환: {}", cacheKey);
            return cachedResponse;
        }

        // 캐시에 없는 경우 기본 응답 반환
        log.warn("거래대금 상위 데이터가 캐시에 없습니다. 백그라운드에서 업데이트 예정입니다.");
        return getEmptyRankingResponse("TRADE_AMOUNT");
    }

    /**
     * 전일대비등락률 상위 종목 목록을 조회합니다.
     *
     * @param marketType 시장구분 (000:전체, 001:코스피, 101:코스닥)
     * @param sortType   정렬구분 (1:상승률, 2:상승폭, 3:하락률, 4:하락폭)
     * @return 전일대비등락률 상위 종목 목록
     */
    public StockRankingResponseDTO getFluctuationRateTopList(String marketType, String sortType) {
        String cacheKey = buildFluctuationRateCacheKey(
                marketType != null ? marketType : DEFAULT_MARKET_TYPE,
                sortType != null ? sortType : DEFAULT_SORT_TYPE
        );

        // 캐시에서 데이터 조회
        StockRankingResponseDTO cachedResponse = getCachedResponse(cacheKey);
        if (cachedResponse != null) {
            log.debug("캐시에서 전일대비등락률 상위 데이터 반환: {}", cacheKey);
            return cachedResponse;
        }

        // 캐시에 없는 경우 기본 응답 반환
        log.warn("전일대비등락률 상위 데이터가 캐시에 없습니다. 백그라운드에서 업데이트 예정입니다.");
        return getEmptyRankingResponse("FLUCTUATION_RATE");
    }

    /**
     * 당일거래량 상위 종목 목록을 조회합니다.
     *
     * @param marketType 시장구분 (000:전체, 001:코스피, 101:코스닥)
     * @param sortType   정렬구분 (1:거래량, 2:거래회전율, 3:거래대금)
     * @return 당일거래량 상위 종목 목록
     */
    public StockRankingResponseDTO getTradeVolumeTopList(String marketType, String sortType) {
        String cacheKey = buildTradeVolumeCacheKey(
                marketType != null ? marketType : DEFAULT_MARKET_TYPE,
                sortType != null ? sortType : DEFAULT_SORT_TYPE
        );

        // 캐시에서 데이터 조회
        StockRankingResponseDTO cachedResponse = getCachedResponse(cacheKey);
        if (cachedResponse != null) {
            log.debug("캐시에서 당일거래량 상위 데이터 반환: {}", cacheKey);
            return cachedResponse;
        }

        // 캐시에 없는 경우 기본 응답 반환
        log.warn("당일거래량 상위 데이터가 캐시에 없습니다. 백그라운드에서 업데이트 예정입니다.");
        return getEmptyRankingResponse("TRADE_VOLUME");
    }

    /**
     * 빈 순위 응답을 생성합니다. 캐시에 데이터가 없을 때 임시로 반환합니다.
     */
    private StockRankingResponseDTO getEmptyRankingResponse(String rankingType) {
        return StockRankingResponseDTO.builder()
                .rankingType(rankingType)
                .rankingItems(new ArrayList<>())
                .hasNext(false)
                .nextKey("")
                .code(0)
                .message("데이터를 불러오는 중입니다. 잠시 후 다시 시도해주세요.")
                .build();
    }

    /**
     * 특정 매개변수에 대한 거래대금 상위 종목 데이터를 업데이트합니다.
     */
    public void updateTradeAmountRankingForParams(String marketType, String includeManagement) {
        try {
            StockRankingRequestDTO request = StockRankingRequestDTO.builder()
                    .rankingType("TRADE_AMOUNT")
                    .marketType(marketType)
                    .includeManagement(includeManagement)
                    .exchangeType(DEFAULT_EXCHANGE_TYPE)
                    .build();

            StockRankingResponseDTO response = fetchStockRanking(request);
            if (response.getCode() == 0) {
                String cacheKey = buildTradeAmountCacheKey(marketType, includeManagement);
                cacheResponse(cacheKey, response);
                log.debug("거래대금 상위 데이터 캐시 업데이트: {}, 항목 수: {}", cacheKey, response.getRankingItems().size());
            }
        } catch (Exception e) {
            log.error("거래대금 상위 데이터 업데이트 실패: market={}, include={}", marketType, includeManagement, e);
        }
    }

    /**
     * 특정 매개변수에 대한 전일대비등락률 상위 종목 데이터를 업데이트합니다.
     */
    public void updateFluctuationRateRankingForParams(String marketType, String sortType) {
        try {
            StockRankingRequestDTO.StockRankingRequestDTOBuilder builder = StockRankingRequestDTO.builder()
                    .rankingType("FLUCTUATION_RATE")
                    .marketType(marketType)
                    .sortType(sortType)
                    .tradeVolumeCondition("0000") // 전체조회
                    .stockCondition("0") // 전체조회
                    .creditCondition("0") // 전체조회
                    .includeUpDownLimit("1") // 상하한가 포함
                    .priceCondition("0") // 전체조회
                    .tradeAmountCondition("0") // 전체조회
                    .exchangeType(DEFAULT_EXCHANGE_TYPE); // 통합

            // 하락률 또는 하락폭일 경우 추가 파라미터 설정
            if ("3".equals(sortType) || "4".equals(sortType)) {
                // 필요한 경우 여기에 하락률/하락폭에 특화된 추가 파라미터 설정
                // 예: builder.additionalParam("value");
            }

            StockRankingRequestDTO request = builder.build();
            StockRankingResponseDTO response = fetchStockRanking(request);

            if (response.getCode() == 0) {
                String cacheKey = buildFluctuationRateCacheKey(marketType, sortType);
                cacheResponse(cacheKey, response);
                log.debug("전일대비등락률 상위 데이터 캐시 업데이트: {}, 항목 수: {}", cacheKey, response.getRankingItems().size());
            }
        } catch (Exception e) {
            log.error("전일대비등락률 상위 데이터 업데이트 실패: market={}, sort={}", marketType, sortType, e);
        }
    }

    /**
     * 특정 매개변수에 대한 당일거래량 상위 종목 데이터를 업데이트합니다.
     */
    public void updateTradeVolumeRankingForParams(String marketType, String sortType) {
        try {
            StockRankingRequestDTO request = StockRankingRequestDTO.builder()
                    .rankingType("TRADE_VOLUME")
                    .marketType(marketType)
                    .sortType(sortType)
                    .includeManagement("0") // 관리종목 포함
                    .creditType("0") // 전체조회
                    .tradeVolumeType("0") // 전체조회
                    .priceType("0") // 전체조회
                    .tradeAmountType("0") // 전체조회
                    .marketOpenType("0") // 전체조회
                    .exchangeType(DEFAULT_EXCHANGE_TYPE) // 통합
                    .build();

            StockRankingResponseDTO response = fetchStockRanking(request);
            if (response.getCode() == 0) {
                String cacheKey = buildTradeVolumeCacheKey(marketType, sortType);
                cacheResponse(cacheKey, response);
                log.debug("당일거래량 상위 데이터 캐시 업데이트: {}, 항목 수: {}", cacheKey, response.getRankingItems().size());
            }
        } catch (Exception e) {
            log.error("당일거래량 상위 데이터 업데이트 실패: market={}, sort={}", marketType, sortType, e);
        }
    }

    /**
     * 주식 순위 데이터를 키움 API에서 조회합니다. (백그라운드 업데이트용)
     * 최대 100개의 종목이 반환되도록 연속 조회를 수행합니다.
     */
    private StockRankingResponseDTO fetchStockRanking(StockRankingRequestDTO request) {
        try {
            String apiId = getApiIdByRankingType(request.getRankingType());
            List<StockRankingItemDTO> allRankingItems = new ArrayList<>();
            String nextKey = "";
            String contYn = "N";
            boolean hasMore = true;
            int maxAttempts = 10; // 무한 루프 방지
            int attempts = 0;

            // 최대 100개 종목을 얻을 때까지 또는 더 이상 조회할 데이터가 없을 때까지 반복
            while (hasMore && allRankingItems.size() < 100 && attempts < maxAttempts) {
                attempts++;

                log.debug("연속 조회 시도 #{}: contYn={}, nextKey={}", attempts, contYn, nextKey);

                // 연속 조회 파라미터 설정
                request.setContYn(contYn);
                request.setNextKey(nextKey);

                // 요청 바디 생성
                String requestBody = buildRequestBody(request);

                // API 요청을 위한 헤더와 바디 설정
                String accessToken = kiwoomAuthService.getAccessToken();

                // 응답 받기
                Map<String, Object> apiResponse = makeApiRequest(
                        apiId, accessToken, requestBody, contYn, nextKey);

                // 응답 코드 확인
                int returnCode = (int) apiResponse.get("return_code");
                log.debug("API 응답 코드: {}, 메시지: {}", returnCode, apiResponse.get("return_msg"));

                if (returnCode != 0) {
                    log.error("키움 API 오류: {} - {}", returnCode, apiResponse.get("return_msg"));
                    break;
                }

                // 응답 데이터 처리
                StockRankingResponseDTO responseDTO = processResponse(apiResponse, request.getRankingType());

                // 종목 목록 누적
                if (responseDTO.getRankingItems() != null && !responseDTO.getRankingItems().isEmpty()) {
                    allRankingItems.addAll(responseDTO.getRankingItems());
                    log.debug("이번 응답에서 얻은 종목 수: {}, 누적 종목 수: {}",
                            responseDTO.getRankingItems().size(), allRankingItems.size());
                } else {
                    log.debug("추가 종목 데이터 없음");
                    break;
                }

                // 연속 조회 정보 업데이트
                hasMore = responseDTO.isHasNext();
                nextKey = responseDTO.getNextKey();
                contYn = hasMore ? "Y" : "N";

                if (!hasMore) {
                    log.debug("더 이상 조회할 데이터 없음");
                    break;
                }
            }

            // 최대 100개로 제한
            if (allRankingItems.size() > 100) {
                // subList를 사용하지 않고 새 ArrayList로 복사
                List<StockRankingItemDTO> limitedList = new ArrayList<>(allRankingItems.subList(0, 100));
                allRankingItems = limitedList;
                log.debug("목록을 100개로 제한했습니다.");
            }

            // 결과 응답 생성
            return StockRankingResponseDTO.builder()
                    .rankingType(request.getRankingType())
                    .rankingItems(allRankingItems)
                    .hasNext(false) // 명시적으로 false로 설정
                    .nextKey("") // 명시적으로 빈 문자열로 설정
                    .code(0)
                    .message("성공")
                    .build();

        } catch (CustomException e) {
            // 이미 CustomException으로 처리된 예외는 그대로 전파
            throw e;
        } catch (Exception e) {
            log.error("키움 API 호출 중 오류 발생: {}", e.getMessage(), e);
            throw new CustomException(StockErrorCode.API_REQUEST_FAILED, "순위 데이터 조회 실패: " + e.getMessage());
        }
    }

    /**
     * 순위 타입에 따른 API ID를 반환합니다.
     */
    private String getApiIdByRankingType(String rankingType) {
        if (rankingType == null) {
            return TRADE_VOLUME_API_ID; // 기본값: 당일거래량상위
        }

        switch (rankingType.toUpperCase()) {
            case "TRADE_AMOUNT":
                return TRADE_AMOUNT_API_ID;
            case "FLUCTUATION_RATE":
                return FLUCTUATION_RATE_API_ID;
            case "TRADE_VOLUME":
                return TRADE_VOLUME_API_ID;
            default:
                throw new CustomException(StockErrorCode.INVALID_RANKING_TYPE, "지원하지 않는 순위 타입입니다: " + rankingType);
        }
    }

    /**
     * 요청 바디 JSON을 생성합니다.
     */
    private String buildRequestBody(StockRankingRequestDTO request) {
        try {
            String rankingType = request.getRankingType().toUpperCase();

            if ("TRADE_AMOUNT".equals(rankingType)) {
                return String.format(
                        "{\"mrkt_tp\":\"%s\",\"mang_stk_incls\":\"%s\",\"stex_tp\":\"%s\"}",
                        request.getMarketType(),
                        request.getIncludeManagement(),
                        request.getExchangeType()
                );
            } else if ("FLUCTUATION_RATE".equals(rankingType)) {
                return String.format(
                        "{\"mrkt_tp\":\"%s\",\"sort_tp\":\"%s\",\"trde_qty_cnd\":\"%s\",\"stk_cnd\":\"%s\",\"crd_cnd\":\"%s\",\"updown_incls\":\"%s\",\"pric_cnd\":\"%s\",\"trde_prica_cnd\":\"%s\",\"stex_tp\":\"%s\"}",
                        request.getMarketType(),
                        request.getSortType(),
                        request.getTradeVolumeCondition(),
                        request.getStockCondition(),
                        request.getCreditCondition(),
                        request.getIncludeUpDownLimit(),
                        request.getPriceCondition(),
                        request.getTradeAmountCondition(),
                        request.getExchangeType()
                );
            } else if ("TRADE_VOLUME".equals(rankingType)) {
                return String.format(
                        "{\"mrkt_tp\":\"%s\",\"sort_tp\":\"%s\",\"mang_stk_incls\":\"%s\",\"crd_tp\":\"%s\",\"trde_qty_tp\":\"%s\",\"pric_tp\":\"%s\",\"trde_prica_tp\":\"%s\",\"mrkt_open_tp\":\"%s\",\"stex_tp\":\"%s\"}",
                        request.getMarketType(),
                        request.getSortType(),
                        request.getIncludeManagement(),
                        request.getCreditType(),
                        request.getTradeVolumeType(),
                        request.getPriceType(),
                        request.getTradeAmountType(),
                        request.getMarketOpenType(),
                        request.getExchangeType()
                );
            } else {
                throw new CustomException(StockErrorCode.INVALID_RANKING_TYPE, "지원하지 않는 순위 타입입니다: " + rankingType);
            }
        } catch (Exception e) {
            log.error("요청 바디 생성 중 오류 발생", e);
            throw new CustomException(StockErrorCode.API_REQUEST_FAILED, "요청 바디 생성 실패");
        }
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
     * API 응답을 StockRankingResponseDTO로 변환합니다.
     * StockData에 있는 종목 코드만 필터링하여 반환합니다.
     */
    private StockRankingResponseDTO processResponse(Map<String, Object> response, String rankingType) {
        try {
            int returnCode = (int) response.get("return_code");
            String returnMsg = (String) response.get("return_msg");

            if (returnCode != 0) {
                log.error("키움 API 오류: {} - {}", returnCode, returnMsg);
                return StockRankingResponseDTO.builder()
                        .code(returnCode)
                        .message(returnMsg)
                        .build();
            }

            // 연속조회 정보
            String contYn = (String) response.getOrDefault("_contYn", "N");
            String nextKey = (String) response.getOrDefault("_nextKey", "");
            boolean hasNext = "Y".equals(contYn);

            // 순위 타입에 따른 데이터 필드 결정
            String dataField = getDataFieldByRankingType(rankingType);
            List<Map<String, Object>> dataList = (List<Map<String, Object>>) response.get(dataField);

            List<StockRankingItemDTO> rankingItems = new ArrayList<>();

            if (dataList != null && !dataList.isEmpty()) {
                for (Map<String, Object> item : dataList) {
                    // 종목 코드 추출 - _AL과 같은 접미사가 있는 경우 제거하여 기본 코드만 추출
                    String fullStockCode = getString(item, "stk_cd");
                    String baseStockCode = fullStockCode.split("_")[0]; // _가 있으면 앞부분만 사용

                    // StockData 테이블에 존재하는 종목인지 확인
                    boolean exists = stockDataRepository.findByShortCode(baseStockCode).isPresent();

                    if (exists) {
                        // StockData에 있는 종목만 매핑하여 추가
                        StockRankingItemDTO itemDTO = mapToRankingItemDTO(item, rankingType);
                        if (itemDTO != null) {
                            // 원래 API가 반환한 코드가 아닌 DB에 있는 기본 코드로 설정
                            itemDTO.setStockCode(baseStockCode);
                            rankingItems.add(itemDTO);
                        }
                    } else {
//                        log.debug("StockData에 존재하지 않는 종목 코드 무시: {}", fullStockCode);
                    }
                }
            }

            log.debug("API 응답 처리 완료: 종목 수={}, hasNext={}, nextKey={}",
                    rankingItems.size(), hasNext, nextKey);

            // 중간 처리 결과 반환 - 실제 최종 응답은 fetchStockRanking에서 처리
            return StockRankingResponseDTO.builder()
                    .rankingType(rankingType)
                    .rankingItems(rankingItems)
                    .hasNext(hasNext)
                    .nextKey(nextKey)
                    .code(returnCode)
                    .message(returnMsg)
                    .build();

        } catch (Exception e) {
            log.error("응답 처리 중 오류: {}", e.getMessage(), e);
            throw new CustomException(StockErrorCode.API_REQUEST_FAILED, "순위 데이터 처리 중 오류 발생: " + e.getMessage());
        }
    }

    /**
     * 순위 타입에 따른 데이터 필드명을 반환합니다.
     */
    private String getDataFieldByRankingType(String rankingType) {
        if (rankingType == null) {
            return "tdy_trde_qty_upper"; // 기본값: 당일거래량상위
        }

        switch (rankingType.toUpperCase()) {
            case "TRADE_AMOUNT":
                return "trde_prica_upper";
            case "FLUCTUATION_RATE":
                return "pred_pre_flu_rt_upper";
            case "TRADE_VOLUME":
                return "tdy_trde_qty_upper";
            default:
                throw new CustomException(StockErrorCode.INVALID_RANKING_TYPE, "지원하지 않는 순위 타입입니다: " + rankingType);
        }
    }

    /**
     * API 응답 데이터를 StockRankingItemDTO로 변환합니다.
     */
    private StockRankingItemDTO mapToRankingItemDTO(Map<String, Object> item, String rankingType) {
        try {
            StockRankingItemDTO.StockRankingItemDTOBuilder builder = StockRankingItemDTO.builder();

            // 공통 필드 설정
            builder.stockCode(getString(item, "stk_cd"));
            builder.stockName(getString(item, "stk_nm"));
            builder.currentPrice(getString(item, "cur_prc"));
            builder.previousDayCompareSign(getString(item, "pred_pre_sig"));
            builder.previousDayCompare(getString(item, "pred_pre"));
            builder.fluctuationRate(getString(item, "flu_rt"));

            // 순위 타입별 추가 필드 설정
            if ("TRADE_AMOUNT".equals(rankingType.toUpperCase())) {
                // 거래대금상위
                builder.currentRank(getString(item, "now_rank"));
                builder.previousRank(getString(item, "pred_rank"));
                builder.currentTradeVolume(getString(item, "now_trde_qty"));
                builder.previousTradeVolume(getString(item, "pred_trde_qty"));
                builder.tradeAmount(getString(item, "trde_prica"));
            } else if ("FLUCTUATION_RATE".equals(rankingType.toUpperCase())) {
                // 전일대비등락률상위
                builder.sellRemaining(getString(item, "sel_req"));
                builder.buyRemaining(getString(item, "buy_req"));
                builder.currentTradeVolume(getString(item, "now_trde_qty"));
                builder.contractStrength(getString(item, "cntr_str"));
            } else if ("TRADE_VOLUME".equals(rankingType.toUpperCase())) {
                // 당일거래량상위
                builder.tradeVolume(getString(item, "trde_qty"));
                builder.previousRatio(getString(item, "pred_rt"));
                builder.tradeTurnoverRate(getString(item, "trde_tern_rt"));
                builder.tradeAmount(getString(item, "trde_amt"));
            }

            return builder.build();
        } catch (Exception e) {
            log.error("순위 데이터 매핑 중 오류: {}", e.getMessage(), e);
            return null;
        }
    }

    /**
     * Map에서 키에 해당하는 값을 문자열로 반환합니다.
     */
    private String getString(Map<String, Object> map, String key) {
        Object value = map.get(key);
        return value != null ? value.toString() : "";
    }

    // 캐시 관련 메서드 - 각 순위 타입별 캐시 키 생성

    /**
     * 거래대금 상위 캐시 키를 생성합니다.
     */
    private String buildTradeAmountCacheKey(String marketType, String includeManagement) {
        return REDIS_RANKING_KEY_PREFIX + "TRADE_AMOUNT:" + marketType + ":" + includeManagement;
    }

    /**
     * 전일대비등락률 상위 캐시 키를 생성합니다.
     */
    private String buildFluctuationRateCacheKey(String marketType, String sortType) {
        return REDIS_RANKING_KEY_PREFIX + "FLUCTUATION_RATE:" + marketType + ":" + sortType;
    }

    /**
     * 당일거래량 상위 캐시 키를 생성합니다.
     */
    private String buildTradeVolumeCacheKey(String marketType, String sortType) {
        return REDIS_RANKING_KEY_PREFIX + "TRADE_VOLUME:" + marketType + ":" + sortType;
    }

    /**
     * Redis 캐시에서 데이터를 조회합니다.
     */
    @SuppressWarnings("unchecked")
    private StockRankingResponseDTO getCachedResponse(String cacheKey) {
        try {
            StockRankingResponseDTO response = (StockRankingResponseDTO) redisTemplate.opsForValue().get(cacheKey);
            if (response != null && response.getRankingItems() != null) {
                log.debug("Redis 캐시 조회 성공: {}, 항목 수: {}", cacheKey, response.getRankingItems().size());
            }
            return response;
        } catch (Exception e) {
            log.warn("Redis 캐시 조회 실패: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Redis 캐시에 데이터를 저장합니다.
     */
    private void cacheResponse(String cacheKey, StockRankingResponseDTO response) {
        try {
            // 캐시에 저장하기 전에 List 타입이 직렬화 가능한지 확인
            if (response.getRankingItems() != null && !(response.getRankingItems() instanceof ArrayList)) {
                response.setRankingItems(new ArrayList<>(response.getRankingItems()));
            }

            // hasNext와 nextKey 초기화 (불필요한 필드)
            response.setHasNext(false);
            response.setNextKey("");

            redisTemplate.opsForValue().set(
                    cacheKey,
                    response,
                    CACHE_TTL_SECONDS,
                    TimeUnit.SECONDS
            );
            log.debug("Redis 캐시 저장 성공: {}, 항목 수: {}", cacheKey,
                    response.getRankingItems() != null ? response.getRankingItems().size() : 0);
        } catch (Exception e) {
            log.warn("Redis 캐시 저장 실패: {}", e.getMessage(), e);
        }
    }
}