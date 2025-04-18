package realClassOne.chickenStock.stock.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Service
@Slf4j
@RequiredArgsConstructor
public class StockSearchService {

    private final KiwoomAuthService authService;
    private final ObjectMapper objectMapper;

    // 캐싱을 위한 맵 (이미 조회한 종목명 → 종목코드)
    private final Map<String, String> stockNameToCodeCache = new ConcurrentHashMap<>();

    /**
     * 종목명으로 종목코드 검색
     * @param stockName 종목명
     * @return 종목코드 (없으면 null)
     */
    public String findStockCodeByName(String stockName) {
        // 캐시에 있으면 바로 반환
        if (stockNameToCodeCache.containsKey(stockName)) {
            return stockNameToCodeCache.get(stockName);
        }

        try {
            // 키움증권 종목검색 API 호출
            String result = searchStockByKeyword(stockName);
            JsonNode jsonNode = objectMapper.readTree(result);

            // 응답 파싱
            if (jsonNode.has("rt_cd") && "0".equals(jsonNode.get("rt_cd").asText())) {
                JsonNode output = jsonNode.get("output");

                if (output != null && output.isArray() && output.size() > 0) {
                    for (JsonNode item : output) {
                        String itemName = item.get("prdt_name").asText();
                        String itemCode = item.get("code").asText();

                        // 정확히 일치하는 종목 찾기
                        if (itemName.equals(stockName)) {
                            // 캐시에 저장
                            stockNameToCodeCache.put(stockName, itemCode);
                            return itemCode;
                        }
                    }

                    // 정확히 일치하는 종목이 없으면 첫 번째 항목 선택
                    String firstMatchCode = output.get(0).get("code").asText();
                    String firstMatchName = output.get(0).get("prdt_name").asText();
                    log.info("종목명 '{}' 정확히 일치하는 종목 없음, 가장 유사한 '{}' 선택", stockName, firstMatchName);

                    // 캐시에 저장
                    stockNameToCodeCache.put(stockName, firstMatchCode);
                    return firstMatchCode;
                }
            }

            log.warn("종목명 '{}' 검색 결과 없음", stockName);
            return null;
        } catch (Exception e) {
            log.error("종목 검색 중 오류 발생: {}", stockName, e);
            return null;
        }
    }

    /**
     * 키움증권 API를 통해 종목 검색
     */
    private String searchStockByKeyword(String keyword) throws Exception {
        String token = authService.getAccessToken();
        String apiUrl = "https://api.kiwoom.com/uapi/domestic-stock/v1/quotations/search";
        // https://api.kiwoom.com/uapi/domestic-stock/v1/quotations/search-stock-info

        WebClient webClient = WebClient.builder()
                .baseUrl(apiUrl)
                .defaultHeader("Authorization", "Bearer " + token)
                .defaultHeader("appkey", authService.getAppKey())
                .defaultHeader("appsecret", authService.getSecretKey())
                .defaultHeader("tr_id", "CTPF1002R") // 종목검색 TR 코드
                .build();

        // 검색 파라미터 설정
        Map<String, String> params = new HashMap<>();
        params.put("search_text", keyword);
        params.put("bld_gb", "000"); // 전체

        return webClient.get()
                .uri(uriBuilder -> {
                    params.forEach(uriBuilder::queryParam);
                    return uriBuilder.build();
                })
                .retrieve()
                .bodyToMono(String.class)
                .block();
    }

    /**
     * 캐시 초기화 (필요시)
     */
    public void clearCache() {
        stockNameToCodeCache.clear();
    }
}