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

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Service
@Slf4j
@RequiredArgsConstructor
public class KiwoomAuthService {

    @Value("${kiwoom.api.url}")
    private String apiUrl;

    @Value("${kiwoom.api.appkey}")
    private String appKey;

    @Value("${kiwoom.api.secretkey}")
    private String secretKey;

    private String accessToken;
    private LocalDateTime tokenExpiryTime;

    private final ObjectMapper objectMapper;

    public String getAccessToken() {
        if (accessToken == null || isTokenExpired()) {
            fetchNewAccessToken();
        }
        return accessToken;
    }

    private boolean isTokenExpired() {
        return tokenExpiryTime == null || LocalDateTime.now().isAfter(tokenExpiryTime);
    }

    public void fetchNewAccessToken() {
        try {
            String tokenUrl = apiUrl + "/oauth2/token";

            WebClient webClient = WebClient.builder()
                    .baseUrl(tokenUrl)
                    .defaultHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                    .build();

            String requestBody = String.format(
                    "{\"grant_type\":\"client_credentials\",\"appkey\":\"%s\",\"secretkey\":\"%s\"}",
                    appKey, secretKey);

            String response = webClient.post()
                    .bodyValue(requestBody)
                    .retrieve()
                    .bodyToMono(String.class)
                    .block();

            JsonNode jsonNode = objectMapper.readTree(response);
            accessToken = jsonNode.get("token").asText();

            // 토큰 만료 시간 설정 (expires_dt는 YYYYMMDDHHmmss 형식)
            String expiresStr = jsonNode.get("expires_dt").asText();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
            tokenExpiryTime = LocalDateTime.parse(expiresStr, formatter);

            log.info("키움증권 토큰 발급 성공: 만료시간 = {}", tokenExpiryTime);
        } catch (Exception e) {
            log.error("키움증권 토큰 발급 실패", e);
            throw new RuntimeException("키움증권 API 인증 실패", e);
        }
    }
}