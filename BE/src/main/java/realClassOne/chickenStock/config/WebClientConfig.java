package realClassOne.chickenStock.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.client.ExchangeStrategies;
import org.springframework.web.reactive.function.client.WebClient;

@Configuration
public class WebClientConfig {

    @Value("${kiwoom.api.url}")
    private String kiwoomApiUrl;

    @Bean
    public WebClient kiwoomWebClient() {
        // 메모리 제한 증가 (16MB)
        ExchangeStrategies strategies = ExchangeStrategies.builder()
                .codecs(configurer -> configurer
                        .defaultCodecs()
                        .maxInMemorySize(16 * 1024 * 1024))
                .build();

        return WebClient.builder()
                .baseUrl(kiwoomApiUrl)
                .exchangeStrategies(strategies)
                .build();
    }
}