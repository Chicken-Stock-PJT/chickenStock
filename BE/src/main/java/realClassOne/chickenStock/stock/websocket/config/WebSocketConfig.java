package realClassOne.chickenStock.stock.websocket.config;

import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;
import realClassOne.chickenStock.stock.websocket.handler.PortfolioWebSocketHandler;
import realClassOne.chickenStock.stock.websocket.handler.StockWebSocketHandler;

@Configuration
@EnableWebSocket
@RequiredArgsConstructor
public class WebSocketConfig implements WebSocketConfigurer {

    private final StockWebSocketHandler stockWebSocketHandler;
    private final PortfolioWebSocketHandler portfolioWebSocketHandler;

    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {

        // 기존 주식 실시간 데이터 웹소켓
        registry.addHandler(stockWebSocketHandler, "/ws/stock")
                .setAllowedOrigins("*"); // 실제 환경에서는 보안을 위해 특정 오리진만 허용하는 것 추천됨.

        // 포트폴리오 실시간 데이터 웹소켓
        registry.addHandler(portfolioWebSocketHandler, "/ws/portfolio")
                .setAllowedOrigins("*");
    }
}