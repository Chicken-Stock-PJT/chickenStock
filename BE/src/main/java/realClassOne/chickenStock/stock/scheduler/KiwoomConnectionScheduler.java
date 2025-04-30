package realClassOne.chickenStock.stock.scheduler;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

@Service
@Slf4j
@RequiredArgsConstructor
public class KiwoomConnectionScheduler {

    private final KiwoomWebSocketClient kiwoomWebSocketClient;

    @Scheduled(fixedRate = 60000000) // 1분마다 연결 상태 체크
    public void checkConnection() {
        if (!kiwoomWebSocketClient.isConnected()) {
            log.info("키움증권 WebSocket 연결이 끊어져 있습니다. 재연결 시도...");
            kiwoomWebSocketClient.connect();
        }
    }
}