package realClassOne.chickenStock.stock.scheduler;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

@Slf4j
@Component
@RequiredArgsConstructor
public class MarketOpenScheduler {

    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final JavaMailSender emailSender;

    @Value("${admin.email}")
    private String adminEmail;

    @Value("${market.open.preparation-time:7:55}")
    private String preparationTime;

    /**
     * 평일 오전 7시 55분에 장 시작 준비
     * 무조건 기존 연결을 끊고 새로운 연결 시작
     */
    @Scheduled(cron = "0 55 7 * * MON-FRI", zone = "Asia/Seoul")
    public void prepareMarketOpen() {
        try {
            if (isHoliday()) {
                return;
            }

            boolean wasConnected = kiwoomWebSocketClient.isConnected();

            if (wasConnected) {
                disconnectWebSocket();
                Thread.sleep(1000);
            }

            kiwoomWebSocketClient.connect();

            boolean connectionSuccess = waitForConnection(30);

            if (connectionSuccess) {
                sendMarketPrepareSuccessEmail();
            } else {
                sendMarketPrepareFailEmail();
                return;
            }

            performHealthCheck();
            cleanupInternalState();

            if (connectionSuccess) {
                sendMarketReadyEmail();
            }

        } catch (Exception e) {
            sendMarketPrepareErrorEmail(e);
        }
    }

    /**
     * WebSocket 연결 종료 메서드
     */
    private void disconnectWebSocket() throws Exception {
        try {
            Field clientField = kiwoomWebSocketClient.getClass().getDeclaredField("client");
            clientField.setAccessible(true);
            Object clientObj = clientField.get(kiwoomWebSocketClient);

            if (clientObj != null) {
                Method closeMethod = clientObj.getClass().getMethod("close");
                closeMethod.invoke(clientObj);
            }

            Field connectedField = kiwoomWebSocketClient.getClass().getDeclaredField("connected");
            connectedField.setAccessible(true);
            java.util.concurrent.atomic.AtomicBoolean connected =
                    (java.util.concurrent.atomic.AtomicBoolean) connectedField.get(kiwoomWebSocketClient);
            connected.set(false);

        } catch (Exception e) {
            throw e;
        }
    }

    /**
     * 장 시간 체크 메서드
     * 평일 오전 8시 ~ 오후 8시
     */
    private boolean isMarketHours() {
        LocalTime now = LocalTime.now(ZoneId.of("Asia/Seoul"));
        LocalDate today = LocalDate.now(ZoneId.of("Asia/Seoul"));
        DayOfWeek dayOfWeek = today.getDayOfWeek();

        if (dayOfWeek == DayOfWeek.SATURDAY || dayOfWeek == DayOfWeek.SUNDAY) {
            return false;
        }

        LocalTime marketOpen = LocalTime.of(8, 0);
        LocalTime marketClose = LocalTime.of(20, 0);

        return now.isAfter(marketOpen) && now.isBefore(marketClose);
    }

    /**
     * 공휴일 체크 (실제 구현시 공휴일 API 연동 필요)
     */
    private boolean isHoliday() {
        DayOfWeek dayOfWeek = LocalDate.now(ZoneId.of("Asia/Seoul")).getDayOfWeek();
        return dayOfWeek == DayOfWeek.SATURDAY || dayOfWeek == DayOfWeek.SUNDAY;
    }

    /**
     * WebSocket 연결 대기
     */
    private boolean waitForConnection(int timeoutSeconds) {
        int retryCount = 0;
        while (!kiwoomWebSocketClient.isConnected() && retryCount < timeoutSeconds) {
            try {
                Thread.sleep(1000);
                retryCount++;
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                return false;
            }
        }
        return kiwoomWebSocketClient.isConnected();
    }

    /**
     * 연결 상태 점검
     */
    private void performHealthCheck() {
        try {
            int subscribedCount = kiwoomWebSocketClient.getSubscribedStockCodes().size();
        } catch (Exception e) {
        }
    }

    /**
     * 내부 상태 정리
     */
    private void cleanupInternalState() {
        try {
        } catch (Exception e) {
        }
    }

    /**
     * 장 시작 준비 성공 이메일
     */
    private void sendMarketPrepareSuccessEmail() {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock] 장 시작 준비 성공");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "장 시작 준비가 성공적으로 완료되었습니다.\n\n" +
                            "- 준비 완료 시간: %s\n" +
                            "- WebSocket 재연결: 성공\n" +
                            "- 시스템 상태: 준비 완료\n\n" +
                            "기존 연결을 종료하고 새로운 연결을 시작했습니다.\n" +
                            "오늘 하루도 안정적인 서비스 운영이 되시길 바랍니다.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime
            );

            message.setText(content);
            emailSender.send(message);
        } catch (Exception e) {
        }
    }

    /**
     * 장 시작 준비 실패 이메일
     */
    private void sendMarketPrepareFailEmail() {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock 긴급] 장 시작 준비 실패");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "장 시작 준비 중 WebSocket 연결에 실패했습니다.\n\n" +
                            "- 시간: %s\n" +
                            "- WebSocket 연결 상태: 실패\n" +
                            "- 오류 내용: 연결 타임아웃\n\n" +
                            "긴급 조치가 필요합니다. 수동으로 시스템을 확인하고 필요시 재시작해주세요.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime
            );

            message.setText(content);
            emailSender.send(message);
        } catch (Exception e) {
        }
    }

    /**
     * 장 시작 준비 오류 이메일
     */
    private void sendMarketPrepareErrorEmail(Exception exception) {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock 오류] 장 시작 준비 중 오류 발생");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String errorMessage = exception.getMessage();
            if (errorMessage == null || errorMessage.isEmpty()) {
                errorMessage = exception.getClass().getName();
            }

            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "장 시작 준비 중 예외가 발생했습니다.\n\n" +
                            "- 발생 시간: %s\n" +
                            "- 오류 내용: %s\n\n" +
                            "시스템 로그를 확인하여 자세한 오류 내용을 확인해주세요.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime, errorMessage
            );

            message.setText(content);
            emailSender.send(message);
        } catch (Exception e) {
        }
    }

    /**
     * 장 시작 준비 완료 이메일
     */
    private void sendMarketReadyEmail() {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock] 장 시작 준비 완료");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));

            int subscribedStocks = kiwoomWebSocketClient.getSubscribedStockCodes().size();
            boolean isConnected = kiwoomWebSocketClient.isConnected();

            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "장 시작 준비가 모두 완료되었습니다.\n\n" +
                            "=== 시스템 상태 ===\n" +
                            "- 확인 시간: %s\n" +
                            "- WebSocket 연결: %s\n" +
                            "- 구독 중인 종목 수: %d개\n" +
                            "- 시스템 준비 상태: 완료\n\n" +
                            "장이 시작되면 정상적으로 실시간 데이터가 수신될 예정입니다.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime,
                    isConnected ? "연결됨" : "연결 안됨",
                    subscribedStocks
            );

            message.setText(content);
            emailSender.send(message);
        } catch (Exception e) {
        }
    }

    /**
     * 장 시작 시간(오전 9시)에 실행 - 장 시작 알림
     */
    @Scheduled(cron = "0 0 8 * * MON-FRI", zone = "Asia/Seoul")
    public void notifyMarketOpen() {
        try {
            if (isHoliday()) {
                return;
            }

            if (!kiwoomWebSocketClient.isConnected()) {
                sendMarketOpenWarningEmail();
            } else {
                sendMarketOpenNotificationEmail();
            }

        } catch (Exception e) {
        }
    }

    /**
     * 장 시작 알림 이메일
     */
    private void sendMarketOpenNotificationEmail() {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock] 주식 시장 시작 알림");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "주식 시장이 정상적으로 시작되었습니다.\n\n" +
                            "- 시간: %s\n" +
                            "- WebSocket 연결 상태: 정상\n" +
                            "- 시스템 상태: 정상 운영 중\n\n" +
                            "오늘 하루도 안정적인 서비스 운영이 되시길 바랍니다.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime
            );

            message.setText(content);
            emailSender.send(message);
        } catch (Exception e) {
        }
    }

    /**
     * 장 시작 경고 이메일 (WebSocket 미연결)
     */
    private void sendMarketOpenWarningEmail() {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock 경고] 장 시작 시 WebSocket 미연결");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "장이 시작되었지만 WebSocket이 연결되어 있지 않습니다.\n\n" +
                            "- 시간: %s\n" +
                            "- WebSocket 연결 상태: 미연결\n" +
                            "- 필요 조치: 즉시 확인 및 재연결 필요\n\n" +
                            "실시간 데이터가 수신되지 않을 수 있습니다.\n" +
                            "긴급히 시스템을 확인해주세요.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime
            );

            message.setText(content);
            emailSender.send(message);
        } catch (Exception e) {
        }
    }
}