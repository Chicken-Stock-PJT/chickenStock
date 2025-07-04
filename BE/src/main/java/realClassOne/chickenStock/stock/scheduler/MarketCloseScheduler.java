package realClassOne.chickenStock.stock.scheduler;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.stock.entity.PendingOrder;
import realClassOne.chickenStock.stock.entity.TradeHistory;
import realClassOne.chickenStock.stock.repository.PendingOrderRepository;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

@Slf4j
@Component
@RequiredArgsConstructor
public class MarketCloseScheduler {

    private final PendingOrderRepository pendingOrderRepository;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final JavaMailSender emailSender;
    private final MemberRepository memberRepository; // 회원 정보 조회를 위해 추가

    @Value("${market.close.batch-size:100}")
    private int batchSize;

    @Value("${market.close.delay-ms:500}")
    private int delayMs;

    @Value("${admin.email}")
    private String adminEmail;

    /**
     * 매일 저녁 8시에 실행되는 스케줄러
     * 모든 지정가 주문을 배치 단위로 취소하고 로그를 남깁니다.
     */
    @Scheduled(cron = "0 0 20 * * *", zone = "Asia/Seoul")
    public void cancelAllPendingOrders() {
        log.info("시장 마감 프로세스 실행: 모든 지정가 주문 취소 시작");

        int pageNumber = 0;
        int totalCanceled = 0;
        boolean hasNext = true;

        try {
            // 서버 부하 방지를 위해 페이징 처리하며 배치 단위로 취소 처리
            while (hasNext) {
                // 페이지 단위로 주문 조회
                Pageable pageable = PageRequest.of(pageNumber, batchSize);
                Page<PendingOrder> pendingOrderPage = pendingOrderRepository.findPageByStatus(
                        PendingOrder.OrderStatus.PENDING, pageable);

                List<PendingOrder> pendingOrders = pendingOrderPage.getContent();

                if (pendingOrders.isEmpty()) {
                    hasNext = false;
                    continue;
                }

                log.info("배치 {} 처리 중: {}개 주문 취소 시작", pageNumber + 1, pendingOrders.size());

                // 현재 배치의 주문들을 취소 처리
                int batchCancelCount = processCancelBatch(pendingOrders);
                totalCanceled += batchCancelCount;

                log.info("배치 {} 완료: {}개 주문 취소됨 (총 {}개)",
                        pageNumber + 1, batchCancelCount, totalCanceled);

                // 다음 페이지로 이동
                pageNumber++;
                hasNext = pendingOrderPage.hasNext();

                // 서버 부하 방지를 위한 잠시 대기
                if (hasNext) {
                    try {
                        TimeUnit.MILLISECONDS.sleep(delayMs);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        log.warn("배치 처리 중 인터럽트 발생", e);
                    }
                }
            }

            log.info("지정가 주문 취소 프로세스 완료: 총 {}개 취소됨, 처리 시간: {}",
                    totalCanceled, LocalDateTime.now());
        } catch (Exception e) {
            log.error("지정가 주문 취소 프로세스 중 오류 발생", e);
        }
    }

    /**
     * 지정가 주문 배치를 취소 처리하는 메서드
     * 각 배치는 별도의 트랜잭션으로 처리되어 실패 시에도 다음 배치 처리 가능
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public int processCancelBatch(List<PendingOrder> pendingOrders) {
        int cancelCount = 0;
        List<String> stockCodes = new ArrayList<>();
        // 문제가 발생한 주문 정보를 저장할 리스트
        List<OrderProblemInfo> problemOrders = new ArrayList<>();

        for (PendingOrder order : pendingOrders) {
            try {
                // 종목 코드 수집 (구독 해제용)
                String stockCode = order.getStockData().getShortCode();
                if (!stockCodes.contains(stockCode)) {
                    stockCodes.add(stockCode);
                }

                // 주문 상태를 취소로 변경
                order.cancel();
                pendingOrderRepository.save(order);

                // 매수 주문인 경우 예약된 금액 환불
                if (order.getOrderType() == TradeHistory.TradeType.BUY) {
                    // 예약된 수수료 포함하여 환불
                    Long reservedFee = order.getReservedFee() != null ? order.getReservedFee() : 0L;
                    Long refundAmount = order.getTargetPrice() * order.getQuantity() + reservedFee;

                    try {
                        // 별도 트랜잭션으로 환불 처리
                        refundMoneyOnOrderCancellation(order.getMember(), refundAmount);

                        log.info("취소된 매수 주문 환불 처리 완료: 주문ID={}, 회원ID={}, 금액={}원(수수료 {}원 포함)",
                                order.getOrderId(), order.getMember().getMemberId(), refundAmount, reservedFee);
                    } catch (Exception e) {
                        log.error("매수 주문 취소 후 환불 처리 중 오류 발생: 주문ID={}, 회원ID={}, 금액={}원, 오류={}",
                                order.getOrderId(), order.getMember().getMemberId(), refundAmount, e.getMessage());

                        // 문제 정보 수집
                        OrderProblemInfo problemInfo = new OrderProblemInfo(
                                order.getMember().getMemberId(),
                                order.getMember().getEmail(),
                                order.getStockData().getShortCode(),
                                order.getStockData().getShortName(),
                                order.getQuantity(),
                                order.getTargetPrice(),
                                "환불 처리 실패: " + e.getMessage()
                        );
                        problemOrders.add(problemInfo);

                        // 개별 환불 실패 알림 발송
                        sendRefundFailureEmail(order.getMember().getMemberId(), refundAmount, e);
                    }
                }

                // 체결된 주문의 구독 목적 해제
                String pendingOrderPurpose = "PENDING_ORDER_" + order.getOrderId();
                kiwoomWebSocketClient.unsubscribeStockForPurpose(stockCode, pendingOrderPurpose);

                cancelCount++;
            } catch (Exception e) {
                log.error("지정가 주문 취소 실패: 주문ID={}, 오류={}", order.getOrderId(), e.getMessage());

                // 문제 정보 수집
                try {
                    OrderProblemInfo problemInfo = new OrderProblemInfo(
                            order.getMember().getMemberId(),
                            order.getMember().getEmail(),
                            order.getStockData().getShortCode(),
                            order.getStockData().getShortName(),
                            order.getQuantity(),
                            order.getTargetPrice(),
                            "주문 취소 실패: " + e.getMessage()
                    );
                    problemOrders.add(problemInfo);
                } catch (Exception ex) {
                    log.error("문제 주문 정보 수집 중 오류 발생", ex);
                }

                // 실패해도 다음 주문 처리 계속
            }
        }

        // 문제가 발생한 주문이 있으면 취합하여 이메일 발송
        if (!problemOrders.isEmpty()) {
            sendOrderProblemSummaryEmail(problemOrders);
        }

        return cancelCount;
    }

    /**
     * 문제 주문 정보를 담는 내부 클래스
     */
    private static class OrderProblemInfo {
        final Long memberId;
        final String memberEmail;
        final String stockCode;
        final String stockName;
        final Integer quantity;
        final Long targetPrice;
        final String errorMessage;

        public OrderProblemInfo(Long memberId, String memberEmail, String stockCode, String stockName,
                                Integer quantity, Long targetPrice, String errorMessage) {
            this.memberId = memberId;
            this.memberEmail = memberEmail;
            this.stockCode = stockCode;
            this.stockName = stockName;
            this.quantity = quantity;
            this.targetPrice = targetPrice;
            this.errorMessage = errorMessage;
        }
    }

    /**
     * 여러 문제 주문에 대한 취합 이메일 발송
     */
    private void sendOrderProblemSummaryEmail(List<OrderProblemInfo> problemOrders) {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock 긴급] 지정가 주문 취소 문제 발생 요약");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            StringBuilder contentBuilder = new StringBuilder();
            contentBuilder.append(String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "시장 마감 시 지정가 주문 취소 중 다음 문제가 발생했습니다.\n\n" +
                            "- 발생 시간: %s\n" +
                            "- 문제 발생 주문 수: %d개\n\n" +
                            "===== 문제 주문 상세 정보 =====\n\n",
                    dateTime, problemOrders.size()
            ));

            for (int i = 0; i < problemOrders.size(); i++) {
                OrderProblemInfo order = problemOrders.get(i);
                contentBuilder.append(String.format(
                        "문제 #%d\n" +
                                "- 회원 ID: %d\n" +
                                "- 회원 이메일: %s\n" +
                                "- 종목 코드: %s\n" +
                                "- 종목 이름: %s\n" +
                                "- 주문 수량: %d주\n" +
                                "- 주문 가격: %d원\n" +
                                "- 오류 내용: %s\n\n",
                        i + 1, order.memberId, order.memberEmail, order.stockCode, order.stockName,
                        order.quantity, order.targetPrice, order.errorMessage
                ));
            }

            contentBuilder.append(
                    "위 문제들에 대한 수동 확인 및 조치가 필요합니다.\n" +
                            "시스템 로그를 확인하여 더 자세한 오류 내용을 확인해주세요.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템"
            );

            message.setText(contentBuilder.toString());
            emailSender.send(message);
            log.info("지정가 주문 취소 문제 요약 이메일 발송 완료 ({}개 문제)", problemOrders.size());
        } catch (Exception e) {
            log.error("지정가 주문 취소 문제 요약 이메일 발송 실패", e);
        }
    }

    /**
     * 주문 취소 시 금액 환불 처리 메서드
     * @param member 회원 정보
     * @param amount 환불 금액
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void refundMoneyOnOrderCancellation(Member member, Long amount) {
        try {
            // 동시성 제어를 위한 락 획득
            ReentrantLock memberLock = new ReentrantLock();
            memberLock.lock();

            try {
                // 최신 회원 정보 조회
                Member freshMember = memberRepository.findById(member.getMemberId())
                        .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

                // 금액 환불
                freshMember.addMemberMoney(amount);
                memberRepository.save(freshMember);
                log.info("매수 주문 취소로 인한 금액 환불: 회원ID={}, 금액={}원", member.getMemberId(), amount);
            } finally {
                memberLock.unlock();
            }
        } catch (Exception e) {
            log.error("주문 취소 후 금액 환불 처리 중 오류 발생", e);
            // 환불 실패 시 관리자에게 알림
            sendRefundFailureEmail(member.getMemberId(), amount, e);
        }
    }

    /**
     * 매일 저녁 8시 5분에 실행되는 스케줄러
     * 모든 종목 구독을 배치 단위로 해제하고 로그를 남깁니다.
     * 구독 취소 상태에 따라 이메일 발송
     */
    @Scheduled(cron = "0 5 20 * * *", zone = "Asia/Seoul")
    public void unsubscribeAllStocks() {
        log.info("시장 마감 프로세스 실행: 모든 종목 구독 해제 시작");
        boolean isSuccessful = true; // 전체 과정 성공 여부
        List<String> allFailedStocks = new ArrayList<>(); // 모든 실패한 종목 추적

        try {
            // 현재 구독 중인 모든 종목 코드 가져오기
            Set<String> subscribedStocks = kiwoomWebSocketClient.getSubscribedStockCodes();
            List<String> stockCodeList = new ArrayList<>(subscribedStocks);
            int totalSize = stockCodeList.size();

            if (totalSize == 0) {
                log.info("구독 중인 종목이 없습니다. 프로세스 완료.");
                // 구독 중인 종목이 없을 때도 성공 메일 발송
                sendSuccessEmail(0);
                return;
            }

            log.info("총 {}개 종목 구독 해제 시작", totalSize);
            int successCount = 0;
            List<String> failedStocks = new ArrayList<>();

            // 배치 단위로 처리
            for (int i = 0; i < totalSize; i += batchSize) {
                int endIndex = Math.min(i + batchSize, totalSize);
                List<String> batchStocks = stockCodeList.subList(i, endIndex);

                log.info("구독 해제 배치 처리 중: {}-{}/{} ({}개)",
                        i + 1, endIndex, totalSize, batchStocks.size());

                // 배치 처리 및 실패한 종목 수집
                List<String> batchFailedStocks = processUnsubscribeBatch(batchStocks);
                failedStocks.addAll(batchFailedStocks);
                successCount += (batchStocks.size() - batchFailedStocks.size());

                // 서버 부하 방지를 위한 잠시 대기
                if (endIndex < totalSize) {
                    try {
                        TimeUnit.MILLISECONDS.sleep(delayMs);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        log.warn("배치 처리 중 인터럽트 발생", e);
                    }
                }
            }

            // 실패한 종목이 있으면 재시도
            if (!failedStocks.isEmpty()) {
                log.warn("{}개 종목 구독 해제 실패, 재시도 시작", failedStocks.size());
                allFailedStocks.addAll(failedStocks); // 실패 목록 추적
                List<String> retryFailedStocks = retryUnsubscribe(failedStocks, 3); // 최대 3회 재시도
                allFailedStocks = retryFailedStocks; // 최종 실패 목록으로 업데이트
            }

            // 최종 상태 확인
            Set<String> remainingSubscriptions = kiwoomWebSocketClient.getSubscribedStockCodes();
            if (!remainingSubscriptions.isEmpty()) {
                log.error("모든 재시도 후에도 {}개 종목 구독 해제 실패", remainingSubscriptions.size());
                isSuccessful = false;
                allFailedStocks = new ArrayList<>(remainingSubscriptions); // 최종 실패 목록으로 업데이트
                // 심각한 오류로 관리자에게 알림 발송
                sendAlertToAdmin(remainingSubscriptions);
            } else {
                log.info("종목 구독 해제 프로세스 완료: 총 {}개 해제됨, 처리 시간: {}",
                        successCount, LocalDateTime.now());
            }

            // 전체 결과에 따라 이메일 발송
            if (isSuccessful) {
                sendSuccessEmail(totalSize);
            } else {
                sendFailureEmail(totalSize, allFailedStocks);
            }

        } catch (Exception e) {
            log.error("종목 구독 해제 프로세스 중 오류 발생", e);
            sendErrorEmail(e);
        }
    }

    /**
     * 종목 구독 해제 배치를 처리하고 실패한 종목 코드 반환
     */
    private List<String> processUnsubscribeBatch(List<String> stockCodes) {
        List<String> failedStocks = new ArrayList<>();

        for (String stockCode : stockCodes) {
            try {
                // 종목 구독 해제
                boolean success = kiwoomWebSocketClient.unsubscribeStock(stockCode);
                if (!success) {
                    log.warn("종목 구독 해제 실패: {}", stockCode);
                    failedStocks.add(stockCode);
                }
            } catch (Exception e) {
                log.error("종목 구독 해제 중 예외 발생: {}, 오류={}", stockCode, e.getMessage());
                failedStocks.add(stockCode);
            }
        }

        return failedStocks;
    }

    /**
     * 실패한 종목에 대한 구독 해제 재시도
     * @param failedStocks 실패한 종목 코드 목록
     * @param maxRetries 최대 재시도 횟수
     * @return 모든 재시도 후에도 실패한 종목 목록
     */
    private List<String> retryUnsubscribe(List<String> failedStocks, int maxRetries) {
        List<String> currentFailedStocks = new ArrayList<>(failedStocks);

        for (int retry = 1; retry <= maxRetries; retry++) {
            if (currentFailedStocks.isEmpty()) {
                break;
            }

            // 재시도 전 잠시 대기 (지수 백오프)
            try {
                // 재시도마다 대기 시간 증가: 1초, 2초, 4초 ...
                long waitTime = (long) Math.pow(2, retry - 1) * 1000;
                TimeUnit.MILLISECONDS.sleep(waitTime);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }

            log.info("구독 해제 {}차 재시도: {}개 종목", retry, currentFailedStocks.size());

            List<String> stillFailed = new ArrayList<>();
            for (String stockCode : currentFailedStocks) {
                try {
                    boolean success = kiwoomWebSocketClient.unsubscribeStock(stockCode);
                    if (!success) {
                        stillFailed.add(stockCode);
                    }
                } catch (Exception e) {
                    log.error("재시도 중 예외 발생: {}, 오류={}", stockCode, e.getMessage());
                    stillFailed.add(stockCode);
                }
            }

            log.info("{}차 재시도 결과: {}개 성공, {}개 실패",
                    retry, currentFailedStocks.size() - stillFailed.size(), stillFailed.size());

            currentFailedStocks = stillFailed;
        }

        return currentFailedStocks; // 최종적으로 실패한 종목 목록 반환
    }

    /**
     * 마지막 안전장치: 8시 10분에 다시 한번 모든 구독 해제 시도
     * 실패 시 강제로 웹소켓 연결 종료
     */
    @Scheduled(cron = "0 10 20 * * *", zone = "Asia/Seoul")
    public void finalUnsubscribeCheck() {
        Set<String> remainingSubscriptions = kiwoomWebSocketClient.getSubscribedStockCodes();

        if (!remainingSubscriptions.isEmpty()) {
            log.warn("마지막 안전 점검: {}개 종목이 여전히 구독 중, 강제 해제 시도", remainingSubscriptions.size());

            List<String> stockCodes = new ArrayList<>(remainingSubscriptions);
            List<String> finalFailedStocks = new ArrayList<>();

            // 일반적인 방법으로 한 번 더 구독 해제 시도
            for (String stockCode : stockCodes) {
                try {
                    boolean success = kiwoomWebSocketClient.unsubscribeStock(stockCode);
                    if (!success) {
                        finalFailedStocks.add(stockCode);
                    }
                } catch (Exception e) {
                    log.error("강제 구독 해제 중 오류: {}", e.getMessage());
                    finalFailedStocks.add(stockCode);
                }
            }

            // 최종 확인
            Set<String> finalCheck = kiwoomWebSocketClient.getSubscribedStockCodes();
            if (!finalCheck.isEmpty()) {
                log.error("모든 시도 후에도 {}개 종목 구독 유지됨 - 웹소켓 강제 종료 시도", finalCheck.size());

                // Reflection을 사용한 강제 초기화
                try {
                    // 1. client 필드 접근
                    Field clientField = kiwoomWebSocketClient.getClass().getDeclaredField("client");
                    clientField.setAccessible(true);
                    Object clientObj = clientField.get(kiwoomWebSocketClient);

                    // 2. client의 close 메서드 호출 (WebSocketClient의 메서드)
                    if (clientObj != null) {
                        Method closeMethod = clientObj.getClass().getMethod("close");
                        closeMethod.invoke(clientObj);
                        log.info("키움증권 WebSocketClient 강제 종료 완료");
                    }

                    // 3. KiwoomWebSocketClient의 내부 상태 초기화
                    clearInternalCollection(kiwoomWebSocketClient, "subscribedStockCodes");
                    clearInternalCollection(kiwoomWebSocketClient, "stockCodeSubscriberCount");
                    clearInternalCollection(kiwoomWebSocketClient, "stockSubscriptionPurposes");
                    clearInternalCollection(kiwoomWebSocketClient, "latestPriceDataCache");

                    // 4. connected 필드를 false로 설정
                    Field connectedField = kiwoomWebSocketClient.getClass().getDeclaredField("connected");
                    connectedField.setAccessible(true);
                    connectedField.set(kiwoomWebSocketClient, false);

                    log.info("키움증권 웹소켓 클라이언트 내부 상태 강제 초기화 완료");
                } catch (Exception e) {
                    log.error("키움증권 웹소켓 클라이언트 강제 종료 중 오류 발생", e);
                }

                // 관리자에게 긴급 알림 발송
                sendUrgentAlertToAdmin(finalCheck);
                sendFinalFailureEmail(finalCheck);

                // 특별 이메일 발송 - 웹소켓 강제 종료 알림
                sendForcedDisconnectEmail(finalCheck);
            } else if (!finalFailedStocks.isEmpty() && finalCheck.isEmpty()) {
                // 마지막 시도에서 성공했을 경우 성공 메일 발송
                sendSuccessEmail(stockCodes.size());
            }
        } else {
            log.info("마지막 안전 점검: 모든 종목 구독 해제 완료");
        }
    }

    /**
     * Reflection을 사용하여 KiwoomWebSocketClient의 내부 컬렉션을 초기화하는 헬퍼 메서드
     */
    private void clearInternalCollection(Object target, String fieldName) {
        try {
            Field field = target.getClass().getDeclaredField(fieldName);
            field.setAccessible(true);
            Object collection = field.get(target);

            if (collection instanceof Collection) {
                ((Collection<?>) collection).clear();
            } else if (collection instanceof Map) {
                ((Map<?, ?>) collection).clear();
            }

            log.info("필드 {} 초기화 완료", fieldName);
        } catch (Exception e) {
            log.error("필드 {} 초기화 중 오류 발생: {}", fieldName, e.getMessage());
        }
    }

    /**
     * 웹소켓 강제 종료 알림 이메일 발송
     */
    private void sendForcedDisconnectEmail(Set<String> remainingStocks) {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock 긴급] 키움증권 웹소켓 강제 종료 알림");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String stockCodesStr = String.join(", ", remainingStocks);

            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "모든 해제 시도 후에도 구독이 유지되어 키움증권 웹소켓 연결을 강제로 종료했습니다.\n\n" +
                            "- 강제 종료 시간: %s\n" +
                            "- 구독 해제 실패 종목 수: %d개\n" +
                            "- 실패 종목 목록: %s\n\n" +
                            "다음 시장 시작 시 웹소켓이 자동으로 재연결됩니다.\n" +
                            "시스템 로그를 확인하여 구독 해제 실패 원인을 분석해주세요.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime, remainingStocks.size(), stockCodesStr
            );

            message.setText(content);
            emailSender.send(message);
            log.info("웹소켓 강제 종료 알림 이메일 발송 완료");
        } catch (Exception e) {
            log.error("웹소켓 강제 종료 알림 이메일 발송 실패", e);
        }
    }

    /**
     * 구독 취소 성공 이메일 발송
     */
    private void sendSuccessEmail(int totalStocks) {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock] 종목 구독 해제 성공 알림");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "종목 구독 해제 프로세스가 성공적으로 완료되었습니다.\n\n" +
                            "- 처리 시간: %s\n" +
                            "- 처리된 종목 수: %d개\n\n" +
                            "추가 조치가 필요하지 않습니다.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime, totalStocks
            );

            message.setText(content);
            emailSender.send(message);
            log.info("구독 해제 성공 이메일 발송 완료");
        } catch (Exception e) {
            log.error("구독 해제 성공 이메일 발송 실패", e);
        }
    }

    /**
     * 구독 취소 실패 이메일 발송
     */
    private void sendFailureEmail(int totalStocks, List<String> failedStocks) {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock] 종목 구독 해제 실패 알림");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String failedStocksStr = String.join(", ", failedStocks);

            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "종목 구독 해제 프로세스 중 일부 종목에서 오류가 발생했습니다.\n\n" +
                            "- 처리 시간: %s\n" +
                            "- 총 종목 수: %d개\n" +
                            "- 실패한 종목 수: %d개\n" +
                            "- 실패한 종목 목록: %s\n\n" +
                            "추가 조치가 필요할 수 있습니다. 시스템 로그를 확인해주세요.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime, totalStocks, failedStocks.size(), failedStocksStr
            );

            message.setText(content);
            emailSender.send(message);
            log.info("구독 해제 실패 이메일 발송 완료");
        } catch (Exception e) {
            log.error("구독 해제 실패 이메일 발송 실패", e);
        }
    }

    /**
     * 최종 구독 취소 실패 이메일 발송 (마지막 안전장치 후)
     */
    private void sendFinalFailureEmail(Set<String> failedStocks) {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock 긴급] 종목 구독 해제 최종 실패 알림");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String failedStocksStr = String.join(", ", failedStocks);

            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "모든 재시도 및 마지막 안전장치 후에도 일부 종목의 구독 해제에 실패했습니다.\n\n" +
                            "- 최종 확인 시간: %s\n" +
                            "- 구독 해제 실패 종목 수: %d개\n" +
                            "- 실패한 종목 목록: %s\n\n" +
                            "긴급 조치가 필요합니다. 수동으로 구독 해제를 시도하거나 시스템을 점검해주세요.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime, failedStocks.size(), failedStocksStr
            );

            message.setText(content);
            emailSender.send(message);
            log.info("최종 구독 해제 실패 이메일 발송 완료");
        } catch (Exception e) {
            log.error("최종 구독 해제 실패 이메일 발송 실패", e);
        }
    }

    /**
     * 구독 해제 중 예외 발생 시 이메일 발송
     */
    private void sendErrorEmail(Exception exception) {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock 오류] 종목 구독 해제 프로세스 오류 발생");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String errorMessage = exception.getMessage();
            if (errorMessage == null || errorMessage.isEmpty()) {
                errorMessage = exception.getClass().getName();
            }

            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "종목 구독 해제 프로세스 중 예외가 발생했습니다.\n\n" +
                            "- 발생 시간: %s\n" +
                            "- 오류 내용: %s\n\n" +
                            "시스템 로그를 확인하여 자세한 오류 내용을 확인해주세요.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime, errorMessage
            );

            message.setText(content);
            emailSender.send(message);
            log.info("구독 해제 오류 이메일 발송 완료");
        } catch (Exception e) {
            log.error("구독 해제 오류 이메일 발송 실패", e);
        }
    }

    /**
     * 환불 실패 시 관리자에게 알림 이메일 발송
     */
    private void sendRefundFailureEmail(Long memberId, Long amount, Exception e) {
        try {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(adminEmail);
            message.setSubject("[ChickenStock 긴급] 주문 취소 환불 실패 알림");

            String dateTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            String errorMessage = (e.getMessage() != null) ? e.getMessage() : e.getClass().getName();

            String content = String.format(
                    "안녕하세요, ChickenStock 관리자님\n\n" +
                            "시장 마감 시 주문 취소 후 환불 처리에 실패했습니다.\n\n" +
                            "- 발생 시간: %s\n" +
                            "- 회원 ID: %d\n" +
                            "- 환불 예정 금액: %d원\n" +
                            "- 오류 내용: %s\n\n" +
                            "긴급 조치가 필요합니다. 해당 회원의 주문 및 자산 상태를 확인해주세요.\n\n" +
                            "감사합니다.\n" +
                            "ChickenStock 시스템",
                    dateTime, memberId, amount, errorMessage
            );

            message.setText(content);
            emailSender.send(message);
            log.info("환불 실패 알림 이메일 발송 완료");
        } catch (Exception ex) {
            log.error("환불 실패 알림 이메일 발송 실패", ex);
        }
    }

    // 관리자 알림 메서드 (이메일로 대체)
    private void sendAlertToAdmin(Set<String> failedStocks) {
        log.error("관리자 알림: {}개 종목 구독 해제 실패 - {}",
                failedStocks.size(), String.join(", ", failedStocks));
    }

    private void sendUrgentAlertToAdmin(Set<String> failedStocks) {
        log.error("긴급 관리자 알림: 모든 시도 후에도 {}개 종목 구독 유지됨 - {}",
                failedStocks.size(), String.join(", ", failedStocks));
    }
}