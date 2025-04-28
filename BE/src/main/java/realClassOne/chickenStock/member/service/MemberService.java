package realClassOne.chickenStock.member.service;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.common.util.SecurityUtil;
import realClassOne.chickenStock.member.dto.response.*;
import realClassOne.chickenStock.member.entity.InvestmentSummary;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.entity.WatchList;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import org.springframework.security.crypto.password.PasswordEncoder;
import realClassOne.chickenStock.member.dto.request.PasswordChangeRequestDTO;
import realClassOne.chickenStock.member.repository.WatchListRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;

import java.util.List;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.entity.TradeHistory;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;
import realClassOne.chickenStock.stock.repository.StockDataRepository;
import realClassOne.chickenStock.stock.repository.TradeHistoryRepository;
import realClassOne.chickenStock.stock.service.PortfolioService;
import realClassOne.chickenStock.stock.service.StockSubscriptionService;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
@RequiredArgsConstructor
public class MemberService {

    private final MemberRepository memberRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtTokenProvider jwtTokenProvider;
    private final HoldingPositionRepository holdingPositionRepository;
    private final WatchListRepository watchListRepository;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final TradeHistoryRepository tradeHistoryRepository;
    private final StockSubscriptionService stockSubscriptionService;
    private final PortfolioService portfolioService;
    private final StockDataRepository stockDataRepository;

    @Transactional(readOnly = true)
    public MemberResponseDto getCurrentUser() {
        String email = SecurityUtil.getCurrentUserEmail();

        Member member = memberRepository.findByEmail(email)
                .orElseThrow(() -> new CustomException(MemberErrorCode.ALREADY_REGISTERED_EMAIL));

        return MemberResponseDto.from(member);
    }
    @Transactional
    public PasswordChangeResponseDTO changePassword(String authorizationHeader, PasswordChangeRequestDTO dto) {

        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        if (!passwordEncoder.matches(dto.getCurrentPassword(), member.getPassword())) {
            throw new CustomException(MemberErrorCode.INVALID_PASSWORD);
        }

        if (!dto.getNewPassword().equals(dto.getCheckPassword())) {
            throw new CustomException(MemberErrorCode.PASSWORD_CONFIRM_MISMATCH);
        }

        String encodedNewPassword = passwordEncoder.encode(dto.getNewPassword());
        member.updatePassword(encodedNewPassword);

        return PasswordChangeResponseDTO.of("비밀번호가 성공적으로 변경되었습니다.");
    }

    @Transactional
    public NicknameChangeResponseDTO changeNickname(String authorizationHeader, String newNickname) {
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // 사용자 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // 닉네임 중복 여부 확인
        if (memberRepository.existsByNickname(newNickname)) {
            throw new CustomException(MemberErrorCode.DUPLICATE_NICKNAME);
        }

        // 닉네임 변경
        member.changeNickname(newNickname);

        return NicknameChangeResponseDTO.of("닉네임이 성공적으로 변경되었습니다.");
    }

    // 사용자의 관심종목 목록과 해당 종목들의 현재가, 변동률 등을 조회합니다.
    @Transactional(readOnly = true)
    public WatchListResponseDTO getWatchList(String authorizationHeader) {
        try {
            // 토큰에서 회원 ID 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 관심종목 목록 조회
            List<WatchList> watchLists = watchListRepository.findByMember(member);

            // 응답 DTO 생성
            List<WatchListResponseDTO.WatchListItemDTO> watchListItems = new ArrayList<>();

            for (WatchList watchList : watchLists) {
                StockData stockData = watchList.getStockData();
                String stockCode = stockData.getShortCode();

                // 종목 구독 확인 및 필요시 구독 등록
                if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
                    try {
                        stockSubscriptionService.registerStockForSubscription(stockCode);
                    } catch (Exception e) {
                        log.error("종목 구독 실패: {}", stockCode, e);
                        // 구독 실패해도 계속 진행 (기존 캐시 데이터 사용)
                    }
                }

                // 최신 가격 정보 가져오기
                JsonNode priceData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);

                WatchListResponseDTO.WatchListItemDTO itemDTO = buildWatchListItemDTO(stockData, priceData);
                watchListItems.add(itemDTO);
            }

            return WatchListResponseDTO.builder()
                    .message("관심종목 조회 성공")
                    .watchList(watchListItems)
                    .updatedAt(LocalDateTime.now())
                    .build();

        } catch (CustomException ce) {
            log.error("관심종목 조회 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("관심종목 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.STOCK_NOT_FOUND, "관심종목 조회 중 오류가 발생했습니다");
        }
    }

    // 사용자의 관심종목에 특정 종목을 추가합니다.
    @Transactional
    public void addToWatchList(String authorizationHeader, String stockCode) {
        try {
            // 토큰에서 회원 ID 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            StockData stockData = stockDataRepository.findByShortCode(stockCode)
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 이미 관심종목으로 등록되어 있는지 확인
            if (watchListRepository.existsByMemberAndStockData(member, stockData)) {
                // 이미 등록된 경우 기존 목록 반환
                return;
            }

            // 관심종목 추가
            WatchList watchList = WatchList.builder()
                    .member(member)
                    .stockData(stockData)
                    .createdAt(LocalDateTime.now())
                    .build();

            watchListRepository.save(watchList);

            // 종목 실시간 구독 등록
            if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
                stockSubscriptionService.registerStockForSubscription(stockCode);
            }

        } catch (CustomException ce) {
            log.error("관심종목 추가 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("관심종목 추가 중 오류 발생", e);
            throw new CustomException(StockErrorCode.OPERATION_FAILED, "관심종목 추가 중 오류가 발생했습니다");
        }
    }


    // 주식 데이터와 실시간 가격 정보로 관심종목 항목 DTO를 생성합니다.
    private WatchListResponseDTO.WatchListItemDTO buildWatchListItemDTO(StockData stockData, JsonNode priceData) {
        try {
            String currentPrice = "";
            String priceChange = "";
            String changeRate = "";
            String tradingVolume = "";
            LocalDateTime timestamp = LocalDateTime.now();

            if (priceData != null) {
                currentPrice = priceData.has("10") ? priceData.get("10").asText() : "0";
                priceChange = priceData.has("11") ? priceData.get("11").asText() : "0";
                changeRate = priceData.has("12") ? priceData.get("12").asText() : "0";
                tradingVolume = priceData.has("13") ? priceData.get("13").asText() : "0";

                // 체결시간 필드가 있으면 파싱
                if (priceData.has("20")) {
                    String timeStr = priceData.get("20").asText();
                    // 시간 형식이 "HHmmss"인 경우 변환
                    try {
                        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HHmmss");
                        int hour = Integer.parseInt(timeStr.substring(0, 2));
                        int minute = Integer.parseInt(timeStr.substring(2, 4));
                        int second = Integer.parseInt(timeStr.substring(4, 6));
                        timestamp = LocalDateTime.now()
                                .withHour(hour)
                                .withMinute(minute)
                                .withSecond(second)
                                .withNano(0);
                    } catch (Exception e) {
                        log.warn("체결시간 파싱 실패: {}", timeStr);
                    }
                }
            }

            // 숫자 문자열을 Long으로 변환
            Long currentPriceValue = 0L;
            try {
                currentPriceValue = Long.parseLong(currentPrice.replace(",", ""));
            } catch (NumberFormatException e) {
                log.warn("현재가 변환 실패: {}", currentPrice);
            }

            return WatchListResponseDTO.WatchListItemDTO.builder()
                    .stockCode(stockData.getShortCode())
                    .stockName(stockData.getShortName())
                    .currentPrice(currentPriceValue)
                    .priceChange(priceChange)
                    .changeRate(changeRate)
                    .tradingVolume(tradingVolume)
                    .timestamp(timestamp)
                    .build();

        } catch (Exception e) {
            log.error("관심종목 항목 DTO 생성 중 오류", e);

            // 오류 발생 시 기본 정보라도 채워서 반환
            return WatchListResponseDTO.WatchListItemDTO.builder()
                    .stockCode(stockData.getShortCode())
                    .stockName(stockData.getShortName())
                    .currentPrice(0L)
                    .priceChange("0")
                    .changeRate("0%")
                    .tradingVolume("0")
                    .timestamp(LocalDateTime.now())
                    .build();
        }
    }

    /**
     * 사용자의 자산 비중(현금/주식)을 조회합니다.
     *
     * @param authorizationHeader 인증 헤더
     * @return 자산 비중 정보 DTO
     */
    @Transactional(readOnly = true)
    public AssetAllocationResponseDTO getAssetAllocation(String authorizationHeader) {
        try {
            // 토큰에서 회원 ID 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            List<HoldingPosition> positions = holdingPositionRepository.findByMember(member);

            // 현금 자산
            Long cashAmount = member.getMemberMoney();

            // 주식 자산 계산
            Long stockValuation = calculateStockValuation(positions);

            // 총 자산
            Long totalAsset = cashAmount + stockValuation;

            // 비중 계산
            Double cashRatio = totalAsset > 0 ? (cashAmount.doubleValue() / totalAsset) * 100 : 0;
            Double stockRatio = totalAsset > 0 ? (stockValuation.doubleValue() / totalAsset) * 100 : 0;

            // 응답 생성 (개별 종목 정보 없이)
            return AssetAllocationResponseDTO.builder()
                    .totalAsset(totalAsset)
                    .cashAmount(cashAmount)
                    .stockValuation(stockValuation)
                    .cashRatio(Math.round(cashRatio * 100) / 100.0) // 소수점 2자리까지 반올림
                    .stockRatio(Math.round(stockRatio * 100) / 100.0)
                    .stocks(new ArrayList<>()) // 기본 조회에서는 개별 종목 정보 제외
                    .updatedAt(LocalDateTime.now())
                    .build();

        } catch (CustomException ce) {
            log.error("자산 비중 조회 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("자산 비중 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.OPERATION_FAILED, "자산 비중 조회 중 오류가 발생했습니다");
        }
    }

    /**
     * 사용자의 상세 자산 비중(현금, 개별 주식 종목별)을 조회합니다.
     *
     * @param authorizationHeader 인증 헤더
     * @return 상세 자산 비중 정보 DTO
     */
    @Transactional(readOnly = true)
    public AssetAllocationResponseDTO getDetailedAssetAllocation(String authorizationHeader) {
        try {
            // 토큰에서 회원 ID 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            List<HoldingPosition> positions = holdingPositionRepository.findByMember(member);

            // 현금 자산
            Long cashAmount = member.getMemberMoney();

            // 주식 자산 계산
            Long stockValuation = calculateStockValuation(positions);

            // 총 자산
            Long totalAsset = cashAmount + stockValuation;

            // 비중 계산
            Double cashRatio = totalAsset > 0 ? (cashAmount.doubleValue() / totalAsset) * 100 : 0;
            Double stockRatio = totalAsset > 0 ? (stockValuation.doubleValue() / totalAsset) * 100 : 0;

            // 개별 종목 비중 계산
            List<AssetAllocationResponseDTO.StockAllocationDTO> stockAllocations = new ArrayList<>();

            for (HoldingPosition position : positions) {
                String stockCode = position.getStockData().getShortCode();

                // 종목 구독 확인 및 필요시 구독 등록
                if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
                    try {
                        stockSubscriptionService.registerStockForSubscription(stockCode);
                    } catch (Exception e) {
                        log.error("종목 구독 실패: {}", stockCode, e);
                        // 구독 실패해도 계속 진행 (평균가를 현재가로 대체 사용)
                    }
                }

                // 최신 가격 정보 가져오기
                JsonNode priceData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);
                Long currentPrice = position.getAveragePrice(); // 기본값으로 평균가 사용

                if (priceData != null && priceData.has("10")) {
                    try {
                        currentPrice = Long.parseLong(priceData.get("10").asText().replace(",", ""));
                    } catch (NumberFormatException e) {
                        log.warn("현재가 변환 실패: {}", priceData.get("10").asText());
                    }
                }

                // 개별 종목 평가금액 및 비중 계산
                Long valuation = currentPrice * position.getQuantity();
                Double ratio = totalAsset > 0 ? (valuation.doubleValue() / totalAsset) * 100 : 0;

                AssetAllocationResponseDTO.StockAllocationDTO stockDTO = AssetAllocationResponseDTO.StockAllocationDTO.builder()
                        .stockCode(stockCode)
                        .stockName(position.getStockData().getShortName())
                        .currentValuation(valuation)
                        .allocationRatio(Math.round(ratio * 100) / 100.0) // 소수점 2자리까지 반올림
                        .quantity(position.getQuantity())
                        .averagePrice(position.getAveragePrice())
                        .currentPrice(currentPrice)
                        .build();

                stockAllocations.add(stockDTO);
            }

            // 응답 생성
            return AssetAllocationResponseDTO.builder()
                    .totalAsset(totalAsset)
                    .cashAmount(cashAmount)
                    .stockValuation(stockValuation)
                    .cashRatio(Math.round(cashRatio * 100) / 100.0)
                    .stockRatio(Math.round(stockRatio * 100) / 100.0)
                    .stocks(stockAllocations)
                    .updatedAt(LocalDateTime.now())
                    .build();

        } catch (CustomException ce) {
            log.error("상세 자산 비중 조회 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("상세 자산 비중 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.OPERATION_FAILED, "상세 자산 비중 조회 중 오류가 발생했습니다");
        }
    }

    /**
     * 사용자의 주식 자산 총 평가액을 계산합니다.
     *
     * @param positions 사용자의 보유 포지션 목록
     * @return 주식 자산 총 평가액
     */
    private Long calculateStockValuation(List<HoldingPosition> positions) {
        Long totalValuation = 0L;

        for (HoldingPosition position : positions) {
            String stockCode = position.getStockData().getShortCode();

            // 종목 구독 확인 및 필요시 구독 등록
            if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
                try {
                    stockSubscriptionService.registerStockForSubscription(stockCode);
                } catch (Exception e) {
                    log.error("종목 구독 실패: {}", stockCode, e);
                    // 구독 실패해도 계속 진행 (평균가를 현재가로 대체 사용)
                }
            }

            // 최신 가격 정보 가져오기
            JsonNode priceData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);
            Long currentPrice = position.getAveragePrice(); // 기본값으로 평균가 사용

            if (priceData != null && priceData.has("10")) {
                try {
                    currentPrice = Long.parseLong(priceData.get("10").asText().replace(",", ""));
                } catch (NumberFormatException e) {
                    log.warn("현재가 변환 실패: {}", priceData.get("10").asText());
                }
            }

            // 종목별 평가액 계산 및 합산
            Long valuation = currentPrice * position.getQuantity();
            totalValuation += valuation;
        }

        return totalValuation;
    }

    /**
     * 사용자의 전체 투자 수익률을 조회합니다.
     *
     * @param authorizationHeader 인증 헤더
     * @return 전체 수익률 정보 DTO
     */
    @Transactional(readOnly = true)
    public ReturnRateResponseDTO getOverallReturnRate(String authorizationHeader) {
        try {
            // 토큰에서 회원 ID 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 투자 요약 정보 확인
            InvestmentSummary summary = member.getInvestmentSummary();

            if (summary == null) {
                // 투자 요약 정보가 없는 경우 기본값 반환
                return createEmptyReturnRateResponse();
            }

            // 현재 포트폴리오 정보 조회 (최신 가격으로 평가액 계산을 위해)
            Long currentValuation = portfolioService.getPortfolioById(memberId).getTotalValuation();

            // 최신 손익 및 수익률 직접 계산
            Long calculatedProfitLoss = currentValuation - summary.getTotalInvestment();
            Double calculatedReturnRate = summary.getTotalInvestment() > 0
                    ? (double) calculatedProfitLoss / summary.getTotalInvestment() * 100
                    : 0.0;

            // 응답 생성 (직접 계산한 값 사용)
            return ReturnRateResponseDTO.builder()
                    .initialInvestment(summary.getTotalInvestment())
                    .currentValuation(currentValuation)
                    .totalProfitLoss(calculatedProfitLoss)
                    .overallReturnRate(Math.round(calculatedReturnRate * 100) / 100.0)
                    .periodReturns(new HashMap<>()) // 기본 조회에서는 기간별 수익률 정보 제외
                    .updatedAt(LocalDateTime.now())
                    .build();

        } catch (CustomException ce) {
            log.error("전체 수익률 조회 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("전체 수익률 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.OPERATION_FAILED, "전체 수익률 조회 중 오류가 발생했습니다");
        }
    }

    /**
     * 사용자의 기간별(일간, 주간, 월간, 연간) 수익률을 조회합니다.
     *
     * @param authorizationHeader 인증 헤더
     * @param period 조회할 기간 (all, daily, weekly, monthly, yearly 중 하나)
     * @return 기간별 수익률 정보 DTO
     */
    @Transactional(readOnly = true)
    public ReturnRateResponseDTO getPeriodReturnRate(String authorizationHeader, String period) {
        try {
            // 토큰에서 회원 ID 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 투자 요약 정보 확인
            InvestmentSummary summary = member.getInvestmentSummary();

            if (summary == null) {
                // 투자 요약 정보가 없는 경우 기본값 반환
                return createEmptyReturnRateResponse();
            }

            // 현재 포트폴리오 정보 조회 (최신 가격으로 평가액 계산을 위해)
            Long currentValuation = portfolioService.getPortfolioById(memberId).getTotalValuation();

            // 최신 손익 및 수익률 직접 계산
            Long calculatedProfitLoss = currentValuation - summary.getTotalInvestment();
            Double calculatedReturnRate = summary.getTotalInvestment() > 0
                    ? (double) calculatedProfitLoss / summary.getTotalInvestment() * 100
                    : 0.0;

            // 기간별 수익률 계산
            Map<String, ReturnRateResponseDTO.PeriodReturnDTO> periodReturns = new HashMap<>();

            // 전체 수익률도 포함 (직접 계산한 값 사용)
            periodReturns.put("overall", ReturnRateResponseDTO.PeriodReturnDTO.builder()
                    .period("overall")
                    .returnRate(Math.round(calculatedReturnRate * 100) / 100.0)
                    .profitLoss(calculatedProfitLoss)
                    .startDate(null) // 전체 기간은 시작일 없음
                    .endDate(LocalDateTime.now())
                    .build());

            // 요청한 기간(들)에 대해 수익률 계산
            if ("all".equalsIgnoreCase(period) || "daily".equalsIgnoreCase(period)) {
                calculatePeriodReturn(memberId, "daily", periodReturns, currentValuation);
            }

            if ("all".equalsIgnoreCase(period) || "weekly".equalsIgnoreCase(period)) {
                calculatePeriodReturn(memberId, "weekly", periodReturns, currentValuation);
            }

            if ("all".equalsIgnoreCase(period) || "monthly".equalsIgnoreCase(period)) {
                calculatePeriodReturn(memberId, "monthly", periodReturns, currentValuation);
            }

            if ("all".equalsIgnoreCase(period) || "yearly".equalsIgnoreCase(period)) {
                calculatePeriodReturn(memberId, "yearly", periodReturns, currentValuation);
            }

            // 응답 생성 (직접 계산한 값 사용)
            return ReturnRateResponseDTO.builder()
                    .initialInvestment(summary.getTotalInvestment())
                    .currentValuation(currentValuation)
                    .totalProfitLoss(calculatedProfitLoss)
                    .overallReturnRate(Math.round(calculatedReturnRate * 100) / 100.0)
                    .periodReturns(periodReturns)
                    .updatedAt(LocalDateTime.now())
                    .build();

        } catch (CustomException ce) {
            log.error("기간별 수익률 조회 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("기간별 수익률 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.OPERATION_FAILED, "기간별 수익률 조회 중 오류가 발생했습니다");
        }
    }

    /**
     * 특정 기간의 수익률을 계산합니다.
     *
     * @param memberId 회원 ID
     * @param periodType 기간 타입 (daily, weekly, monthly, yearly)
     * @param periodReturns 결과를 저장할 Map
     * @param currentValuation 현재 평가액
     */
    private void calculatePeriodReturn(Long memberId, String periodType,
                                       Map<String, ReturnRateResponseDTO.PeriodReturnDTO> periodReturns,
                                       Long currentValuation) {
        try {
            LocalDateTime startDateTime;
            LocalDateTime endDateTime = LocalDateTime.now();
            LocalDate today = LocalDate.now();

            // 기간 시작일 설정
            switch (periodType) {
                case "daily":
                    // 어제 같은 시간
                    startDateTime = endDateTime.minus(1, ChronoUnit.DAYS);
                    break;
                case "weekly":
                    // 일주일 전 같은 시간
                    startDateTime = endDateTime.minus(7, ChronoUnit.DAYS);
                    break;
                case "monthly":
                    // 한 달 전 같은 날짜와 시간 (또는 말일)
                    LocalDate lastMonth = today.minus(1, ChronoUnit.MONTHS);
                    // 해당 월의 일 수가 다를 경우 말일로 조정
                    if (lastMonth.lengthOfMonth() < today.getDayOfMonth()) {
                        lastMonth = lastMonth.with(TemporalAdjusters.lastDayOfMonth());
                    }
                    startDateTime = LocalDateTime.of(lastMonth, LocalTime.from(endDateTime));
                    break;
                case "yearly":
                    // 1년 전 같은 날짜와 시간 (또는 윤년 등 고려하여 조정)
                    LocalDate lastYear = today.minus(1, ChronoUnit.YEARS);
                    // 윤년 고려 (2월 29일인 경우 2월 28일로 조정)
                    if (today.getMonthValue() == 2 && today.getDayOfMonth() == 29 && lastYear.lengthOfMonth() < 29) {
                        lastYear = lastYear.withDayOfMonth(28);
                    }
                    startDateTime = LocalDateTime.of(lastYear, LocalTime.from(endDateTime));
                    break;
                default:
                    log.warn("지원하지 않는 기간 타입: {}", periodType);
                    return;
            }

            // 특정 기간 시작 시점의 거래내역 검색
            // 해당 시점 이전의 가장 최근 거래 또는 가장 가까운 시점의 거래 조회
            List<TradeHistory> histories = tradeHistoryRepository.findByMemberIdAndTradedAtBefore(
                    memberId, startDateTime);

            // 해당 시점의 평가액 계산 (또는 추정)
            // 정확한 과거 시점 평가 데이터가 없는 경우 거래 내역으로 추정
            Long startValuation = estimateHistoricalValuation(memberId, startDateTime, histories);

            if (startValuation == null || startValuation == 0) {
                // 시작 시점 평가액을 계산할 수 없는 경우 (데이터 부족)
                log.info("{} 기간의 시작 평가액 계산 불가: 데이터 부족", periodType);
                return;
            }

            // 수익률 및 손익 계산
            Long profitLoss = currentValuation - startValuation;
            Double returnRate = startValuation > 0 ?
                    (profitLoss.doubleValue() / startValuation) * 100 : 0;

            // 결과 저장
            periodReturns.put(periodType, ReturnRateResponseDTO.PeriodReturnDTO.builder()
                    .period(periodType)
                    .returnRate(Math.round(returnRate * 100) / 100.0) // 소수점 2자리까지 반올림
                    .profitLoss(profitLoss)
                    .startDate(startDateTime)
                    .endDate(endDateTime)
                    .build());

        } catch (Exception e) {
            log.error("{} 기간 수익률 계산 중 오류 발생", periodType, e);
            // 오류가 발생해도 전체 처리를 중단하지 않고 해당 기간만 건너뜀
        }
    }

    /**
     * 특정 시점의 평가액을 추정합니다.
     * 정확한 과거 데이터가 없는 경우 거래 내역을 기반으로 추정합니다.
     *
     * @param memberId 회원 ID
     * @param targetDateTime 평가액을 계산할 시점
     * @param histories 해당 시점 이전의 거래 내역
     * @return 추정된 평가액
     */
    private Long estimateHistoricalValuation(Long memberId, LocalDateTime targetDateTime, List<TradeHistory> histories) {
        // 구현 방법 1: 해당 시점에 가장 가까운 거래 내역을 기준으로 추정
        if (histories.isEmpty()) {
            // 거래 내역이 없는 경우 (즉, 투자 시작 전)
            return 0L;
        }

        // 가장 가까운 시점의 거래 찾기 (정렬된 상태 가정)
        TradeHistory closestTrade = histories.get(0);
        for (TradeHistory history : histories) {
            if (history.getTradedAt().isBefore(targetDateTime) &&
                    history.getTradedAt().isAfter(closestTrade.getTradedAt())) {
                closestTrade = history;
            }
        }

        // 해당 시점의 포트폴리오 상태 추정 (거래 이후 변동 고려)
        // 이 예제에서는 단순히 해당 시점 이전의 모든 매수/매도 금액을 합산하여 추정
        Long buyTotal = 0L;
        Long sellTotal = 0L;

        for (TradeHistory history : histories) {
            if (history.getTradedAt().isBefore(targetDateTime)) {
                if (history.getTradeType() == TradeHistory.TradeType.BUY) {
                    buyTotal += history.getTotalPrice();
                } else {
                    sellTotal += history.getTotalPrice();
                }
            }
        }

        // 순 투자금 계산
        Long netInvestment = buyTotal - sellTotal;

        // 정확한 수익률 계산이 어려우므로, 최소한 순 투자금은 반환
        // 실제 애플리케이션에서는 더 정교한 방법으로 과거 시점의 평가액 추정 필요
        return Math.max(netInvestment, 0);
    }

    /**
     * 사용자의 관심종목에서 특정 종목을 삭제합니다.
     *
     * @param authorizationHeader 인증 헤더
     * @param stockCode 삭제할 종목 코드
     * @return 갱신된 관심종목 목록 DTO
     */
    @Transactional
    public void removeFromWatchList(String authorizationHeader, String stockCode) {
        try {
            // 토큰에서 회원 ID 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            StockData stockData = stockDataRepository.findByShortCode(stockCode)
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 관심종목 존재 여부 확인
            if (!watchListRepository.existsByMemberAndStockData(member, stockData)) {
                throw new CustomException(StockErrorCode.STOCK_NOT_FOUND, "관심종목에 존재하지 않는 종목입니다.");
            }

            // 관심종목 삭제
            watchListRepository.deleteByMemberAndStockData(member, stockData);

        } catch (CustomException ce) {
            log.error("관심종목 삭제 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("관심종목 삭제 중 오류 발생", e);
            throw new CustomException(StockErrorCode.OPERATION_FAILED, "관심종목 삭제 중 오류가 발생했습니다");
        }
    }

    /**
     * 투자 내역이 없는 경우의 기본 응답을 생성합니다.
     *
     * @return 기본 수익률 정보 DTO
     */
    private ReturnRateResponseDTO createEmptyReturnRateResponse() {
        return ReturnRateResponseDTO.builder()
                .initialInvestment(0L)
                .currentValuation(0L)
                .totalProfitLoss(0L)
                .overallReturnRate(0.0)
                .periodReturns(new HashMap<>())
                .updatedAt(LocalDateTime.now())
                .build();
    }


    public SimpleMemberProfileResponseDTO getSimpleProfile(String authorizationHeader) {
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        List<HoldingPosition> holdingPositions = holdingPositionRepository.findAllByMember_MemberId(memberId);

        // 수익률 평균 계산
        double averageReturnRate = 0.0;
        if (holdingPositions != null && !holdingPositions.isEmpty()) {
            averageReturnRate = holdingPositions.stream()
                    .mapToDouble(HoldingPosition::getReturnRate)
                    .average()
                    .orElse(0.0);
        }

        String nickname = member.getNickname();
        String memberMoney = member.getMemberMoney() != null ? member.getMemberMoney().toString() : "0";
        String returnRate = String.valueOf(averageReturnRate);
        String isOauth = !"local".equals(member.getProvider()) ? "true" : "false";

        return SimpleMemberProfileResponseDTO.of(nickname, memberMoney, returnRate, isOauth);
    }

}
