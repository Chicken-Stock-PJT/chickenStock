package realClassOne.chickenStock.member.service;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.stream.Collectors;
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

import java.util.*;

import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.entity.TradeHistory;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;
import realClassOne.chickenStock.stock.repository.TradeHistoryRepository;
import realClassOne.chickenStock.stock.service.KiwoomStockApiService;
import realClassOne.chickenStock.stock.service.PortfolioService;
import realClassOne.chickenStock.stock.service.StockSubscriptionService;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
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
    private final TradeHistoryRepository tradeHistoryRepository;
    private final PortfolioService portfolioService;
    private final StockDataRepository stockDataRepository;
    private final KiwoomStockApiService kiwoomStockApiService;

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

            if (watchLists.isEmpty()) {
                return WatchListResponseDTO.builder()
                        .message("관심종목이 없습니다")
                        .watchList(new ArrayList<>())
                        .updatedAt(LocalDateTime.now())
                        .build();
            }

            // 키움증권 API 호출을 위해 관심종목 코드를 '|'로 구분하여 문자열 생성
            String stockCodes = watchLists.stream()
                    .map(watchList -> watchList.getStockData().getShortCode())
                    .collect(Collectors.joining("|"));

            // 키움증권 API 호출 - KiwoomStockApiService 활용
            JsonNode response = kiwoomStockApiService.getWatchListInfo(stockCodes);

            // 응답 DTO 생성
            List<WatchListResponseDTO.WatchListItemDTO> watchListItems = new ArrayList<>();

            if (response.has("atn_stk_infr") && response.get("atn_stk_infr").isArray()) {
                JsonNode stockInfoArray = response.get("atn_stk_infr");

                for (JsonNode stockInfo : stockInfoArray) {
                    WatchListResponseDTO.WatchListItemDTO itemDTO = parseWatchListItem(stockInfo);
                    watchListItems.add(itemDTO);
                }
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

    /**
     * 키움증권 API 응답 데이터에서 관심종목 항목 DTO를 생성합니다.
     *
     * @param stockInfo API 응답의 종목 정보
     * @return 관심종목 항목 DTO
     */
    private WatchListResponseDTO.WatchListItemDTO parseWatchListItem(JsonNode stockInfo) {
        try {
            String stockCode = stockInfo.get("stk_cd").asText();
            String stockName = stockInfo.get("stk_nm").asText();

            // 현재가 처리 (앞에 +/- 부호가 있을 경우 처리)
            String currentPriceStr = stockInfo.get("cur_prc").asText();
            Long currentPrice = parsePrice(currentPriceStr);

            // 전일대비 변동금액
            String priceChange = stockInfo.get("pred_pre").asText();

            // 등락률
            String changeRate = stockInfo.get("flu_rt").asText();

            // 거래량
            String tradingVolume = stockInfo.get("trde_qty").asText();

            // 체결시간 파싱
            LocalDateTime timestamp = LocalDateTime.now();
            if (stockInfo.has("cntr_tm") && stockInfo.has("dt")) {
                String timeStr = stockInfo.get("cntr_tm").asText();
                String dateStr = stockInfo.get("dt").asText();

                try {
                    // 날짜: yyyyMMdd, 시간: HHmmss 형식
                    LocalDate date = LocalDate.parse(dateStr, DateTimeFormatter.ofPattern("yyyyMMdd"));
                    LocalTime time = LocalTime.parse(timeStr, DateTimeFormatter.ofPattern("HHmmss"));
                    timestamp = LocalDateTime.of(date, time);
                } catch (Exception e) {
                    log.warn("체결시간 파싱 실패: {}, {}", dateStr, timeStr);
                }
            }

            return WatchListResponseDTO.WatchListItemDTO.builder()
                    .stockCode(stockCode)
                    .stockName(stockName)
                    .currentPrice(currentPrice)
                    .priceChange(priceChange)
                    .changeRate(changeRate)
                    .tradingVolume(tradingVolume)
                    .timestamp(timestamp)
                    .build();

        } catch (Exception e) {
            log.error("관심종목 항목 DTO 생성 중 오류", e);

            // 기본 정보라도 반환
            return WatchListResponseDTO.WatchListItemDTO.builder()
                    .stockCode(stockInfo.has("stk_cd") ? stockInfo.get("stk_cd").asText() : "")
                    .stockName(stockInfo.has("stk_nm") ? stockInfo.get("stk_nm").asText() : "")
                    .currentPrice(0L)
                    .priceChange("0")
                    .changeRate("0%")
                    .tradingVolume("0")
                    .timestamp(LocalDateTime.now())
                    .build();
        }
    }

    /**
     * 문자열을 Long으로 변환합니다. 앞에 +/- 부호가 있으면 제거합니다.
     *
     * @param value 변환할 문자열
     * @return 변환된 Long 값, 변환 실패시 0 반환
     */
    private Long parsePrice(String value) {
        if (value == null || value.isEmpty()) {
            return 0L;
        }

        try {
            // +/- 기호 제거, 콤마(,) 제거
            String cleaned = value.replaceAll("^[+\\-]", "").replace(",", "");
            return Long.parseLong(cleaned);
        } catch (NumberFormatException e) {
            log.warn("가격 변환 실패: {}", value);
            return 0L;
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

            log.info("관심종목 추가 성공: 회원 ID {}, 종목 코드 {}", memberId, stockCode);
        } catch (CustomException ce) {
            log.error("관심종목 추가 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("관심종목 추가 중 오류 발생", e);
            throw new CustomException(StockErrorCode.OPERATION_FAILED, "관심종목 추가 중 오류가 발생했습니다");
        }
    }

    /**
     * API를 통해 사용자의 주식 자산 총 평가액을 계산합니다.
     *
     * @param positions 사용자의 보유 포지션 목록
     * @return 주식 자산 총 평가액
     */
    private Long calculateStockValuationUsingAPI(List<HoldingPosition> positions) {
        // 보유 포지션이 없으면 0 반환
        if (positions == null || positions.isEmpty()) {
            return 0L;
        }

        // 종목 코드 목록 생성
        List<String> stockCodes = positions.stream()
                .map(position -> position.getStockData().getShortCode())
                .collect(Collectors.toList());

        // 현재가 정보를 담을 맵
        Map<String, Long> currentPrices = new HashMap<>();

        // API를 통해 가격 정보 조회
        if (!stockCodes.isEmpty()) {
            try {
                String stockCodesStr = String.join("|", stockCodes);
                JsonNode priceData = kiwoomStockApiService.getWatchListInfo(stockCodesStr);

                // API 응답에서 가격 정보 추출
                if (priceData.has("atn_stk_infr") && priceData.get("atn_stk_infr").isArray()) {
                    JsonNode stockInfoArray = priceData.get("atn_stk_infr");

                    for (JsonNode stockInfo : stockInfoArray) {
                        String stockCode = stockInfo.get("stk_cd").asText();
                        String currentPriceStr = stockInfo.get("cur_prc").asText();
                        Long currentPrice = parsePrice(currentPriceStr);
                        currentPrices.put(stockCode, currentPrice);
                    }
                }
            } catch (Exception e) {
                log.error("API를 통한 가격 정보 조회 중 오류 발생", e);
                // 오류 발생시 평균가격 사용 (아래에서 처리)
            }
        }

        // 총 평가액 계산
        Long totalValuation = 0L;
        for (HoldingPosition position : positions) {
            String stockCode = position.getStockData().getShortCode();
            // 현재가 (API에서 조회된 가격 또는 평균가)
            Long currentPrice = currentPrices.getOrDefault(stockCode, position.getAveragePrice());
            // 종목별 평가액 계산 및 합산
            Long valuation = currentPrice * position.getQuantity();
            totalValuation += valuation;
        }

        return totalValuation;
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

            // 주식 자산 계산 - REST API 방식으로 변경된 메서드 사용
            Long stockValuation = calculateStockValuationUsingAPI(positions);

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

            // 주식 종목 코드 목록 생성 (API 호출용)
            List<String> stockCodes = positions.stream()
                    .map(position -> position.getStockData().getShortCode())
                    .collect(Collectors.toList());

            // 실시간 가격 정보 조회 (한 번의 API 호출로 모든 종목 정보 얻기)
            Map<String, Long> currentPrices = new HashMap<>();
            if (!stockCodes.isEmpty()) {
                try {
                    String stockCodesStr = String.join("|", stockCodes);
                    JsonNode priceData = kiwoomStockApiService.getWatchListInfo(stockCodesStr);

                    // API 응답에서 가격 정보 추출
                    if (priceData.has("atn_stk_infr") && priceData.get("atn_stk_infr").isArray()) {
                        JsonNode stockInfoArray = priceData.get("atn_stk_infr");

                        for (JsonNode stockInfo : stockInfoArray) {
                            String stockCode = stockInfo.get("stk_cd").asText();
                            String currentPriceStr = stockInfo.get("cur_prc").asText();
                            Long currentPrice = parsePrice(currentPriceStr);
                            currentPrices.put(stockCode, currentPrice);
                        }
                    }
                } catch (Exception e) {
                    log.error("API를 통한 가격 정보 조회 중 오류 발생", e);
                    // 오류 발생시 아래에서 평균가격 사용
                }
            }

            // 주식 자산 계산 및 개별 종목 비중 계산
            Long stockValuation = 0L;
            List<AssetAllocationResponseDTO.StockAllocationDTO> stockAllocations = new ArrayList<>();

            for (HoldingPosition position : positions) {
                String stockCode = position.getStockData().getShortCode();

                // 현재가 확인 (API 응답에서 가져온 가격 또는 기본값으로 평균가 사용)
                Long currentPrice = currentPrices.getOrDefault(stockCode, position.getAveragePrice());

                // 개별 종목 평가금액 및 합산
                Long valuation = currentPrice * position.getQuantity();
                stockValuation += valuation;

                // 비중 계산은 최종 총자산 계산 후에 수행하기 위해 일단 데이터만 저장
                stockAllocations.add(AssetAllocationResponseDTO.StockAllocationDTO.builder()
                        .stockCode(stockCode)
                        .stockName(position.getStockData().getShortName())
                        .currentValuation(valuation)
                        .allocationRatio(0.0) // 임시값 (나중에 업데이트)
                        .quantity(position.getQuantity())
                        .averagePrice(position.getAveragePrice())
                        .currentPrice(currentPrice)
                        .build());
            }

            // 총 자산
            Long totalAsset = cashAmount + stockValuation;

            // 비중 계산
            Double cashRatio = totalAsset > 0 ? (cashAmount.doubleValue() / totalAsset) * 100 : 0;
            Double stockRatio = totalAsset > 0 ? (stockValuation.doubleValue() / totalAsset) * 100 : 0;

            // 개별 종목 비중 업데이트
            for (AssetAllocationResponseDTO.StockAllocationDTO stock : stockAllocations) {
                Double ratio = totalAsset > 0 ? (stock.getCurrentValuation().doubleValue() / totalAsset) * 100 : 0;
                stock.setAllocationRatio(Math.round(ratio * 100) / 100.0); // 소수점 2자리까지 반올림
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

            // 단순화된 접근: 전체 수익률을 기간별로 배분
            // 이 방식은 실제 거래 기록이 없거나 정확한 계산이 어려울 때 유용합니다.

            // 요청한 기간(들)에 대해 수익률 계산
            if ("all".equalsIgnoreCase(period) || "daily".equalsIgnoreCase(period)) {
                addSimpleDailyReturn(periodReturns, calculatedReturnRate, calculatedProfitLoss);
            }

            if ("all".equalsIgnoreCase(period) || "weekly".equalsIgnoreCase(period)) {
                addSimpleWeeklyReturn(periodReturns, calculatedReturnRate, calculatedProfitLoss);
            }

            if ("all".equalsIgnoreCase(period) || "monthly".equalsIgnoreCase(period)) {
                addSimpleMonthlyReturn(periodReturns, calculatedReturnRate, calculatedProfitLoss);
            }

            if ("all".equalsIgnoreCase(period) || "yearly".equalsIgnoreCase(period)) {
                addSimpleYearlyReturn(periodReturns, calculatedReturnRate, calculatedProfitLoss);
            }

            // 응답 생성
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
     * 일간(오늘) 수익률을 간단하게 추가합니다.
     * 전체 수익률의 일부를 할당합니다.
     */
    private void addSimpleDailyReturn(Map<String, ReturnRateResponseDTO.PeriodReturnDTO> periodReturns,
                                      Double overallReturnRate, Long overallProfitLoss) {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime startOfDay = LocalDateTime.of(LocalDate.now(), LocalTime.MIN);

        Double dailyReturnRate = overallReturnRate;
        Long dailyProfitLoss = overallProfitLoss;

        // 결과 저장
        periodReturns.put("daily", ReturnRateResponseDTO.PeriodReturnDTO.builder()
                .period("daily")
                .returnRate(Math.round(dailyReturnRate * 100) / 100.0)
                .profitLoss(dailyProfitLoss)
                .startDate(startOfDay)
                .endDate(now)
                .build());
    }

    /**
     * 주간(오늘 포함 일주일) 수익률을 간단하게 추가합니다.
     * 전체 수익률의 일부를 할당합니다.
     */
    private void addSimpleWeeklyReturn(Map<String, ReturnRateResponseDTO.PeriodReturnDTO> periodReturns,
                                       Double overallReturnRate, Long overallProfitLoss) {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime weekAgo = now.minus(6, ChronoUnit.DAYS).withHour(0).withMinute(0).withSecond(0).withNano(0);

        // 전체 수익률의 40%를 주간 수익률로 설정
        Double weeklyReturnRate = overallReturnRate;
        Long weeklyProfitLoss = overallProfitLoss;

        // 결과 저장
        periodReturns.put("weekly", ReturnRateResponseDTO.PeriodReturnDTO.builder()
                .period("weekly")
                .returnRate(Math.round(weeklyReturnRate * 100) / 100.0)
                .profitLoss(weeklyProfitLoss)
                .startDate(weekAgo)
                .endDate(now)
                .build());
    }

    /**
     * 월간(오늘 포함 한달) 수익률을 간단하게 추가합니다.
     * 전체 수익률의 일부를 할당합니다.
     */
    private void addSimpleMonthlyReturn(Map<String, ReturnRateResponseDTO.PeriodReturnDTO> periodReturns,
                                        Double overallReturnRate, Long overallProfitLoss) {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime monthAgo = now.minus(29, ChronoUnit.DAYS).withHour(0).withMinute(0).withSecond(0).withNano(0);

        // 전체 수익률의 60%를 월간 수익률로 설정
        Double monthlyReturnRate = overallReturnRate;
        Long monthlyProfitLoss = overallProfitLoss;

        // 결과 저장
        periodReturns.put("monthly", ReturnRateResponseDTO.PeriodReturnDTO.builder()
                .period("monthly")
                .returnRate(Math.round(monthlyReturnRate * 100) / 100.0)
                .profitLoss(monthlyProfitLoss)
                .startDate(monthAgo)
                .endDate(now)
                .build());
    }

    /**
     * 연간(오늘 포함 일년) 수익률을 간단하게 추가합니다.
     * 전체 수익률과 동일하게 설정합니다.
     */
    private void addSimpleYearlyReturn(Map<String, ReturnRateResponseDTO.PeriodReturnDTO> periodReturns,
                                       Double overallReturnRate, Long overallProfitLoss) {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime yearAgo = now.minus(364, ChronoUnit.DAYS).withHour(0).withMinute(0).withSecond(0).withNano(0);

        // 전체 수익률을 연간 수익률로 설정
        Double yearlyReturnRate = overallReturnRate;
        Long yearlyProfitLoss = overallProfitLoss;

        // 결과 저장
        periodReturns.put("yearly", ReturnRateResponseDTO.PeriodReturnDTO.builder()
                .period("yearly")
                .returnRate(Math.round(yearlyReturnRate * 100) / 100.0)
                .profitLoss(yearlyProfitLoss)
                .startDate(yearAgo)
                .endDate(now)
                .build());
    }

    /**
     * 사용자의 관심종목에서 특정 종목을 삭제합니다.
     *
     * @param authorizationHeader 인증 헤더
     * @param stockCode           삭제할 종목 코드
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

            log.info("관심종목 삭제 성공: 회원 ID {}, 종목 코드 {}", memberId, stockCode);
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
        try {
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 기본값 설정
            String totalAsset = "0";
            String returnRate = "0.0";

            try {
                // 포트폴리오 서비스에서 전체 포트폴리오 정보를 가져옴
                var portfolio = portfolioService.getPortfolioById(memberId);

                // totalAsset(총 자산 = 현금 + 주식평가금액)을 사용
                totalAsset = portfolio.getTotalAsset().toString();
                returnRate = String.format("%.2f", portfolio.getTotalReturnRate());

            } catch (Exception e) {
                log.warn("포트폴리오 정보 조회 중 오류 발생: {}", e.getMessage());

                // 포트폴리오 조회 실패 시 최소한 현금 금액이라도 표시
                if (member.getMemberMoney() != null) {
                    totalAsset = member.getMemberMoney().toString();
                    log.info("포트폴리오 조회 실패, 현금만 표시: {}", totalAsset);
                }
            }

            String nickname = member.getNickname();
            String isOauth = !"local".equals(member.getProvider()) ? "true" : "false";
            String memberMoney = member.getMemberMoney() != null ? member.getMemberMoney().toString() : "조회 중";

            return SimpleMemberProfileResponseDTO.of(nickname, totalAsset, returnRate, isOauth, memberMoney);
        } catch (Exception e) {
            log.error("간단 회원 정보 조회 중 오류 발생", e);
            throw new CustomException(MemberErrorCode.MEMBER_NOT_FOUND, "회원 정보를 불러올 수 없습니다");
        }
    }

    // 특정 종목에 대한 거래내역을 조회합니다.
    @Transactional(readOnly = true)
    public StockTradeHistoryResponseDTO getStockTradeHistory(
            String authorizationHeader, String stockCode) {

        try {
            // 토큰에서 회원 ID 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            StockData stockData = stockDataRepository.findByShortCode(stockCode)
                    .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));

            // 전체 거래내역 조회
            List<TradeHistory> tradeHistories = tradeHistoryRepository.findByMemberAndStockData(member, stockData);

            // 현재 보유 정보 조회
            Optional<HoldingPosition> currentPosition = holdingPositionRepository.findByMemberAndStockData(member, stockData);

            // 거래내역이 없는 경우 빈 응답 반환
            if (tradeHistories.isEmpty() && currentPosition.isEmpty()) {
                return StockTradeHistoryResponseDTO.builder()
                        .stockCode(stockCode)
                        .stockName(stockData.getShortName())
                        .tradeHistories(new ArrayList<>())
                        .message("해당 종목의 거래내역이 없습니다.")
                        .build();
            }

            // 응답 DTO 생성
            List<StockTradeHistoryResponseDTO.TradeHistoryDTO> historyDTOs = new ArrayList<>();

            for (TradeHistory history : tradeHistories) {
                StockTradeHistoryResponseDTO.TradeHistoryDTO dto = StockTradeHistoryResponseDTO.TradeHistoryDTO.builder()
                        .tradeId(history.getTradeHistoryId())
                        .tradeType(history.getTradeType().toString())
                        .quantity(history.getQuantity())
                        .unitPrice(history.getUnitPrice())
                        .totalPrice(history.getTotalPrice())
                        .tradedAt(history.getTradedAt())
                        .build();

                historyDTOs.add(dto);
            }

            // 현재 보유 정보 추가
            Long currentQuantity = 0L;
            Long averagePrice = 0L;
            Double returnRate = 0.0;

            if (currentPosition.isPresent()) {
                HoldingPosition position = currentPosition.get();
                currentQuantity = position.getQuantity().longValue();
                averagePrice = position.getAveragePrice();
                returnRate = position.getReturnRate();
            }

            return StockTradeHistoryResponseDTO.builder()
                    .stockCode(stockCode)
                    .stockName(stockData.getShortName())
                    .tradeHistories(historyDTOs)
                    .currentQuantity(currentQuantity)
                    .averagePrice(averagePrice)
                    .returnRate(returnRate)
                    .message("조회 성공")
                    .build();

        } catch (CustomException ce) {
            log.error("종목 거래내역 조회 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("종목 거래내역 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.OPERATION_FAILED, "종목 거래내역 조회 중 오류가 발생했습니다");
        }
    }

    /**
     * 사용자의 보유 주식 목록과 현금 자산을 조회합니다.
     *
     * @param authorizationHeader 인증 헤더
     * @return 보유 주식 및 현금 정보 DTO
     */
    @Transactional(readOnly = true)
    public HoldingStocksResponseDTO getHoldingStocks(String authorizationHeader) {
        try {
            // 토큰에서 회원 ID 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // 보유 주식 조회
            List<HoldingPosition> holdingPositions = holdingPositionRepository.findByMember(member);

            // 보유 주식 DTO 리스트 생성
            List<HoldingStocksResponseDTO.HoldingStockDTO> holdingStockDTOs = holdingPositions.stream()
                    .map(position -> HoldingStocksResponseDTO.HoldingStockDTO.builder()
                            .stockCode(position.getStockData().getShortCode())
                            .stockName(position.getStockData().getShortName())
                            .quantity(position.getQuantity().longValue())
                            .averagePrice(position.getAveragePrice())
                            .build())
                    .collect(Collectors.toList());

            // 응답 생성
            return HoldingStocksResponseDTO.builder()
                    .cashAmount(member.getMemberMoney())
                    .holdingStocks(holdingStockDTOs)
                    .updatedAt(LocalDateTime.now())
                    .message("보유 주식 조회 성공")
                    .build();

        } catch (CustomException ce) {
            log.error("보유 주식 조회 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("보유 주식 조회 중 오류 발생", e);
            throw new CustomException(StockErrorCode.OPERATION_FAILED, "보유 주식 조회 중 오류가 발생했습니다");
        }
    }
}
