package realClassOne.chickenStock.stock.service;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.dto.response.PortfolioResponseDTO;
import realClassOne.chickenStock.stock.entity.HoldingPosition;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;
import realClassOne.chickenStock.stock.websocket.client.KiwoomWebSocketClient;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class PortfolioService {

    @Value("${kiwoom.api.url}")
    private String apiUrl;

    private final MemberRepository memberRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final JwtTokenProvider jwtTokenProvider;
    private final KiwoomStockApiService kiwoomStockApiService;

    // 캐시를 추가하여 API 호출 횟수 감소
    private final Map<Long, Map<String, JsonNode>> memberStockCache = new ConcurrentHashMap<>();
    private final Map<Long, LocalDateTime> memberCacheTime = new ConcurrentHashMap<>();
    private static final long CACHE_DURATION_MS = 2000; // 2초 캐시 유효시간

    /**
     * 인증 토큰으로 포트폴리오 정보를 조회합니다.
     * @param authorizationHeader 인증 헤더
     * @return 포트폴리오 정보
     */
    @Transactional(readOnly = true)
    public PortfolioResponseDTO getPortfolio(String authorizationHeader) {
        try {
            // 토큰에서 회원 ID 추출
            String token = jwtTokenProvider.resolveToken(authorizationHeader);
            Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

            if (memberId == null) {
                throw new CustomException(MemberErrorCode.MEMBER_NOT_FOUND);
            }

            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // active가 true인 항목만 조회하고 StockData를 함께 로드 (N+1 문제 해결)
            List<HoldingPosition> positions = holdingPositionRepository.findByMemberWithStockData(member)
                    .stream()
                    .filter(HoldingPosition::getActive)
                    .collect(Collectors.toList());

            // 캐시 확인 - 2초 이내 요청은 캐시된 데이터 사용
            LocalDateTime lastRequestTime = memberCacheTime.get(memberId);
            LocalDateTime now = LocalDateTime.now();

            if (lastRequestTime != null && lastRequestTime.plusNanos(CACHE_DURATION_MS * 1000000).isAfter(now)) {
                log.debug("캐시된 데이터 사용: 회원ID={}, 마지막 요청 시간={}", memberId, lastRequestTime);
                return buildPortfolioResponseDTOFromCache(member, positions, memberId);
            }

            // 캐시 시간 업데이트
            memberCacheTime.put(memberId, now);

            return buildPortfolioResponseDTO(member, positions);
        } catch (CustomException ce) {
            log.error("포트폴리오 조회 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("포트폴리오 조회 중 오류 발생", e);
            throw new CustomException(MemberErrorCode.MEMBER_NOT_FOUND, "포트폴리오 정보를 불러올 수 없습니다");
        }
    }

    @Transactional(readOnly = true)
    public PortfolioResponseDTO getPortfolioById(Long memberId) {
        try {
            Member member = memberRepository.findById(memberId)
                    .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

            // active가 true인 항목만 조회하고 StockData를 함께 로드
            List<HoldingPosition> positions = holdingPositionRepository.findByMemberWithStockData(member)
                    .stream()
                    .filter(HoldingPosition::getActive)
                    .collect(Collectors.toList());

            // 캐시 확인 - 2초 이내 요청은 캐시된 데이터 사용
            LocalDateTime lastRequestTime = memberCacheTime.get(memberId);
            LocalDateTime now = LocalDateTime.now();

            if (lastRequestTime != null && lastRequestTime.plusNanos(CACHE_DURATION_MS * 1000000).isAfter(now)) {
                log.debug("캐시된 데이터 사용: 회원ID={}, 마지막 요청 시간={}", memberId, lastRequestTime);
                return buildPortfolioResponseDTOFromCache(member, positions, memberId);
            }

            // 캐시 시간 업데이트
            memberCacheTime.put(memberId, now);

            return buildPortfolioResponseDTO(member, positions);
        } catch (CustomException ce) {
            log.error("포트폴리오 조회 실패: {}", ce.getMessage());
            throw ce;
        } catch (Exception e) {
            log.error("포트폴리오 조회 중 오류 발생", e);
            throw new CustomException(MemberErrorCode.MEMBER_NOT_FOUND, "포트폴리오 정보를 불러올 수 없습니다");
        }
    }

    /**
     * 캐시된 데이터를 활용하여 포트폴리오 응답 DTO를 생성합니다.
     */
    private PortfolioResponseDTO buildPortfolioResponseDTOFromCache(Member member, List<HoldingPosition> positions, Long memberId) {
        List<PortfolioResponseDTO.StockPositionDTO> positionDTOs = new ArrayList<>();
        Long totalInvestment = 0L;
        Long totalValuation = 0L;

        Map<String, JsonNode> stockDataCache = memberStockCache.getOrDefault(memberId, new HashMap<>());

        for (HoldingPosition position : positions) {
            String stockCode = position.getStockData().getShortCode();

            // 캐시에서 종목 정보 조회
            JsonNode stockData = stockDataCache.get(stockCode);
            if (stockData == null) {
                // 캐시에 없으면 웹소켓 데이터 활용 시도
                JsonNode wsData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);
                if (wsData != null && wsData.has("10")) {
                    stockData = wsData;
                }
            }

            // 현재가 추출
            Long currentPrice = 0L;
            if (stockData != null && stockData.has("cur_prc")) {
                String priceStr = stockData.get("cur_prc").asText()
                        .replace(",", "")
                        .replace("+", "")
                        .replace("-", "")
                        .trim();
                try {
                    currentPrice = Long.parseLong(priceStr);
                } catch (NumberFormatException e) {
                    log.warn("종목 {} 현재가 파싱 오류: {}", stockCode, priceStr);
                }
            } else if (stockData != null && stockData.has("10")) { // 웹소켓 데이터 구조
                String priceStr = stockData.get("10").asText()
                        .replace(",", "")
                        .replace("+", "")
                        .replace("-", "")
                        .trim();
                try {
                    currentPrice = Long.parseLong(priceStr);
                } catch (NumberFormatException e) {
                    log.warn("종목 {} 현재가 파싱 오류: {}", stockCode, priceStr);
                }
            }

            // 금액 계산
            Long investmentAmount = position.getAveragePrice() * position.getQuantity();
            Long valuationAmount = currentPrice * position.getQuantity();
            Long profitLoss = valuationAmount - investmentAmount;
            Double returnRate = investmentAmount > 0
                    ? (profitLoss.doubleValue() / investmentAmount.doubleValue()) * 100
                    : 0.0;

            totalInvestment += investmentAmount;
            totalValuation += valuationAmount;

            PortfolioResponseDTO.StockPositionDTO positionDTO = PortfolioResponseDTO.StockPositionDTO.builder()
                    .stockCode(stockCode)
                    .stockName(position.getStockData().getShortName())
                    .quantity(position.getQuantity())
                    .averagePrice(position.getAveragePrice())
                    .currentPrice(currentPrice)
                    .valuationAmount(valuationAmount)
                    .profitLoss(profitLoss)
                    .returnRate(returnRate)
                    .build();

            positionDTOs.add(positionDTO);
        }

        Long totalProfitLoss = totalValuation - totalInvestment;
        Double totalReturnRate = totalInvestment > 0
                ? (totalProfitLoss.doubleValue() / totalInvestment.doubleValue()) * 100
                : 0.0;
        Long totalAsset = member.getMemberMoney() + totalValuation;

        return PortfolioResponseDTO.builder()
                .memberMoney(member.getMemberMoney())
                .totalAsset(totalAsset)
                .totalInvestment(totalInvestment)
                .totalValuation(totalValuation)
                .totalProfitLoss(totalProfitLoss)
                .totalReturnRate(totalReturnRate)
                .positions(positionDTOs)
                .updatedAt(LocalDateTime.now())
                .build();
    }

    /**
     * 회원과 보유 종목 정보로 포트폴리오 응답 DTO를 생성합니다.
     * 관심종목정보요청(ka10095) API를 활용하여 한 번에 여러 종목 정보를 가져옵니다.
     */
    /**
     * 회원과 보유 종목 정보로 포트폴리오 응답 DTO를 생성합니다.
     * 관심종목정보요청(ka10095) API를 활용하여 한 번에 여러 종목 정보를 가져옵니다.
     */
    private PortfolioResponseDTO buildPortfolioResponseDTO(Member member, List<HoldingPosition> positions) {
        List<PortfolioResponseDTO.StockPositionDTO> positionDTOs = new ArrayList<>();
        Long totalInvestment = 0L;
        Long totalValuation = 0L;

        // 종목 코드 목록 추출
        List<String> stockCodes = positions.stream()
                .map(position -> position.getStockData().getShortCode())
                .collect(Collectors.toList());

        if (stockCodes.isEmpty()) {
            log.info("회원 ID: {}의 보유 종목이 없습니다.", member.getMemberId());

            // 보유 종목이 없는 경우 빈 포트폴리오 반환
            return PortfolioResponseDTO.builder()
                    .memberMoney(member.getMemberMoney())
                    .totalAsset(member.getMemberMoney())
                    .totalInvestment(0L)
                    .totalValuation(0L)
                    .totalProfitLoss(0L)
                    .totalReturnRate(0.0)
                    .positions(new ArrayList<>())
                    .updatedAt(LocalDateTime.now())
                    .build();
        }

        // KiwoomStockApiService를 통해 관심종목 정보 가져오기
        Map<String, JsonNode> stockDataMap = kiwoomStockApiService.getWatchListInfoMap(stockCodes);

        // API 호출 실패 시 웹소켓 데이터로 대체
        if (stockDataMap.isEmpty()) {
            stockCodes.forEach(stockCode -> {
                JsonNode websocketData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);
                if (websocketData != null) {
                    stockDataMap.put(stockCode, websocketData);
                }
            });
        }

        // 종목 데이터 캐시 업데이트
        memberStockCache.put(member.getMemberId(), stockDataMap);

        // 각 포지션에 대한 정보 구성
        for (HoldingPosition position : positions) {
            String stockCode = position.getStockData().getShortCode();

            // 해당 종목 데이터 조회
            JsonNode stockData = stockDataMap.get(stockCode);

            // 현재가 추출
            Long currentPrice = 0L;
            if (stockData != null) {
                try {
                    if (stockData.has("cur_prc")) { // API 응답
                        String priceStr = stockData.get("cur_prc").asText()
                                .replace(",", "")
                                .replace("+", "")
                                .replace("-", "")
                                .trim();
                        currentPrice = Long.parseLong(priceStr);
                    } else if (stockData.has("10")) { // 웹소켓 데이터
                        String priceStr = stockData.get("10").asText()
                                .replace(",", "")
                                .replace("+", "")
                                .replace("-", "")
                                .trim();
                        currentPrice = Long.parseLong(priceStr);
                    }
                } catch (NumberFormatException e) {
                    log.warn("종목 {} 현재가 파싱 오류", stockCode, e);
                }
            }

            if (currentPrice == 0L) {
                log.warn("종목 {} 현재가를 가져올 수 없어 평균매입가를 사용합니다.", stockCode);
                currentPrice = position.getAveragePrice(); // 현재가 정보가 없을 경우 평균 매입가 사용
            }

            // 금액 계산
            Long investmentAmount = position.getAveragePrice() * position.getQuantity();
            Long valuationAmount = currentPrice * position.getQuantity();
            Long profitLoss = valuationAmount - investmentAmount;
            Double returnRate = investmentAmount > 0
                    ? (profitLoss.doubleValue() / investmentAmount.doubleValue()) * 100
                    : 0.0;

            totalInvestment += investmentAmount;
            totalValuation += valuationAmount;

            PortfolioResponseDTO.StockPositionDTO positionDTO = PortfolioResponseDTO.StockPositionDTO.builder()
                    .stockCode(stockCode)
                    .stockName(position.getStockData().getShortName())
                    .quantity(position.getQuantity())
                    .averagePrice(position.getAveragePrice())
                    .currentPrice(currentPrice)
                    .valuationAmount(valuationAmount)
                    .profitLoss(profitLoss)
                    .returnRate(returnRate)
                    .build();

            positionDTOs.add(positionDTO);
        }

        Long totalProfitLoss = totalValuation - totalInvestment;
        Double totalReturnRate = totalInvestment > 0
                ? (totalProfitLoss.doubleValue() / totalInvestment.doubleValue()) * 100
                : 0.0;
        Long totalAsset = member.getMemberMoney() + totalValuation;

        return PortfolioResponseDTO.builder()
                .memberMoney(member.getMemberMoney())
                .totalAsset(totalAsset)
                .totalInvestment(totalInvestment)
                .totalValuation(totalValuation)
                .totalProfitLoss(totalProfitLoss)
                .totalReturnRate(totalReturnRate)
                .positions(positionDTOs)
                .updatedAt(LocalDateTime.now())
                .build();
    }

    /**
     * 회원이 보유한 종목 코드 목록을 조회합니다.
     * @param memberId 회원 ID
     * @return 보유 종목 코드 목록
     */
    @Transactional(readOnly = true)
    public List<String> getMemberStockCodes(Long memberId) {
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // active가 true인 항목만 조회
        List<HoldingPosition> positions = holdingPositionRepository.findByMemberAndActiveTrue(member);

        return positions.stream()
                .map(p -> p.getStockData().getShortCode())
                .collect(Collectors.toList());
    }
}