package realClassOne.chickenStock.stock.service;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class PortfolioService {

    private final MemberRepository memberRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final KiwoomWebSocketClient kiwoomWebSocketClient;
    private final StockSubscriptionService stockSubscriptionService;
    private final JwtTokenProvider jwtTokenProvider;

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

            List<HoldingPosition> positions = holdingPositionRepository.findByMember(member);

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

            List<HoldingPosition> positions = holdingPositionRepository.findByMember(member);

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
     * 회원과 보유 종목 정보로 포트폴리오 응답 DTO를 생성합니다.
     */
    private PortfolioResponseDTO buildPortfolioResponseDTO(Member member, List<HoldingPosition> positions) {
        List<PortfolioResponseDTO.StockPositionDTO> positionDTOs = new ArrayList<>();
        Long totalInvestment = 0L;
        Long totalValuation = 0L;

        for (HoldingPosition position : positions) {
            String stockCode = position.getStockData().getShortCode();

            // 종목 실시간 구독 등록 (아직 구독되지 않은 경우)
            if (!kiwoomWebSocketClient.isSubscribed(stockCode)) {
                try {
                    stockSubscriptionService.registerStockForSubscription(stockCode);
                } catch (Exception e) {
                    log.error("종목 구독 실패: {}", stockCode, e);
                    // 구독 실패해도 계속 진행
                }
            }

            // 최신 가격 정보 가져오기
            JsonNode priceData = kiwoomWebSocketClient.getLatestStockPriceData(stockCode);
            Long currentPrice = 0L;

            try {
                if (priceData != null && priceData.has("10")) {
                    currentPrice = Long.parseLong(priceData.get("10").asText().replace(",", ""));
                } else {
                    log.warn("종목 {}의 현재가 정보를 찾을 수 없습니다", stockCode);
                }
            } catch (Exception e) {
                log.error("종목 {}의 현재가 변환 중 오류 발생", stockCode, e);
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

        List<HoldingPosition> positions = holdingPositionRepository.findByMember(member);

        return positions.stream()
                .map(p -> p.getStockData().getShortCode())
                .collect(Collectors.toList());
    }
}