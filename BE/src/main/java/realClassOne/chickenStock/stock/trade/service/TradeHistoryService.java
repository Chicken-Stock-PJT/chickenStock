package realClassOne.chickenStock.stock.trade.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;
import realClassOne.chickenStock.security.jwt.JwtTokenProvider;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.entity.TradeHistory;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;
import realClassOne.chickenStock.stock.repository.TradeHistoryRepository;
import realClassOne.chickenStock.stock.repository.HoldingPositionRepository;
import realClassOne.chickenStock.stock.trade.dto.response.TradeHistoryDTO;
import realClassOne.chickenStock.stock.trade.dto.response.TradeHistoriesResponse;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TradeHistoryService {

    private final JwtTokenProvider jwtTokenProvider;
    private final TradeHistoryRepository tradeHistoryRepository;
    private final HoldingPositionRepository holdingPositionRepository;
    private final MemberRepository memberRepository;
    private final StockDataRepository stockDataRepository;

    public TradeHistoriesResponse getTradeHistories(String authorizationHeader) {
        // 토큰에서 memberId 추출
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // memberId로 Member 객체 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // member로 거래내역 조회
        List<TradeHistory> histories = tradeHistoryRepository.findByMember(member);

        List<TradeHistoryDTO> tradeHistoryDtos = histories.stream()
                .map(history -> {
                    StockData stockData = stockDataRepository.findByShortCode(history.getStockData().getShortCode())
                            .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));
                    return new TradeHistoryDTO(
                            stockData.getShortName(),
                            history.getTradeType().name(),  // ENUM 타입을 문자열로 변환
                            history.getQuantity(),
                            history.getUnitPrice(),
                            history.getCreatedAt(),
                            history.getTradedAt()
                    );
                })
                .collect(Collectors.toList());

        // FIFO 실현 손익 계산 로직
        List<TradeHistory> buyHistories = tradeHistoryRepository.findByMemberAndTradeTypeOrderByCreatedAtAsc(member, TradeHistory.TradeType.BUY);

        long realizedProfit = 0L;

        for (TradeHistory sell : histories) {
            if (!sell.getTradeType().equals(TradeHistory.TradeType.SELL)) continue;

            int remainingSellQuantity = sell.getQuantity();
            long sellUnitPrice = sell.getUnitPrice();

            for (TradeHistory buy : buyHistories) {
                if (buy.getQuantity() == 0) continue;

                // 종목이 다르면 스킵
                if (!buy.getStockData().getShortCode().equals(sell.getStockData().getShortCode())) continue;

                int availableQuantity = buy.getQuantity();
                int matchedQuantity = Math.min(availableQuantity, remainingSellQuantity);

                realizedProfit += (sellUnitPrice - buy.getUnitPrice()) * matchedQuantity;

                buy.setQuantity(availableQuantity - matchedQuantity);
                remainingSellQuantity -= matchedQuantity;

                if (remainingSellQuantity == 0) break;
            }
        }

        return new TradeHistoriesResponse(tradeHistoryDtos, realizedProfit);
    }
}
