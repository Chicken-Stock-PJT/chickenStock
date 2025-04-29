package realClassOne.chickenStock.stock.trade.service;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
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

    public TradeHistoriesResponse getTradeHistories(String authorizationHeader, int page, int size) {
        // 토큰에서 memberId 추출
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // memberId로 Member 객체 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // member로 전체 거래내역 조회 (실현 손익 계산용)
        List<TradeHistory> allHistories = tradeHistoryRepository.findByMember(member);

        // 무한스크롤용 페이징 처리 추가
        PageRequest pageRequest = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "createdAt"));
        Page<TradeHistory> pagedHistories = tradeHistoryRepository.findByMember(member, pageRequest);

        // 페이지별 거래내역만 변환
        List<TradeHistoryDTO> tradeHistoryDtos = pagedHistories.stream()
                .map(history -> {
                    StockData stockData = stockDataRepository.findByShortCode(history.getStockData().getShortCode())
                            .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));
                    return new TradeHistoryDTO(
                            stockData.getShortName(),
                            history.getTradeType().name(),
                            history.getQuantity(),
                            history.getUnitPrice(),
                            history.getCreatedAt(),
                            history.getTradedAt()
                    );
                })
                .collect(Collectors.toList());

        // FIFO 실현 손익 계산 로직
        // 실현 손익은 전체 거래 기준으로 계산
        List<TradeHistory> buyHistories = allHistories.stream()
                .filter(history -> history.getTradeType() == TradeHistory.TradeType.BUY)
                .sorted((h1, h2) -> h1.getCreatedAt().compareTo(h2.getCreatedAt()))
                .collect(Collectors.toList());

        long realizedProfit = 0L;

        for (TradeHistory sell : allHistories) {
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
