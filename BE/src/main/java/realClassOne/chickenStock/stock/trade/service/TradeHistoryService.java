package realClassOne.chickenStock.stock.trade.service;

import lombok.RequiredArgsConstructor;
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
import realClassOne.chickenStock.stock.trade.dto.response.TradeHistoryDTO;
import realClassOne.chickenStock.stock.trade.dto.response.TradeHistoriesCursorResponse;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TradeHistoryService {

    private final JwtTokenProvider jwtTokenProvider;
    private final TradeHistoryRepository tradeHistoryRepository;
    private final MemberRepository memberRepository;
    private final StockDataRepository stockDataRepository;

    public TradeHistoriesCursorResponse getTradeHistories(String authorizationHeader, String cursor, int size) {
        // 🔐 토큰에서 memberId 추출
        String token = jwtTokenProvider.resolveToken(authorizationHeader);
        Long memberId = jwtTokenProvider.getMemberIdFromToken(token);

        // 👤 Member 조회
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        // ⏱ 기준 시간 설정
        LocalDateTime baseTime = (cursor == null) ? LocalDateTime.now() : LocalDateTime.parse(cursor);

        // 📄 커서 기반 페이징: createdAt < baseTime 기준 size + 1개 조회
        PageRequest pageRequest = PageRequest.of(0, size + 1, Sort.by(Sort.Direction.DESC, "createdAt"));
        List<TradeHistory> histories = tradeHistoryRepository.findByMemberAndCreatedAtBefore(member, baseTime, pageRequest);


        // 🔁 다음 페이지 여부 판단
        boolean hasNext = histories.size() > size;
        if (hasNext) {
            histories.remove(histories.size() - 1);
        }

        // 📦 거래내역 DTO 변환
        List<TradeHistoryDTO> tradeHistoryDtos = histories.stream()
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

        // 💰 실현 손익 계산 (전체 거래 기준 FIFO)
        List<TradeHistory> allHistories = tradeHistoryRepository.findByMember(member);
        List<TradeHistory> buyHistories = allHistories.stream()
                .filter(h -> h.getTradeType() == TradeHistory.TradeType.BUY)
                .sorted((h1, h2) -> h1.getCreatedAt().compareTo(h2.getCreatedAt()))
                .collect(Collectors.toList());

        long realizedProfit = 0L;
        for (TradeHistory sell : allHistories) {
            if (!sell.getTradeType().equals(TradeHistory.TradeType.SELL)) continue;

            int remainingSellQuantity = sell.getQuantity();
            long sellUnitPrice = sell.getUnitPrice();

            for (TradeHistory buy : buyHistories) {
                if (buy.getQuantity() == 0) continue;
                if (!buy.getStockData().getShortCode().equals(sell.getStockData().getShortCode())) continue;

                int availableQuantity = buy.getQuantity();
                int matchedQuantity = Math.min(availableQuantity, remainingSellQuantity);

                realizedProfit += (sellUnitPrice - buy.getUnitPrice()) * matchedQuantity;

                buy.setQuantity(availableQuantity - matchedQuantity);
                remainingSellQuantity -= matchedQuantity;

                if (remainingSellQuantity == 0) break;
            }
        }

        // 🪝 다음 커서 값
        String nextCursor = hasNext
                ? histories.get(histories.size() - 1).getCreatedAt().toString()
                : null;

        return new TradeHistoriesCursorResponse(tradeHistoryDtos, realizedProfit, hasNext, nextCursor);
    }
}
