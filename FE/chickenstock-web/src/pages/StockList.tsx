import { useEffect, useState } from "react";
import StockListIndex from "@/features/stocks/list/ui/StockListIndex";
import StockListItem from "@/features/stocks/list/ui/StockListItem";
import { MarketType, RankingType, StockProps } from "@/features/stocks/list/model/types";
import StockListHeader from "@/features/stocks/list/ui/StockListHeader";
import StockListSkeleton from "@/features/stocks/list/ui/StockListSkeleton";
import { getStockRanking } from "@/features/stocks/list/api";

const StockList = () => {
  const [stocks, setStocks] = useState<StockProps[]>([]);
  const [marketType, setMarketType] = useState<MarketType>("000");
  const [rankingType, setRankingType] = useState<RankingType>("tradeAmount");
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchStocks = async () => {
      setLoading(true);
      try {
        const response = await getStockRanking(rankingType, marketType);
        const formattedStocks = response.rankingItems.map((item, index: number) => ({
          ...item,
          rankingType: rankingType.toLowerCase() as RankingType,
          rank: index + 1,
        }));

        setStocks(formattedStocks as StockProps[]);
      } catch (error) {
        console.error("Error fetching stocks:", error);
      } finally {
        setLoading(false);
      }
    };

    void fetchStocks();
  }, [marketType, rankingType]);

  return (
    <div>
      <StockListHeader
        marketType={marketType}
        rankingType={rankingType}
        onMarketTypeChange={setMarketType}
        onRankingTypeChange={setRankingType}
      />
      <StockListIndex rankingType={rankingType} />
      {loading ? (
        <StockListSkeleton />
      ) : (
        stocks.map((stock, index) => {
          // rankingType에 따라 props를 올바르게 타입 지정
          const stockWithRank = {
            ...stock,
            stockCode: stock.stockCode.slice(0, 6), // stockCode의 앞 6자리만 사용
            rank: index + 1,
            rankingType, // 명시적으로 rankingType 지정
          } as StockProps; // StockProps로 타입 단언

          return <StockListItem key={stock.stockCode} {...stockWithRank} />;
        })
      )}
    </div>
  );
};

export default StockList;
