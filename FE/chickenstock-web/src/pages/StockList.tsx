import { useEffect, useRef, useState } from "react";
import StockListIndex from "@/features/stocks/list/ui/StockListIndex";
import StockListItem from "@/features/stocks/list/ui/StockListItem";
import { MarketType, RankingType, StockProps } from "@/features/stocks/list/model/types";
import StockListHeader from "@/features/stocks/list/ui/StockListHeader";
import StockListSkeleton from "@/features/stocks/list/ui/StockListSkeleton";
import { getStockRanking } from "@/features/stocks/list/api";

const StockList = () => {
  const [stocks, setStocks] = useState<StockProps[]>([]); // 현재 순위
  const [prevStocks, setPrevStocks] = useState<StockProps[]>([]); // 이전 순위
  const [marketType, setMarketType] = useState<MarketType>("000");
  const [rankingType, setRankingType] = useState<RankingType>("tradeAmount");
  const [sortType, setSortType] = useState<string | undefined>(undefined);
  const [loading, setLoading] = useState(true);
  const stockRefs = useRef<Record<string, HTMLDivElement | null>>({});
  const pollingInterval = useRef<NodeJS.Timeout>(null);

  const handleRankingTypeChange = (type: RankingType, newSortType?: string) => {
    setRankingType(type);
    setSortType(newSortType);
  };

  const fetchStocks = async () => {
    try {
      setLoading(true);
      const response = await getStockRanking(rankingType, marketType, sortType);
      const formattedStocks = response.rankingItems.map((item, index: number) => ({
        ...item,
        rankingType: rankingType.toLowerCase() as RankingType,
        rank: index + 1,
      }));

      console.log(formattedStocks);
      setPrevStocks(stocks);
      setStocks(formattedStocks as StockProps[]);
    } catch (error) {
      console.error("Error fetching stocks:", error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    void fetchStocks();

    pollingInterval.current = setInterval(() => {
      void fetchStocks();
    }, 30000);

    return () => {
      if (pollingInterval.current) {
        clearInterval(pollingInterval.current);
      }
    };
  }, [marketType, rankingType, sortType]);

  useEffect(() => {
    stocks.forEach((stock) => {
      const prevStock = prevStocks.find((prevStock) => prevStock.stockCode === stock.stockCode);
      const element = stockRefs.current[stock.stockCode];

      if (!element) return;

      if (!prevStock) {
        element.style.animation = "fadeIn";
      } else {
        const rankDiff = stock.rank - prevStock.rank;
        if (rankDiff > 0) {
          element.style.animation = "slideUp";
        } else if (rankDiff < 0) {
          element.style.animation = "slideDown";
        }
      }
    });
  }, [stocks, prevStocks]);

  return (
    <div>
      <StockListHeader
        marketType={marketType}
        rankingType={rankingType}
        sortType={sortType}
        onMarketTypeChange={setMarketType}
        onRankingTypeChange={handleRankingTypeChange}
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

          return (
            <div
              key={stock.stockCode}
              ref={(el) => {
                stockRefs.current[stock.stockCode] = el;
              }}
              className="transition-all duration-300 ease-in-out"
            >
              <StockListItem {...stockWithRank} />
            </div>
          );
        })
      )}
    </div>
  );
};

export default StockList;
