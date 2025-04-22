import { useEffect, useState } from "react";
import StockListIndex from "../features/stocks/list/StockListIndex";
import StockListItem from "../features/stocks/list/StockListItem";
import {
  MarketType,
  RankingType,
  StockProps,
} from "../features/stocks/list/types";
import StockListHeader from "../features/stocks/list/StockListHeader";
import StockListSkeleton from "../features/stocks/list/StockListSkeleton";

const StockList = () => {
  const [stocks, setStocks] = useState<StockProps[]>([]);
  const [marketType, setMarketType] = useState<MarketType>("000");
  const [rankingType, setRankingType] = useState<RankingType>("tradeAmount");
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchStocks = async () => {
      setLoading(true);
      try {
        const response = await fetch(
          `/api/stock/ranking/${rankingType}?marketType=${marketType}&includeManagement=1`
        );
        const data = await response.json();
        setStocks(data.rankingItems);
      } catch (error) {
        console.error("Error fetching stocks:", error);
      } finally {
        setLoading(false);
      }
    };

    fetchStocks();
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
        stocks.map((stock, index) => (
          <StockListItem
            key={index}
            {...stock}
            rankingType={rankingType}
            rank={index + 1}
          />
        ))
      )}
    </div>
  );
};

export default StockList;
