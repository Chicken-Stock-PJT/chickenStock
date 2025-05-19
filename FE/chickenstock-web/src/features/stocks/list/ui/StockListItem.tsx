import { Heart } from "lucide-react";
import { useNavigate } from "react-router-dom";
import { StockProps } from "../model/types";
import useWatchlistStore from "@/features/watchlist/model/store";
import { useWatchlistToggle } from "@/features/watchlist/model/hooks";
import { isNxtStock } from "@/features/stocks/trade/model/nxtStocks";
import { useEffect, useState } from "react";

const StockListItem = (props: StockProps) => {
  const [isNxt, setIsNxt] = useState(false);
  const { isInWatchlist } = useWatchlistStore();
  const { toggleWatchlist } = useWatchlistToggle();

  const isCurrentInWatchlist = isInWatchlist(props.stockCode);

  const navigate = useNavigate();

  useEffect(() => {
    const checkNxtStock = async () => {
      const result = await isNxtStock(props.stockCode.slice(0, 6));
      setIsNxt(result);
    };
    void checkNxtStock();
  }, [props.stockCode]);

  const handleClick = () => {
    void navigate(`/stocks/${props.stockCode}`);
  };

  const renderSpecificColumns = () => {
    switch (props.rankingType) {
      case "tradeAmount":
        return (
          <>
            <div className="w-1/6 text-right">
              <span>{Number(props.tradeAmount).toLocaleString()}</span>
              <span className="text-sm text-gray-500">백만</span>
            </div>
            <div className="w-1/6 text-right">
              {Number(props.currentTradeVolume).toLocaleString()}
            </div>
          </>
        );
      case "volume":
        return (
          <>
            <div className="w-1/6 text-right">{Number(props.tradeVolume).toLocaleString()}</div>
            <div className="w-1/6 text-right">
              <span>{props.previousRatio}%</span>
            </div>
          </>
        );
      case "fluctuationRate":
        return (
          <>
            <div className="w-1/6 text-right hover:bg-gray-50" onClick={handleClick}>
              {props.contractStrength}
            </div>
            <div className="w-1/6 text-right hover:bg-gray-50" onClick={handleClick}>
              매수: {Number(props.buyRemaining).toLocaleString()}
              <br />
              매도: {Number(props.sellRemaining).toLocaleString()}
            </div>
          </>
        );
    }
  };

  return (
    <div
      className="flex cursor-pointer items-center justify-between border-b p-4 hover:bg-gray-50"
      onClick={handleClick}
    >
      <div className="flex w-1/4 items-center gap-4">
        <Heart
          fill={isCurrentInWatchlist ? "red" : "none"}
          stroke={isCurrentInWatchlist ? "red" : "black"}
          className={`cursor-pointer transition-opacity ${isCurrentInWatchlist ? "hover:opacity-20" : "hover:fill-gray-300 hover:stroke-none"}`}
          onClick={(e) => {
            e.stopPropagation();
            toggleWatchlist(props.stockCode);
          }}
        />
        <div className="flex w-full items-center gap-4">
          <p className="min-w-[32px] text-center font-semibold text-gray-600">{props.rank}</p>
          <div className="text-left">
            <div className="flex items-center gap-2">
              <div className="font-medium">{props.stockName}</div>
              {isNxt && (
                <div className="rounded-xl bg-primary-300 px-3 py-1 text-xs font-semibold text-gray-900">
                  NXT
                </div>
              )}
            </div>
            <div className="text-sm text-gray-500">{props.stockCode}</div>
          </div>
        </div>
      </div>
      <div className="w-1/6 text-right">
        {Math.abs(Number(props.currentPrice)).toLocaleString()}
      </div>
      <div className="w-1/6 text-right">
        <span
          className={
            props.fluctuationRate.startsWith("+")
              ? "text-chart-red"
              : props.fluctuationRate.startsWith("-")
                ? "text-chart-blue"
                : ""
          }
        >
          {(Number(props.previousDayCompare) > 0 ? "+" : "") +
            Number(props.previousDayCompare).toLocaleString()}
        </span>
      </div>
      <div className="w-1/6 text-right">
        <span
          className={
            props.fluctuationRate.startsWith("+")
              ? "text-chart-red"
              : props.fluctuationRate.startsWith("-")
                ? "text-chart-blue"
                : ""
          }
        >
          {props.fluctuationRate}%
        </span>
      </div>

      {renderSpecificColumns()}
    </div>
  );
};

export default StockListItem;
