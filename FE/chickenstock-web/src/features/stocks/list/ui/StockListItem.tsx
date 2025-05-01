import { Heart } from "lucide-react";
import { useNavigate } from "react-router-dom";
import { StockProps } from "../model/types";
import useWatchlistStore from "@/features/watchlist/model/store";
import { useWatchlistToggle } from "@/features/watchlist/model/hooks";

const StockListItem = (props: StockProps) => {
  const { isInWatchlist } = useWatchlistStore();
  const { toggleWatchlist } = useWatchlistToggle();

  const isCurrentInWatchlist = isInWatchlist(props.stockCode);

  const navigate = useNavigate();

  const handleClick = () => {
    void navigate(`/stocks/${props.stockCode}`);
  };

  const renderSpecificColumns = () => {
    switch (props.rankingType) {
      case "tradeAmount":
        return (
          <>
            <div className="w-1/6 text-right">{Number(props.tradeAmount).toLocaleString()}</div>
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
              <span
                className={props.fluctuationRate.startsWith("+") ? "text-red-500" : "text-blue-500"}
              >
                {props.previousRatio}%
              </span>
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
            <div className="font-medium">{props.stockName}</div>
            <div className="text-sm text-gray-500">{props.stockCode}</div>
          </div>
        </div>
      </div>
      <div className="w-1/6 text-right">{Number(props.currentPrice).toLocaleString()}</div>
      <div className="w-1/6 text-right">
        <span className={props.fluctuationRate.startsWith("+") ? "text-red-500" : "text-blue-500"}>
          {(props.previousDayCompare.startsWith("-") ? "" : "+") +
            Number(props.previousDayCompare).toLocaleString()}
        </span>
      </div>
      <div className="w-1/6 text-right">
        <span className={props.fluctuationRate.startsWith("+") ? "text-red-500" : "text-blue-500"}>
          {props.fluctuationRate}%
        </span>
      </div>

      {renderSpecificColumns()}
    </div>
  );
};

export default StockListItem;
