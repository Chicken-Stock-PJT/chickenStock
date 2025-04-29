import { Heart } from "lucide-react";
import { useState } from "react";
import { useNavigate } from "react-router-dom";
import { StockProps } from "../model/types";

const StockListItem = (props: StockProps) => {
  console.log(props);
  const navigate = useNavigate();
  const [like, setLike] = useState<boolean>(false);

  const handleClick = () => navigate(`/stocks/${props.stockCode}`);

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
            <div className="w-1/6 text-right">{props.contractStrength}</div>
            <div className="w-1/6 text-right">
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
      onClick={() => void handleClick()}
    >
      <div className="flex w-1/4 items-center gap-4">
        <Heart
          className="cursor-pointer"
          fill={like ? "red" : "none"}
          stroke={like ? "red" : "black"}
          onClick={(e) => {
            e.stopPropagation();
            setLike(!like);
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
