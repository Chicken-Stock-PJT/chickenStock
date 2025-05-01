import { Heart } from "lucide-react";
import { ChartHeaderProps } from "../model/types";
import useWatchlistStore from "@/features/watchlist/model/store";
import { useWatchlistToggle } from "@/features/watchlist/model/hooks";

const ChartHeader = ({
  stockName,
  stockCode,
  currentPrice,
  priceChange,
  changeRate,
  onChartTypeChange,
  selectedChartType,
}: ChartHeaderProps) => {
  const formattedPrice = (price: string) => Number(price.slice(1)).toLocaleString();
  const { isInWatchlist } = useWatchlistStore();
  const { toggleWatchlist } = useWatchlistToggle();

  const isCurrentInWatchlist = isInWatchlist(stockCode);
  const isPriceUp = priceChange.startsWith("+");

  return (
    <div className="flex justify-between">
      <div className="flex items-center gap-4">
        <img
          className="w-[78px] rounded-2xl"
          src={`https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${stockCode}.png`}
          alt=""
        />
        <div className="flex flex-col gap-2">
          <div className="flex items-end gap-2 text-left">
            <h1 className="text-xl font-bold text-gray-800">{stockName}</h1>
            <p className="text-gray-600">{stockCode}</p>
          </div>
          <div className="flex gap-2 text-left text-chart-blue">
            <p className="text-3xl font-semibold leading-none">{formattedPrice(currentPrice)}</p>
            <div className="flex items-end items-center gap-2">
              <span className="text-lg leading-none">
                {isPriceUp ? "▲" : "▼"} {formattedPrice(priceChange)}
              </span>
              <span className="leading-none text-opacity-25">({changeRate}%)</span>
            </div>
          </div>
        </div>
      </div>
      <div className="flex flex-col items-end justify-between gap-2">
        <div>
          <Heart
            onClick={() => toggleWatchlist(stockCode)}
            fill={isCurrentInWatchlist ? "red" : "none"}
            stroke={isCurrentInWatchlist ? "red" : "black"}
          />
        </div>
        <div className="flex items-end gap-1">
          <button
            className={`rounded px-2 py-1 text-xs ${
              selectedChartType === "MINUTE" ? "bg-blue-100 text-blue-600" : "bg-gray-100"
            }`}
            onClick={() => onChartTypeChange("MINUTE")}
          >
            분
          </button>
          <button
            className={`rounded px-2 py-1 text-xs ${
              selectedChartType === "DAILY" ? "bg-blue-100 text-blue-600" : "bg-gray-100"
            }`}
            onClick={() => onChartTypeChange("DAILY")}
          >
            일
          </button>
          {/* <button
          className="rounded- bg-gray-100 py-1 text-xs"
          onClick={() => onChartTypeChange("WEEKLY")}
          >
          주
          </button>
          <button
          className="rounded- bg-gray-100 py-1 text-xs"
          onClick={() => onChartTypeChange("MONTHLY")}
          >
          월
          </button> */}
          <button
            className={`rounded px-2 py-1 text-xs ${
              selectedChartType === "YEARLY" ? "bg-blue-100 text-blue-600" : "bg-gray-100"
            }`}
            onClick={() => onChartTypeChange("YEARLY")}
          >
            연
          </button>
        </div>
      </div>
    </div>
  );
};

export default ChartHeader;
