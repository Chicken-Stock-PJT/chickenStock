import { Heart } from "lucide-react";
import { ChartHeaderProps } from "../model/types";
import useWatchlistStore from "@/features/watchlist/model/store";
import { useWatchlistToggle } from "@/features/watchlist/model/hooks";
import {
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from "@/shared/libs/ui/dropdown-menu";
import { DropdownMenu } from "@/shared/libs/ui/dropdown-menu";

const ChartHeader = ({
  stockName,
  stockCode,
  currentPrice,
  priceChange,
  changeRate,
  onChartTypeChange,
  selectedChartType,
  timeInterval,
  onTimeIntervalChange,
}: ChartHeaderProps) => {
  const formattedPrice = (price: string) => Math.abs(Number(price)).toLocaleString();
  const { isInWatchlist } = useWatchlistStore();
  const { toggleWatchlist } = useWatchlistToggle();

  const isCurrentInWatchlist = isInWatchlist(stockCode);

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
          <div className={`flex gap-2 text-left`}>
            <p className="text-3xl font-semibold leading-none">{formattedPrice(currentPrice)}</p>
            <div
              className={`flex items-end items-center gap-2 ${Number(priceChange) > 0 ? "text-chart-red" : Number(priceChange) < 0 ? "text-chart-blue" : "text-gray-800"}`}
            >
              <span className="text-lg leading-none">
                {Number(priceChange) > 0 ? "▲" : Number(priceChange) < 0 ? "▼" : ""}{" "}
                {formattedPrice(priceChange)}
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
          <DropdownMenu>
            <DropdownMenuTrigger className="rounded p-0 text-xs">
              <button
                className={`rounded px-2 py-1 text-xs ${
                  selectedChartType === "MINUTE" ? "bg-blue-100 text-blue-600" : "bg-gray-100"
                }`}
              >
                {timeInterval}분
              </button>
            </DropdownMenuTrigger>
            <DropdownMenuContent>
              <DropdownMenuItem onClick={() => onTimeIntervalChange("1")}>1분</DropdownMenuItem>
              <DropdownMenuItem onClick={() => onTimeIntervalChange("5")}>5분</DropdownMenuItem>
              <DropdownMenuItem onClick={() => onTimeIntervalChange("10")}>10분</DropdownMenuItem>
              <DropdownMenuItem onClick={() => onTimeIntervalChange("15")}>15분</DropdownMenuItem>
              <DropdownMenuItem onClick={() => onTimeIntervalChange("30")}>30분</DropdownMenuItem>
              <DropdownMenuItem onClick={() => onTimeIntervalChange("60")}>60분</DropdownMenuItem>
            </DropdownMenuContent>
          </DropdownMenu>
          <button
            className={`rounded px-2 py-1 text-xs ${
              selectedChartType === "DAILY" ? "bg-blue-100 text-blue-600" : "bg-gray-100"
            }`}
            onClick={() => onChartTypeChange("DAILY")}
          >
            일
          </button>
          <button
            className={`rounded px-2 py-1 text-xs ${
              selectedChartType === "WEEKLY" ? "bg-blue-100 text-blue-600" : "bg-gray-100"
            }`}
            onClick={() => onChartTypeChange("WEEKLY")}
          >
            주
          </button>
          <button
            className={`rounded px-2 py-1 text-xs ${
              selectedChartType === "MONTHLY" ? "bg-blue-100 text-blue-600" : "bg-gray-100"
            }`}
            onClick={() => onChartTypeChange("MONTHLY")}
          >
            월
          </button>
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
