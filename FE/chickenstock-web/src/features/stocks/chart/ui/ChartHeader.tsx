import { Heart } from "lucide-react";
import { ChartHeaderProps } from "../model/types";
import useWatchlistStore from "@/features/watchlist/model/store";
import { useWatchlistToggle } from "@/features/watchlist/model/hooks";
import { useNavigate } from "react-router-dom";
import { useAuthStore } from "@/shared/store/auth";
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
  const navigate = useNavigate();
  const { isLoggedIn } = useAuthStore();
  const isCurrentInWatchlist = isInWatchlist(stockCode);

  return (
    <>
      <div className="flex justify-between">
        <div className="flex items-center gap-4">
          <img
            className="w-[50px] rounded-2xl lg:w-[78px]"
            src={`https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${stockCode}.png`}
            alt=""
          />
          <div className="flex flex-col lg:gap-2">
            <div className="flex items-end gap-2 text-left">
              <h1 className="text-lg font-bold text-gray-800 lg:text-xl">{stockName}</h1>
              <p className="text-sm text-gray-600 lg:text-base">{stockCode}</p>
              {/* 커뮤니티 버튼 추가 */}
              {isLoggedIn && (
                <span
                  onClick={() => {
                    void navigate(`/stocks/${stockCode}/community`, {
                      state: { stockName },
                    });
                  }}
                  className="inline-block cursor-pointer rounded-md bg-yellow-200 px-2 py-0.5 text-base text-gray-900 hover:bg-yellow-100 hover:text-gray-600"
                >
                  커뮤니티
                </span>
              )}
            </div>
            <div className={`flex gap-2 text-left`}>
              <p className="text-2xl font-semibold leading-none lg:text-3xl">
                {formattedPrice(currentPrice)}
              </p>
              <div
                className={`flex items-end items-center gap-2 text-sm lg:text-lg ${Number(priceChange) > 0 ? "text-chart-red" : Number(priceChange) < 0 ? "text-chart-blue" : "text-gray-800"}`}
              >
                <span className="leading-none">
                  {Number(priceChange) > 0 ? "▲" : Number(priceChange) < 0 ? "▼" : ""}{" "}
                  {formattedPrice(priceChange)}
                </span>
                <span className="leading-none text-opacity-25">({changeRate}%)</span>
              </div>
            </div>
          </div>
        </div>
        <div className="flex flex-col items-end justify-between">
          <Heart
            onClick={() => toggleWatchlist(stockCode)}
            fill={isCurrentInWatchlist ? "red" : "none"}
            stroke={isCurrentInWatchlist ? "red" : "black"}
            className="cursor-pointer"
          />
          {/* </div> */}
          <div className="mb-2 hidden items-end gap-1 lg:flex">
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
      <div className="mb-2 flex items-end gap-1 lg:hidden">
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
    </>
  );
};

export default ChartHeader;
