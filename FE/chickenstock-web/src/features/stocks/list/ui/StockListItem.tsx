import { Heart } from "lucide-react";
import { useNavigate } from "react-router-dom";
import { StockProps } from "../model/types";
import useWatchlistStore from "@/features/watchlist/model/store";
import { useWatchlistToggle } from "@/features/watchlist/model/hooks";
import { isNxtStock } from "@/features/stocks/trade/model/nxtStocks";
import { useEffect, useState } from "react";
import { ChevronRight } from "lucide-react";

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

  // 데스크탑 버전의 특정 열 렌더링
  const renderSpecificColumns = () => {
    switch (props.rankingType) {
      case "tradeAmount":
        return (
          <>
            <div className="hidden w-1/6 text-right lg:block">
              <span>{Number(props.tradeAmount).toLocaleString()}</span>
              <span className="text-sm text-gray-500">백만</span>
            </div>
            <div className="hidden w-1/6 text-right lg:block">
              {Number(props.currentTradeVolume).toLocaleString()}
            </div>
          </>
        );
      case "volume":
        return (
          <>
            <div className="hidden w-1/6 text-right lg:block">
              {Number(props.tradeVolume).toLocaleString()}
            </div>
            <div className="hidden w-1/6 text-right lg:block">
              <span>{props.previousRatio}%</span>
            </div>
          </>
        );
      case "fluctuationRate":
        return (
          <>
            <div className="hidden w-1/6 text-right lg:block">{props.contractStrength}</div>
            <div className="hidden w-1/6 text-right lg:block">
              <span>{props.buyRemaining}</span>
              <br />
              <span>{props.sellRemaining}</span>
            </div>
          </>
        );
    }
  };

  // 모바일 버전의 특정 데이터 렌더링
  const renderMobileSpecificInfo = () => {
    switch (props.rankingType) {
      case "tradeAmount":
        return (
          <>
            <div className="flex flex-col items-end gap-1">
              <div className="text-sm text-gray-500">거래대금(백만)</div>
              <div className="text-right">
                <span>{Number(props.tradeAmount).toLocaleString()}</span>
              </div>
            </div>
          </>
        );
      case "volume":
        return (
          <>
            <div className="flex flex-col items-end gap-1">
              <div className="text-sm text-gray-500">거래량</div>
              <div className="text-right">
                <span>{Number(props.tradeVolume).toLocaleString()}</span>
              </div>
            </div>
          </>
        );
      case "fluctuationRate":
        return (
          <>
            <div className="flex flex-col items-end gap-1">
              <div className="text-sm text-gray-500">체결강도</div>
              <div className="text-right">
                <span>{props.contractStrength}</span>
              </div>
            </div>
          </>
        );
      default:
        return null;
    }
  };

  return (
    <div className="border-b hover:bg-gray-50" onClick={handleClick}>
      {/* 데스크탑 뷰 */}
      <div className="hidden cursor-pointer items-center justify-between p-4 lg:flex">
        <div className="flex w-1/4 items-center gap-4">
          <Heart
            fill={isCurrentInWatchlist ? "red" : "none"}
            stroke={isCurrentInWatchlist ? "red" : "black"}
            className={`cursor-pointer transition-opacity ${
              isCurrentInWatchlist ? "hover:opacity-20" : "hover:fill-gray-300 hover:stroke-none"
            }`}
            onClick={(e) => {
              e.stopPropagation();
              toggleWatchlist(props.stockCode);
            }}
          />
          <div className="flex w-full items-center gap-4">
            <p className="min-w-[16px] text-center font-semibold text-gray-600">{props.rank}</p>
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
        <div className="w-1/4 text-right lg:w-1/6">
          {Math.abs(Number(props.currentPrice)).toLocaleString()}
        </div>
        <div className="w-1/6 text-right lg:w-1/6">
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

      {/* 모바일 뷰 */}
      <div className="cursor-pointer p-4 lg:hidden">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <Heart
              fill={isCurrentInWatchlist ? "red" : "none"}
              stroke={isCurrentInWatchlist ? "red" : "black"}
              className={`cursor-pointer transition-opacity ${
                isCurrentInWatchlist ? "hover:opacity-20" : "hover:fill-gray-300 hover:stroke-none"
              }`}
              onClick={(e) => {
                e.stopPropagation();
                toggleWatchlist(props.stockCode);
              }}
            />
            <div className="flex flex-col items-start gap-1">
              <div className="flex items-center gap-2">
                <div className="font-medium">{props.stockName}</div>
                {isNxt && (
                  <div className="rounded-xl bg-primary-300 px-2 py-0.5 text-xs font-semibold text-gray-900">
                    NXT
                  </div>
                )}
              </div>
              <div className="flex items-center gap-2 text-sm">
                <span className="font-medium">
                  {Math.abs(Number(props.currentPrice)).toLocaleString()}원
                </span>
                <span
                  className={`text-xs ${
                    props.fluctuationRate.startsWith("+")
                      ? "text-chart-red"
                      : props.fluctuationRate.startsWith("-")
                        ? "text-chart-blue"
                        : ""
                  }`}
                >
                  {props.fluctuationRate}%
                </span>
              </div>
            </div>
          </div>
          <div className="flex items-center gap-2">
            {renderMobileSpecificInfo()}
            <ChevronRight className="size-5 text-gray-400" />
          </div>
        </div>
        {/* 
        <div className="mt-3 grid grid-cols-3 gap-2">
          <div className="text-sm text-gray-500">현재가</div>
          <div className="col-span-2 text-right font-medium">
            {Math.abs(Number(props.currentPrice)).toLocaleString()}원
          </div>

          <div className="text-sm text-gray-500">전일비</div>
          <div className={`col-span-2 text-right ${getFluctuationColor()}`}>
            {(Number(props.previousDayCompare) > 0 ? "+" : "") +
              Number(props.previousDayCompare).toLocaleString()}
            원
          </div>

          <div className="text-sm text-gray-500">등락률</div>
          <div className={`col-span-2 text-right ${getFluctuationColor()}`}>
            {props.fluctuationRate}%
          </div>
        </div> */}
      </div>
    </div>
  );
};

export default StockListItem;
