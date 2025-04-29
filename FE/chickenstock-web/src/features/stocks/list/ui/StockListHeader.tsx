import { TrendingUp, DollarSign, BarChart3 } from "lucide-react";
import { MarketType, RankingType, StockListHeaderProps } from "./types";

const StockListHeader = ({
  marketType,
  rankingType,
  onMarketTypeChange,
  onRankingTypeChange,
}: StockListHeaderProps) => {
  const marketTypes = [
    { id: "000", name: "전체" },
    { id: "001", name: "코스피" },
    { id: "101", name: "코스닥" },
  ];

  const rankingTypes = [
    {
      id: "tradeAmount",
      name: "거래대금",
      icon: <DollarSign className="size-4" />,
    },
    {
      id: "volume",
      name: "거래량",
      icon: <BarChart3 className="size-4" />,
    },
    {
      id: "fluctuationRate",
      name: "등락률",
      icon: <TrendingUp className="size-4" />,
    },
  ];

  return (
    <div className="mb-4 rounded-lg bg-white p-4 text-left shadow-sm">
      <div className="space-y-4">
        <div className="flex flex-col gap-4 sm:flex-row sm:items-center">
          <span className="w-20 text-sm font-medium text-gray-500">시장</span>
          <div className="flex gap-2">
            {marketTypes.map((type) => (
              <button
                key={type.id}
                className={`
                    flex-1 rounded-full px-4 py-2 text-sm font-medium transition-all
                    duration-200 ease-in-out sm:flex-none
                    ${
                      marketType === type.id
                        ? "bg-primary-300 text-gray-800 shadow-md"
                        : "bg-gray-50 text-gray-600 hover:bg-gray-100"
                    }
                  `}
                onClick={() => onMarketTypeChange(type.id as MarketType)}
              >
                {type.name}
              </button>
            ))}
          </div>
        </div>

        <div className="flex flex-col gap-4 sm:flex-row sm:items-center">
          <span className="w-20 text-sm font-medium text-gray-500">정렬 기준</span>
          <div className="flex gap-2">
            {rankingTypes.map((type) => (
              <button
                key={type.id}
                className={`
                    inline-flex flex-1 items-center justify-center gap-2 rounded-full px-4
                    py-2 text-sm font-medium
                    transition-all duration-200 ease-in-out sm:flex-none
                    ${
                      rankingType === type.id
                        ? "bg-primary-300 text-gray-800 shadow-md"
                        : "bg-gray-50 text-gray-600 hover:bg-gray-100"
                    }
                  `}
                onClick={() => onRankingTypeChange(type.id as RankingType)}
              >
                {type.icon}
                {type.name}
              </button>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
};

export default StockListHeader;
