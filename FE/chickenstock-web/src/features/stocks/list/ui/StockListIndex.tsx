import { RankingType } from "../model/types";

interface StockListIndexProps {
  rankingType: RankingType;
}

const StockListIndex = ({ rankingType }: StockListIndexProps) => {
  const getColumns = () => {
    const baseColumns = [
      { id: "stock", label: "종목", width: "w-1/4" },
      { id: "price", label: "현재가", width: "w-1/6" },
      { id: "compare", label: "전일대비", width: "w-1/6" },
      { id: "rate", label: "등락률", width: "w-1/6" },
    ];

    const specificColumns = {
      tradeAmount: [
        { id: "amount", label: "거래대금", width: "w-1/6" },
        { id: "volume", label: "거래량", width: "w-1/6" },
      ],
      volume: [
        { id: "volume", label: "거래량", width: "w-1/6" },
        { id: "ratio", label: "전일비", width: "w-1/6" },
      ],
      fluctuationRate: [
        { id: "strength", label: "체결강도", width: "w-1/6" },
        { id: "remaining", label: "잔량", width: "w-1/6" },
      ],
    } as const;

    return [...baseColumns, ...specificColumns[rankingType as keyof typeof specificColumns]];
  };

  return (
    <div className="sticky top-0 z-10 flex items-center justify-between rounded-t-lg bg-primary-50 p-4 text-gray-700">
      {getColumns().map((column) => (
        <div
          key={column.id}
          className={`${column.width} ${
            column.id === "stock" ? "pl-[84px] text-left" : "text-right"
          }`}
        >
          {column.label}
        </div>
      ))}
    </div>
  );
};

export default StockListIndex;
