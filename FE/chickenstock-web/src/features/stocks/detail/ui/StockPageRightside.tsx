import Status from "@/features/stocks/status/ui/Status";
import Trade from "@/features/stocks/trade/ui/Trade";

const StockPageRightside = () => {
  return (
    <div className="flex basis-1/3 flex-col gap-2">
      <div className="flex w-full items-center justify-center rounded-lg border bg-gray-100">
        <Trade currentPrice={currentPriceNumber} stockCode={stockCode ?? ""} />
      </div>
      <div className="min-h-[400px]">
        <Status stockCode={stockCode ?? ""} />
      </div>
    </div>
  );
};

export default StockPageRightside;
