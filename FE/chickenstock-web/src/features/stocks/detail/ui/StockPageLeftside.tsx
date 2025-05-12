import Chart from "@/features/stocks/chart/ui/Chart";
import OrderBook from "@/features/stocks/orderBook/ui";

const StockPageLeftside = () => {
  return (
    <div className="basis-2/3 gap-2 space-y-2">
      <div className="flex w-full items-center justify-center rounded-lg border bg-gray-100">
        <Chart stockName={stockName} stockCode={stockCode} priceData={priceData} />
      </div>
      <div>
        <OrderBook stockCode={stockCode ?? ""} currentPrice={currentPriceNumber} />
      </div>
    </div>
  );
};
