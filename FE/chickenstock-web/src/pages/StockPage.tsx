import Chart from "../features/stocks/chart/Chart";
import OrderBook from "../features/stocks/orderBook/OrderBook";
import Trade from "../features/stocks/trade/Trade";

const StockPage = () => {
  return (
    <div className="flex grid grid-cols-12 gap-6">
      <div className="flex col-span-8 flex-col gap-4">
        <div className="w-full aspect-video border rounded-lg bg-gray-100 flex items-center justify-center">
          <Chart />
        </div>
        <div>
          <OrderBook />
        </div>
      </div>
      <div className="col-span-4">
        <div className="w-full border rounded-lg bg-gray-100 items-center justify-center flex">
          <Trade />
        </div>
      </div>
    </div>
  );
};
export default StockPage;
