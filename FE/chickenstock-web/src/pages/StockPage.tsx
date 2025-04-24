import { useParams } from "react-router-dom";
import Chart from "../features/stocks/chart/Chart";
import OrderBook from "../features/stocks/orderBook/OrderBook";
import Trade from "../features/stocks/trade/Trade";
import Status from "@/features/stocks/status/Status";

const StockPage = () => {
  const stockCode = useParams().stockCode?.slice(0, 6);

  return (
    <div className="flex grid max-h-screen grid-cols-12 gap-6 overflow-auto">
      <div className="col-span-8 flex flex-col gap-4">
        <div className="flex aspect-video w-full items-center justify-center rounded-lg border bg-gray-100">
          <Chart stockCode={stockCode} />
        </div>
        <div>
          <OrderBook />
        </div>
      </div>
      <div className="col-span-4 flex flex-col gap-4">
        <div className="flex w-full items-center justify-center rounded-lg border bg-gray-100">
          <Trade />
        </div>
        <div>
          <Status />
        </div>
      </div>
    </div>
  );
};
export default StockPage;
