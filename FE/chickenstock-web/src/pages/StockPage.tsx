import { useParams } from "react-router-dom";
import Chart from "../features/stocks/chart/Chart";
import OrderBook from "../features/stocks/orderBook/OrderBook";
import Trade from "../features/stocks/trade/Trade";
import Status from "@/features/stocks/status/Status";

const StockPage = () => {
  const stockCode = useParams().stockCode?.slice(0, 6);

  return (
    <div className="px-[10px] flex absolute overflow-x-auto top-[60px] left-0 w-screen max-h-[calc(90vh)] flex-col overflow-hidden">
      <div className="flex flex-1 grid grid-cols-12 gap-2 overflow-hidden">
        <div className="col-span-8 flex flex-col gap-2 overflow-hidden">
          <div className="flex w-full items-center justify-center rounded-lg border bg-gray-100">
            <Chart stockCode={stockCode} />
          </div>
          <div className="flex-1 overflow-auto">
            <OrderBook />
          </div>
        </div>
        <div className="col-span-4 flex flex-col gap-2 overflow-hidden">
          <div className="flex w-full items-center justify-center rounded-lg border bg-gray-100">
            <Trade />
          </div>
          <div className="flex-1 overflow-auto">
            <Status />
          </div>
        </div>
      </div>
    </div>
  );
};

export default StockPage;
