import { useParams } from "react-router-dom";
import Chart from "@/features/stocks/chart/ui/Chart";
import OrderBook from "@/features/stocks/orderBook/ui";
import Trade from "@/features/stocks/trade/ui/Trade";
import Status from "@/features/stocks/status/ui/Status";
import { useEffect, useState } from "react";
import { getStockInfo } from "@/features/stocks/api";
import { useWebSocketStore } from "@/shared/store/websocket";

const StockPage = () => {
  const stockCode = useParams().stockCode?.slice(0, 6);
  const [stockName, setStockName] = useState<string>("");

  const { connect, disconnect, stockPriceData } = useWebSocketStore();

  // 종목 정보 조회
  useEffect(() => {
    if (!stockCode) return;

    const fetchStockInfo = async () => {
      const res = await getStockInfo(stockCode);
      setStockName(res.shortName);
    };

    // 마운트 시 종목 정보 조회 및 웹소켓 연결
    void fetchStockInfo();
    connect(stockCode);

    // 언마운트 시 웹소켓 연결 해제
    return () => {
      disconnect();
    };
  }, [stockCode, connect, disconnect]);

  return (
    <div className="absolute left-0 top-[60px] flex max-h-[calc(90vh)] w-screen flex-col overflow-hidden overflow-x-auto px-[10px]">
      <div className="flex grid flex-1 grid-cols-12 gap-2 overflow-hidden">
        <div className="col-span-8 flex flex-col gap-2 overflow-hidden">
          <div className="flex w-full items-center justify-center rounded-lg border bg-gray-100">
            <Chart
              stockName={stockName}
              stockCode={stockCode}
              priceData={{
                currentPrice: stockPriceData?.currentPrice ?? "0",
                priceChange: stockPriceData?.priceChange ?? "0",
                changeRate: stockPriceData?.changeRate ?? "0",
              }}
            />
          </div>
          <div className="flex-1 overflow-auto">
            <OrderBook
              stockCode={stockCode ?? ""}
              currentPrice={Number(stockPriceData?.currentPrice ?? "0")}
            />
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
