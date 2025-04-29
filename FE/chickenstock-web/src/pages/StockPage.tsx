import { useParams } from "react-router-dom";
import Chart from "@/features/stocks/chart/ui/Chart";
import OrderBook from "@/features/stocks/orderBook/ui";
import Trade from "@/features/stocks/trade/ui/Trade";
import Status from "@/features/stocks/status/ui/Status";
import { useEffect, useState } from "react";
import { StockPriceData, WebSocketResponse } from "@/features/stocks/chart/model/types";
import { getStockInfo } from "@/features/stocks/api";

const StockPage = () => {
  const stockCode = useParams().stockCode?.slice(0, 6);
  const [stockName, setStockName] = useState<string>("");
  const [currentPrice, setCurrentPrice] = useState<string>("0");
  const [priceChange, setPriceChange] = useState<string>("0");
  const [changeRate, setChangeRate] = useState<string>("0");

  useEffect(() => {
    if (!stockCode) return;
    const fetchStockInfo = async () => {
      const res = await getStockInfo(stockCode);
      // setCurrentPrice(res.currentPrice);
      setStockName(res.shortName);
      setPriceChange(res.prevDayCompare);
      setChangeRate(res.fluctuationRate);
    };

    void fetchStockInfo();

    const ws = new WebSocket(`${import.meta.env.VITE_WS_BASE_URL}/stock`);

    ws.onopen = () => {
      console.log("WebSocket 연결 성공");
      ws.send(
        JSON.stringify({
          action: "subscribe",
          stockCode: stockCode,
        }),
      );
    };

    ws.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data as string) as WebSocketResponse;

        switch (data.type) {
          case "connected":
            console.log("서버 연결 성공:", data.message);
            break;

          case "success":
            console.log("구독 성공:", data.stockCode);
            break;

          case "stockPrice": {
            const priceData = data as StockPriceData;
            setCurrentPrice(priceData.currentPrice);
            setPriceChange(priceData.priceChange);
            setChangeRate(priceData.changeRate);
            break;
          }
        }
      } catch (error) {
        console.error("WebSocket 메시지 파싱 에러:", error);
      }
    };

    ws.onerror = (error) => {
      console.error("WebSocket 에러:", error);
    };

    ws.onclose = () => {
      console.log("WebSocket 연결 종료");
    };

    return () => {
      ws.send(
        JSON.stringify({
          action: "unsubscribe",
          stockCode: stockCode,
        }),
      );
      ws.close();
    };
  }, [stockCode]);

  return (
    <div className="absolute left-0 top-[60px] flex max-h-[calc(90vh)] w-screen flex-col overflow-hidden overflow-x-auto px-[10px]">
      <div className="flex grid flex-1 grid-cols-12 gap-2 overflow-hidden">
        <div className="col-span-8 flex flex-col gap-2 overflow-hidden">
          <div className="flex w-full items-center justify-center rounded-lg border bg-gray-100">
            <Chart
              stockName={stockName}
              stockCode={stockCode}
              priceData={{ currentPrice, priceChange, changeRate }}
            />
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
