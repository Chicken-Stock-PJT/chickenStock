import { useParams } from "react-router-dom";
import { useEffect, useState, useCallback, useMemo } from "react";
import Chart from "@/features/stocks/chart/ui/Chart";
import OrderBook from "@/features/stocks/orderBook/ui";
import Trade from "@/features/stocks/trade/ui/Trade";
import Status from "@/features/stocks/status/ui/Status";
import { getStockInfo, getStockPrice } from "@/features/stocks/api";
import { useWebSocketStore } from "@/shared/store/websocket";
import Transaction from "@/features/stocks/transaction/ui";

const StockPage = () => {
  const stockCode = useParams().stockCode?.slice(0, 6);
  const [stockName, setStockName] = useState<string>("");
  const [currentPrice, setCurrentPrice] = useState<string>("");
  const [priceChange, setPriceChange] = useState<string>("");
  const [changeRate, setChangeRate] = useState<string>("");

  const { connect, disconnect, stockPriceData } = useWebSocketStore();

  // 초기 주식 가격 데이터 가져오기
  const fetchInitialStockPrice = useCallback(async () => {
    if (!stockCode) return;
    try {
      const res = await getStockPrice(stockCode);
      setCurrentPrice(res.currentPrice);
      setPriceChange(res.priceChange);
      setChangeRate(res.changeRate);
    } catch (error) {
      console.error("Failed to fetch initial stock price:", error);
    }
  }, [stockCode]);

  // 종목 정보 가져오기
  const fetchStockInfo = useCallback(async () => {
    if (!stockCode) return;
    try {
      const res = await getStockInfo(stockCode);
      setStockName(res.shortName);
    } catch (error) {
      console.error("Failed to fetch stock info:", error);
    }
  }, [stockCode]);

  // 초기 데이터 로딩
  useEffect(() => {
    void fetchInitialStockPrice();
  }, [fetchInitialStockPrice]);

  // 웹소켓 연결 관리
  useEffect(() => {
    if (!stockCode) return;

    // 종목 정보 가져오기 및 웹소켓 연결
    void fetchStockInfo();
    connect(stockCode);

    // 언마운트 시 웹소켓 연결 해제
    return () => {
      disconnect();
    };
  }, [stockCode, connect, disconnect, fetchStockInfo]);

  // 현재 가격 정보 메모이제이션
  const priceData = useMemo(
    () => ({
      currentPrice: stockPriceData?.currentPrice ?? currentPrice,
      priceChange: stockPriceData?.priceChange ?? priceChange,
      changeRate: stockPriceData?.changeRate ?? changeRate,
    }),
    [stockPriceData, currentPrice, priceChange, changeRate],
  );

  // 현재 가격 숫자 값 메모이제이션
  const currentPriceNumber = useMemo(() => {
    return Number(stockPriceData?.currentPrice ?? "0");
  }, [stockPriceData]);

  return (
    <div className="absolute left-0 top-[60px] flex w-screen flex-col px-[10px]">
      <div className="mb-4 flex grid flex-1 grid-cols-12 gap-2">
        <div className="col-span-8 flex flex-col gap-2">
          <div className="flex w-full items-center justify-center rounded-lg border bg-gray-100">
            <Chart stockName={stockName} stockCode={stockCode} priceData={priceData} />
          </div>
          <div className="flex w-full gap-2">
            <div className="w-1/2">
              <OrderBook stockCode={stockCode ?? ""} currentPrice={currentPriceNumber} />
            </div>
            <div className="w-1/2 overflow-auto">
              <Transaction />
            </div>
          </div>
        </div>
        <div className="col-span-4 flex flex-col gap-2">
          <div className="flex w-full items-center justify-center rounded-lg border bg-gray-100">
            <Trade currentPrice={currentPriceNumber} stockCode={stockCode ?? ""} />
          </div>
          <div className="flex-1 overflow-auto">
            <Status stockCode={stockCode ?? ""} />
          </div>
        </div>
      </div>
    </div>
  );
};

export default StockPage;
