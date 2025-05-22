import { useParams } from "react-router-dom";
import { useEffect, useMemo } from "react";
import Chart from "@/features/stocks/chart/ui/Chart";
import OrderBook from "@/features/stocks/orderBook/ui";
import Trade from "@/features/stocks/trade/ui/Trade";
import Status from "@/features/stocks/status/ui/Status";
import { useWebSocketStore } from "@/shared/store/websocket";
import Transaction from "@/features/stocks/transaction/ui";
import { useStockInfo, useStockPrice } from "@/features/stocks/queries";
import { useOrderBook } from "@/features/stocks/orderBook/model/queries";
import { useGetAvailableQuantity, useGetIsNxtStock } from "@/features/stocks/trade/model/queries";

const StockPage = () => {
  const stockCode = useParams().stockCode?.slice(0, 6);

  // REST API 데이터 fetch (부모 컴포넌트에서 한 번만 호출)
  const { data: initialPriceData } = useStockPrice(stockCode ?? "");
  const { data: stockInfoData } = useStockInfo(stockCode ?? "");
  const { data: initialOrderBookData } = useOrderBook(stockCode ?? "");
  const { data: maxSellQty } = useGetAvailableQuantity(stockCode ?? "");
  const { data: isNxt } = useGetIsNxtStock(stockCode ?? "");

  // 실시간 WebSocket 데이터는 zustand에서 selector로 구독
  const connect = useWebSocketStore((state) => state.connect);
  const disconnect = useWebSocketStore((state) => state.disconnect);
  const stockPriceData = useWebSocketStore((state) => state.stockPriceData);
  const orderBookData = useWebSocketStore((state) => state.orderBookData);
  const tradeExecutionData = useWebSocketStore((state) => state.tradeExecutionData);

  // 웹소켓 연결 관리
  useEffect(() => {
    if (!stockCode) return;
    connect(stockCode);
    return () => disconnect();
  }, [stockCode, connect, disconnect]);

  // 현재가 데이터 메모이제이션
  const currentPrice = useMemo(() => {
    const price = Math.abs(
      Number(stockPriceData?.currentPrice ?? initialPriceData?.currentPrice ?? "0"),
    );
    return price;
  }, [stockPriceData?.currentPrice, initialPriceData?.currentPrice]);

  // 주식 정보 메모이제이션
  const stockInfo = useMemo(
    () => ({
      stockCode: stockCode ?? "",
      stockName: stockInfoData?.shortName ?? "",
      currentPrice: currentPrice.toString(),
      priceChange: stockPriceData?.priceChange ?? initialPriceData?.priceChange ?? "0",
      changeRate: stockPriceData?.changeRate ?? initialPriceData?.changeRate ?? "0",
    }),
    [stockCode, stockInfoData?.shortName, currentPrice, stockPriceData, initialPriceData],
  );

  // 호가 데이터 메모이제이션
  const orderBookInfo = useMemo(
    () => ({
      initialData: initialOrderBookData,
      realTimeData: orderBookData,
    }),
    [initialOrderBookData, orderBookData],
  );

  return (
    <div className="absolute left-0 top-[60px] flex w-screen flex-col gap-2 px-[10px] sm:flex-row">
      {/* 데스크톱: 좌측 2/3 영역 */}
      <div className="flex w-full flex-col gap-2 sm:w-2/3">
        {/* 차트 영역 */}
        <Chart
          stockCode={stockInfo.stockCode}
          stockName={stockInfo.stockName}
          priceData={stockInfo}
          stockPriceData={stockPriceData ?? undefined}
          tradeExecutionData={tradeExecutionData ?? undefined}
        />

        {/* 호가 및 거래 내역 영역 */}
        <div className="mb-4 hidden sm:flex sm:gap-2">
          <div className="w-1/2">
            <OrderBook
              stockCode={stockInfo.stockCode}
              currentPrice={currentPrice}
              initialData={orderBookInfo.initialData}
              orderBookData={orderBookInfo.realTimeData}
            />
          </div>
          <div className="w-1/2">
            <Transaction tradeExecutionData={tradeExecutionData} stockCode={stockInfo.stockCode} />
          </div>
        </div>
      </div>

      {/* 데스크톱: 우측 1/3 영역 */}
      <div className="mb-4 hidden w-full flex-col sm:flex sm:w-1/3 sm:gap-2">
        <Trade
          stockCode={stockInfo.stockCode}
          currentPrice={currentPrice}
          maxSellQty={maxSellQty?.availableQuantity ?? 0}
          isNxt={isNxt ?? false}
        />
        <Status stockCode={stockInfo.stockCode} />
      </div>

      {/* 모바일: 하단 영역 */}
      <div className="mb-4 flex gap-2 sm:hidden">
        <div className="flex w-3/5 flex-col gap-2">
          <Trade
            stockCode={stockInfo.stockCode}
            currentPrice={currentPrice}
            maxSellQty={maxSellQty?.availableQuantity ?? 0}
            isNxt={isNxt ?? false}
          />
          <Status stockCode={stockInfo.stockCode} />
        </div>
        <div className="flex w-2/5 flex-col gap-2">
          <OrderBook
            stockCode={stockInfo.stockCode}
            currentPrice={currentPrice}
            initialData={orderBookInfo.initialData}
            orderBookData={orderBookInfo.realTimeData}
          />
          <Transaction tradeExecutionData={tradeExecutionData} stockCode={stockInfo.stockCode} />
        </div>
      </div>
    </div>
  );
};

export default StockPage;
