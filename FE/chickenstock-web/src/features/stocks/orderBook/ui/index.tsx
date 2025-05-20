import OrderRow from "./OrderRow";
import OrderIndex from "./OrderIndex";
import { getOrderBook } from "@/features/stocks/orderBook/api";
import { useEffect, useState } from "react";
import {
  mapInitialOrderBookToOrderBookData,
  convertOrderBookDataToRows,
  OrderBookRow,
  OrderBookProps,
} from "@/features/stocks/orderBook/model/types";
import { useWebSocketStore } from "@/shared/store/websocket";
import React from "react";

const MemoizedOrderRow = React.memo(OrderRow);

const OrderBook = ({ stockCode, currentPrice }: OrderBookProps) => {
  const [orderBookRows, setOrderBookRows] = useState<OrderBookRow[]>([]);
  const { orderBookData } = useWebSocketStore();

  useEffect(() => {
    const fetchOrderBook = async () => {
      const data = await getOrderBook(stockCode);
      if (data) {
        const mappedData = mapInitialOrderBookToOrderBookData(data);
        const rows = convertOrderBookDataToRows(mappedData);
        setOrderBookRows(rows);
      }
    };

    void fetchOrderBook();
  }, [stockCode]);

  useEffect(() => {
    if (!orderBookData) return;

    const rows = convertOrderBookDataToRows(orderBookData);
    setOrderBookRows((prevRows) => {
      if (JSON.stringify(prevRows) === JSON.stringify(rows)) {
        return prevRows;
      }
      return rows;
    });
  }, [orderBookData]);

  if (orderBookRows.length === 0) return null;

  // 최대 거래량 계산
  const maxVolume = Math.max(...orderBookRows.map((row) => Math.max(row.askVolume, row.bidVolume)));

  return (
    <div className="flex h-full flex-col overflow-hidden rounded-lg border border-gray-200 bg-white shadow-sm">
      <header className="flex items-center justify-between border-b border-gray-200 px-6 py-3">
        <span className="font-semibold text-gray-800">호가잔량</span>
      </header>

      <div className="relative flex-1 overflow-auto">
        <OrderIndex />
        {/* <div className="divide-y divide-gray-100"> */}
        <div className="grid">
          {orderBookRows.map((row, index) => (
            <MemoizedOrderRow
              key={`row-${index}`}
              price={row.price}
              askVolume={row.askVolume}
              bidVolume={row.bidVolume}
              max={maxVolume}
              isCurrentPrice={row.price === currentPrice}
            />
          ))}
        </div>
      </div>

      <footer className="flex justify-between border-t border-gray-200 px-6 py-2 text-xs text-gray-500">
        <span>
          총 매수잔량: {orderBookRows.reduce((sum, row) => sum + row.bidVolume, 0).toLocaleString()}
        </span>
        <span>
          총 매도잔량: {orderBookRows.reduce((sum, row) => sum + row.askVolume, 0).toLocaleString()}
        </span>
      </footer>
    </div>
  );
};

export default OrderBook;
