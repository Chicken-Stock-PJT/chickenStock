import OrderRow from "./OrderRow";
import OrderIndex from "./OrderIndex";
import { getOrderBook } from "@/features/stocks/orderBook/api";
import { useEffect, useState, useMemo, useCallback } from "react";
import {
  mapInitialOrderBookToOrderBookData,
  convertOrderBookDataToRows,
  OrderBookRow,
  OrderBookProps,
} from "@/features/stocks/orderBook/model/types";
import { useWebSocketStore } from "@/shared/store/websocket";
import React from "react";

const MemoizedOrderRow = React.memo(OrderRow);
const MemoizedOrderIndex = React.memo(OrderIndex);

const OrderBook = ({ stockCode, currentPrice }: OrderBookProps) => {
  const [orderBookRows, setOrderBookRows] = useState<OrderBookRow[]>([]);
  const orderBookData = useWebSocketStore((state) => state.orderBookData);

  // 초기 호가 데이터 로드
  useEffect(() => {
    const fetchOrderBook = async () => {
      try {
        const data = await getOrderBook(stockCode);
        if (data) {
          const mappedData = mapInitialOrderBookToOrderBookData(data);
          const rows = convertOrderBookDataToRows(mappedData);
          setOrderBookRows(rows);
        }
      } catch (error) {
        console.error("Failed to fetch order book:", error);
      }
    };

    void fetchOrderBook();
  }, [stockCode]);

  // WebSocket 데이터 업데이트 처리
  useEffect(() => {
    if (!orderBookData) return;

    const rows = convertOrderBookDataToRows(orderBookData);
    setOrderBookRows((prevRows) => {
      // 이전 데이터와 동일한 경우 업데이트하지 않음
      if (JSON.stringify(prevRows) === JSON.stringify(rows)) {
        return prevRows;
      }
      return rows;
    });
  }, [orderBookData]);

  // 최대 거래량 계산 메모이제이션
  const maxVolume = useMemo(() => {
    return Math.max(...orderBookRows.map((row) => Math.max(row.askVolume, row.bidVolume)));
  }, [orderBookRows]);

  // 총 거래량 계산 메모이제이션
  const totalVolumes = useMemo(() => {
    return orderBookRows.reduce(
      (acc, row) => ({
        bid: acc.bid + row.bidVolume,
        ask: acc.ask + row.askVolume,
      }),
      { bid: 0, ask: 0 },
    );
  }, [orderBookRows]);

  // OrderRow 렌더링 메모이제이션
  const renderOrderRows = useCallback(() => {
    return orderBookRows.map((row, index) => (
      <MemoizedOrderRow
        key={`row-${index}`}
        price={row.price}
        askVolume={row.askVolume}
        bidVolume={row.bidVolume}
        max={maxVolume}
        isCurrentPrice={row.price === currentPrice}
      />
    ));
  }, [orderBookRows, maxVolume, currentPrice]);

  if (orderBookRows.length === 0) return null;

  return (
    <div className="flex h-full flex-col rounded-lg border border-gray-200 bg-white shadow-sm">
      <header className="flex items-center justify-between border-b border-gray-200 px-3 py-2 sm:px-6 sm:py-3">
        <span className="text-sm font-semibold text-gray-800 sm:text-base">호가잔량</span>
      </header>

      <div className="relative flex-1 overflow-auto">
        <div className="hidden sm:block">
          <MemoizedOrderIndex />
        </div>

        <div className="grid">{renderOrderRows()}</div>
      </div>

      <footer className="flex justify-between border-t border-gray-200 px-1 py-1.5 text-[10px] text-gray-500 sm:px-6 sm:py-2 sm:text-xs">
        <div className="flex flex-col items-start gap-1 md:flex-row md:items-center md:gap-2">
          <span>총 매수잔량: </span>
          <span>{totalVolumes.bid.toLocaleString()}</span>
        </div>
        <div className="flex flex-col items-end gap-1 md:flex-row md:items-center md:gap-2">
          <span>총 매도잔량: </span>
          <span>{totalVolumes.ask.toLocaleString()}</span>
        </div>
      </footer>
    </div>
  );
};

export default React.memo(OrderBook);
