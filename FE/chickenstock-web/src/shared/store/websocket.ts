import { create } from "zustand";
import { OrderBookData, RealTimeOrderBook } from "@/features/stocks/orderBook/model/types";
import {
  StockPriceData,
  TradeExecutionData,
  WebSocketResponse,
} from "@/features/stocks/chart/model/types";
import apiClient from "@/shared/api/axios";

interface WebSocketState {
  ws: WebSocket | null;
  orderBookData: OrderBookData | null;
  stockPriceData: StockPriceData | null;
  tradeExecutionData: TradeExecutionData | null;
  connect: (stockCode: string) => void;
  disconnect: () => void;
  subscribedList: string[];
  getSubscribedList: () => Promise<void>;
  setStockPriceData: (data: StockPriceData) => void;
  setOrderBookData: (data: OrderBookData) => void;
  setTradeExecutionData: (data: TradeExecutionData) => void;
}

export const useWebSocketStore = create<WebSocketState>()((set, get) => ({
  ws: null,
  orderBookData: null,
  stockPriceData: null,
  tradeExecutionData: null,
  subscribedList: [],

  getSubscribedList: async () => {
    const response = await apiClient.get(`${import.meta.env.VITE_BASE_URL}/stocks/subscribed`);
    set({ subscribedList: response.data as string[] });
    console.log(response);
  },

  setStockPriceData: (data) =>
    set((state) => {
      if (JSON.stringify(state.stockPriceData) === JSON.stringify(data)) {
        return state;
      }
      return { stockPriceData: data };
    }),
  setOrderBookData: (data) =>
    set((state) => {
      if (JSON.stringify(state.orderBookData) === JSON.stringify(data)) {
        return state;
      }
      return { orderBookData: data };
    }),
  setTradeExecutionData: (data) =>
    set((state) => {
      if (JSON.stringify(state.tradeExecutionData) === JSON.stringify(data)) {
        return state;
      }
      return { tradeExecutionData: data };
    }),

  connect: (stockCode: string) => {
    // 기존 웹소켓이 있다면 먼저 닫기
    const currentWs = get().ws;
    if (currentWs && currentWs.readyState !== WebSocket.CLOSED) {
      currentWs.close();
    }

    const ws = new WebSocket(`${import.meta.env.VITE_BASE_WS_URL}/stock`);

    ws.onopen = () => {
      setTimeout(() => {
        ws.send(
          JSON.stringify({
            action: "subscribe",
            stockCode,
          }),
        );
        void get().getSubscribedList();
      }, 1000);
    };

    ws.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data as string) as WebSocketResponse;
        console.log(data);

        switch (data.type) {
          case "stockBidAsk": {
            get().setOrderBookData(data as RealTimeOrderBook);
            break;
          }
          case "stockPrice": {
            get().setStockPriceData(data as StockPriceData);
            break;
          }
          case "tradeExecution": {
            get().setTradeExecutionData(data as TradeExecutionData);
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

      // 현재 저장된 웹소켓이 닫히는 웹소켓과 동일한 경우에만 상태 초기화
      const currentWs = get().ws;
      if (currentWs === ws) {
        set({ ws: null, orderBookData: null, stockPriceData: null });
      }
    };

    set({ ws });
  },

  disconnect: () => {
    const { ws } = get();
    if (ws) {
      if (ws.readyState !== WebSocket.CLOSED) {
        ws.close();
      }
      set({ ws: null, orderBookData: null, stockPriceData: null });
      console.log("WebSocket 연결 종료");
    }
  },
}));
