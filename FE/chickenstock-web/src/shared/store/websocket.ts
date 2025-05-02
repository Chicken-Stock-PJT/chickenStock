import { create } from "zustand";
import { OrderBookData, RealTimeOrderBook } from "@/features/stocks/orderBook/model/types";
import { StockPriceData, WebSocketResponse } from "@/features/stocks/chart/model/types";
import apiClient from "@/shared/api/axios";

interface WebSocketState {
  ws: WebSocket | null;
  orderBookData: OrderBookData | null;
  stockPriceData: StockPriceData | null;
  connect: (stockCode: string) => void;
  disconnect: () => void;
}

export const useWebSocketStore = create<WebSocketState>()((set, get) => ({
  ws: null,
  orderBookData: null,
  stockPriceData: null,
  subscribedList: [],
  getSubscribedList: async () => {
    const response = await apiClient.get(`${import.meta.env.VITE_BASE_URL}/stocks/subscribed`);
    set({ subscribedList: response.data });
    console.log(response);
  },

  connect: (stockCode: string) => {
    const ws = new WebSocket(`${import.meta.env.VITE_BASE_WS_URL}/stock`);

    ws.onopen = () => {
      console.log("WebSocket 연결 성공");
      ws.send(
        JSON.stringify({
          action: "subscribe",
          stockCode,
        }),
      );
      get().getSubscribedList();
    };

    ws.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data as string) as WebSocketResponse;

        switch (data.type) {
          case "stockBidAsk": {
            const orderBookData = data as RealTimeOrderBook;
            set({ orderBookData });
            break;
          }
          case "stockPrice": {
            const stockPriceData = data as StockPriceData;
            set({ stockPriceData });
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

    set({ ws });
  },

  disconnect: () => {
    const { ws } = get();
    if (ws) {
      ws.close();
      set({ ws: null, orderBookData: null, stockPriceData: null });
      get().getSubscribedList();
    }
  },
}));
