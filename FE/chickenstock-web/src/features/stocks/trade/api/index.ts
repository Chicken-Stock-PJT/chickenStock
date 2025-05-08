import {
  ErrorResponse,
  LimitTradeRequest,
  MarketTradeRequest,
  PendingResponse,
  TradeResponse,
} from "@/features/stocks/trade/model/types";
import apiClient from "@/shared/api/axios";

// 지정가 매수
export const buyLimitOrder = async (params: LimitTradeRequest) => {
  const response = await apiClient.post<TradeResponse | PendingResponse | ErrorResponse>(
    `/stock/trading/buy`,
    {
      ...params,
      marketOrder: false,
    },
  );
  return response.data;
};

// 시장가 매수
export const buyMarketOrder = async (params: MarketTradeRequest) => {
  const response = await apiClient.post<TradeResponse | PendingResponse | ErrorResponse>(
    `/stock/trading/buy`,
    {
      ...params,
      marketOrder: true,
    },
  );
  return response.data;
};

// 지정가 매도
export const sellLimitOrder = async (params: LimitTradeRequest) => {
  const response = await apiClient.post<TradeResponse | PendingResponse | ErrorResponse>(
    `/stock/trading/sell`,
    {
      ...params,
      marketOrder: false,
    },
  );
  return response.data;
};

// 시장가 매도
export const sellMarketOrder = async (params: MarketTradeRequest) => {
  const response = await apiClient.post<TradeResponse | PendingResponse | ErrorResponse>(
    `/stock/trading/sell`,
    {
      ...params,
      marketOrder: true,
    },
  );
  return response.data;
};
