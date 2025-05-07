import {
  buyLimitOrder,
  buyMarketOrder,
  sellLimitOrder,
  sellMarketOrder,
} from "@/features/stocks/trade/api";
import { LimitTradeRequest, MarketTradeRequest } from "@/features/stocks/trade/model/types";
import { useMutation, useQueryClient } from "@tanstack/react-query";

export const useBuyLimitOrder = (params: LimitTradeRequest) => {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => buyLimitOrder(params),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ["status", params.stockCode] });
    },
    onError: (error) => {
      console.error("오류 발생:", error);
    },
  });
};

export const useBuyMarketOrder = (params: MarketTradeRequest) => {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => buyMarketOrder(params),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ["status", params.stockCode] });
    },
  });
};

export const useSellLimitOrder = (params: LimitTradeRequest) => {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => sellLimitOrder(params),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ["status", params.stockCode] });
    },
  });
};

export const useSellMarketOrder = (params: MarketTradeRequest) => {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => sellMarketOrder(params),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ["status", params.stockCode] });
    },
  });
};
