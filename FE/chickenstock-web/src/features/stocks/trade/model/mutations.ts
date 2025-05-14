import {
  buyLimitOrder,
  buyMarketOrder,
  sellLimitOrder,
  sellMarketOrder,
} from "@/features/stocks/trade/api";
import { LimitTradeRequest, MarketTradeRequest } from "@/features/stocks/trade/model/types";
import { useMutation, useQueryClient } from "@tanstack/react-query";

export const useBuyLimitOrder = () => {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (params: LimitTradeRequest) => buyLimitOrder(params),
    onSuccess: (_, variables) => {
      void queryClient.invalidateQueries({ queryKey: ["pendingOrders", variables.stockCode] });
      void queryClient.invalidateQueries({ queryKey: ["status", variables.stockCode] });
      void queryClient.invalidateQueries({ queryKey: ["simpleProfile"] });
    },
    onError: (error) => {
      console.error("오류 발생:", error);
    },
  });
};

export const useBuyMarketOrder = () => {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (params: MarketTradeRequest) => buyMarketOrder(params),
    onSuccess: (_, variables) => {
      void queryClient.invalidateQueries({ queryKey: ["status", variables.stockCode] });
      void queryClient.invalidateQueries({ queryKey: ["simpleProfile"] });
    },
  });
};

export const useSellLimitOrder = () => {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (params: LimitTradeRequest) => sellLimitOrder(params),
    onSuccess: (_, variables) => {
      void queryClient.invalidateQueries({ queryKey: ["pendingOrders", variables.stockCode] });
      void queryClient.invalidateQueries({ queryKey: ["status", variables.stockCode] });
      void queryClient.invalidateQueries({ queryKey: ["simpleProfile"] });
    },
  });
};

export const useSellMarketOrder = () => {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: (params: MarketTradeRequest) => sellMarketOrder(params),
    onSuccess: (_, variables) => {
      void queryClient.invalidateQueries({ queryKey: ["status", variables.stockCode] });
      void queryClient.invalidateQueries({ queryKey: ["simpleProfile"] });
    },
  });
};
