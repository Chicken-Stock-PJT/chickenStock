import apiClient from "@/shared/api/axios";
import { useQuery } from "@tanstack/react-query";
import { AvailableQuantityResponse } from "./types";
import { useAuthStore } from "@/shared/store/auth";
import { isNxtStock } from "./nxtStocks";

const useGetIsNxtStock = (stockCode: string) => {
  return useQuery<boolean>({
    queryKey: ["isNxt", stockCode],
    queryFn: () => isNxtStock(stockCode),
    enabled: !!stockCode,
  });
};

const useGetAvailableQuantity = (stockCode: string) => {
  const isLoggedIn = useAuthStore((state) => state.isLoggedIn);
  return useQuery({
    queryKey: ["availableQuantity", stockCode],
    queryFn: async () => {
      const response = await apiClient.get<AvailableQuantityResponse>(
        `/stocks/${stockCode}/available-quantity`,
      );
      return response.data;
    },
    enabled: !!stockCode && !!isLoggedIn,
  });
};

export { useGetAvailableQuantity, useGetIsNxtStock };
