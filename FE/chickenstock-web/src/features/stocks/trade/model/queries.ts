import apiClient from "@/shared/api/axios";
import { useQuery } from "@tanstack/react-query";
import { AvailableQuantityResponse } from "./types";

const useGetAvailableQuantity = (stockCode: string) => {
  return useQuery({
    queryKey: ["availableQuantity", stockCode],
    queryFn: async () => {
      const response = await apiClient.get<AvailableQuantityResponse>(
        `/stocks/${stockCode}/available-quantity`,
      );
      return response.data;
    },
    enabled: !!stockCode,
  });
};

export { useGetAvailableQuantity };
