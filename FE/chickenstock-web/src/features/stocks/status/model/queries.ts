import { getPendingOrders, getStatus } from "@/features/stocks/status/api";
import { useQuery } from "@tanstack/react-query";

// 체결내역조회회
export const useGetStatus = (stockCode: string) => {
  return useQuery({
    queryKey: ["status", stockCode],
    queryFn: () => getStatus(stockCode),
  });
};

// 미체결내역조회
export const useGetPendingOrders = (stockCode: string) => {
  return useQuery({
    queryKey: ["pendingOrders", stockCode],
    queryFn: () => getPendingOrders(stockCode),
    enabled: !!stockCode,
  });
};
