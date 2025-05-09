import { getPendingOrders, getStatus } from "@/features/stocks/status/api";
import { useQuery } from "@tanstack/react-query";
import { useAuthStore } from "@/shared/store/auth";

// 체결내역조회회
export const useGetStatus = (stockCode: string) => {
  const { isLoggedIn } = useAuthStore();
  return useQuery({
    queryKey: ["status", stockCode],
    queryFn: () => getStatus(stockCode),
    enabled: !!isLoggedIn,
  });
};

// 미체결내역조회
export const useGetPendingOrders = (stockCode: string) => {
  const { isLoggedIn } = useAuthStore();
  return useQuery({
    queryKey: ["pendingOrders", stockCode],
    queryFn: () => getPendingOrders(stockCode),
    enabled: !!isLoggedIn,
  });
};
