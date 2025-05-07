import getStatus from "@/features/stocks/status/api";
import { useQuery } from "@tanstack/react-query";

export const useGetStatus = (stockCode: string) => {
  return useQuery({
    queryKey: ["status", stockCode],
    queryFn: () => getStatus(stockCode),
  });
};
