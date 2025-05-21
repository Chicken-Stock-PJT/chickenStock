import { useQuery } from "@tanstack/react-query";
import { getOrderBook } from "../api";

const useOrderBook = (stockCode: string) => {
  return useQuery({
    queryKey: ["orderBook", stockCode],
    queryFn: () => getOrderBook(stockCode),
    enabled: !!stockCode,
  });
};

export { useOrderBook };
