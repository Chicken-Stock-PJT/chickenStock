import { useQuery } from "@tanstack/react-query";
import { getStockInfo, getStockPrice } from "./api";

const useStockPrice = (stockCode: string) => {
  return useQuery({
    queryKey: ["stockPrice", stockCode],
    queryFn: () => getStockPrice(stockCode),
    enabled: !!stockCode,
  });
};

const useStockInfo = (stockCode: string) => {
  return useQuery({
    queryKey: ["stockInfo", stockCode],
    queryFn: () => getStockInfo(stockCode),
    enabled: !!stockCode,
  });
};

export { useStockPrice, useStockInfo };
