import apiClient from "@/shared/api/axios";
import { StockInfo, StockPrice } from "./types";

export const getStockInfo = async (stockCode: string): Promise<StockInfo> => {
  const response = await apiClient.get<StockInfo>(`/stocks/code/${stockCode}`);
  return response.data;
};

export const getStockPrice = async (stockCode: string): Promise<StockPrice> => {
  const response = await apiClient.get<StockPrice>(`/stocks/info/${stockCode}`);
  return response.data;
};
