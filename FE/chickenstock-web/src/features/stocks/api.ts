import apiClient from "@/shared/api/axios";
import { StockInfo } from "./types";

export const getStockInfo = async (stockCode: string): Promise<StockInfo> => {
  const response = await apiClient.get<StockInfo>(`/stocks/code/${stockCode}`);
  return response.data;
};
