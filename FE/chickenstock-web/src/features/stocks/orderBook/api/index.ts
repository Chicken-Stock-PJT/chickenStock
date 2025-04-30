import { InitialOrderBook } from "@/features/stocks/orderBook/model/types";
import apiClient from "@/shared/api/axios";

export const getOrderBook = async (stockCode: string) => {
  const response = await apiClient.get<InitialOrderBook>(`/stocks/askbid/${stockCode}`);
  return response.data;
};
