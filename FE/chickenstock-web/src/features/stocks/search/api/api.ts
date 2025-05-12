import apiClient from "@/shared/api/axios";
import { StockInfo, StockInfoResponse } from "../model/types";

export const searchStocks = async (): Promise<StockInfo[]> => {
  try {
    const response = await apiClient.get<StockInfoResponse>(`/stocks/all`);

    return response.data;
  } catch (error) {
    console.error("주식 검색 실패:", error);
    return [];
  }
};
