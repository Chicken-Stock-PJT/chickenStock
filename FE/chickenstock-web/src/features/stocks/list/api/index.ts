import apiClient from "@/shared/api/axios";
import { RankingType, StockChartResponse, StockListResponse } from "../model/types";

export const getStockRanking = async (
  rankingType: RankingType,
  marketType: string,
  sortType?: string,
): Promise<StockListResponse> => {
  try {
    let url = `stock/ranking/${rankingType}?marketType=${marketType}&includeManagement=1`;
    if (rankingType === "fluctuationRate" && sortType) {
      url += `&sortType=${sortType}`;
    }
    const response = await apiClient.get<StockListResponse>(url);
    return response.data;
  } catch (error) {
    console.error(error);
    throw error;
  }
};

export const getStockChart = async (
  chartType: string,
  stockCode: string,
): Promise<StockChartResponse> => {
  try {
    const response = await apiClient.get<StockChartResponse>(
      `stock/chart/${chartType}/${stockCode}`,
    );
    return response.data;
  } catch (error) {
    console.error(error);
    throw error;
  }
};
