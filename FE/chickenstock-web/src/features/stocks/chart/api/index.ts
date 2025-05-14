import apiClient from "@/shared/api/axios";
import { ChartRequest, ChartResponse } from "../model/types";

export const getStockChartData = async ({
  stockCode,
  chartType,
  hasNext,
  nextKey,
}: ChartRequest) => {
  const response = await apiClient.get<ChartResponse>(
    `/stock/chart/${chartType}/${stockCode}` + `${hasNext ? "?contYn=Y&nextKey=" + nextKey : ""}`,
  );
  return response.data;
};
