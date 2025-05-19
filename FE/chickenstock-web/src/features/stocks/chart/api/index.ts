import apiClient from "@/shared/api/axios";
import { ChartRequest, ChartResponse } from "../model/types";

export const getStockChartData = async ({
  stockCode,
  chartType,
  hasNext,
  nextKey,
  timeInterval,
}: ChartRequest) => {
  const queryParams = new URLSearchParams();
  if (hasNext) {
    queryParams.set("contYn", "Y");
    queryParams.set("nextKey", nextKey);
  }
  if (chartType === "MINUTE") {
    queryParams.set("timeInterval", timeInterval);
  }
  const response = await apiClient.get<ChartResponse>(
    `/stock/chart/${chartType}/${stockCode}?${queryParams.toString()}`,
  );
  return response.data;
};
