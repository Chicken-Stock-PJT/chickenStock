import apiClient from "@/shared/api/axios";
import { TotalAssetRankingResponse, ReturnRateRankingResponse } from "../model/types";

export const fetchTotalAssetRanking = async (): Promise<TotalAssetRankingResponse> => {
  const response = await apiClient.get<TotalAssetRankingResponse>("/ranking/total-asset");
  return response.data;
};

export const fetchReturnRateRanking = async (): Promise<ReturnRateRankingResponse> => {
  const response = await apiClient.get<ReturnRateRankingResponse>("/ranking/return-rate");
  return response.data;
};
