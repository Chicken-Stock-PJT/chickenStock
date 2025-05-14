import apiClient from "@/shared/api/axios";
import { RankingResponse } from "../model/types";

export const fetchRanking = async (): Promise<RankingResponse> => {
  const response = await apiClient.get<RankingResponse>("/ranking/total-asset");
  return response.data;
};
