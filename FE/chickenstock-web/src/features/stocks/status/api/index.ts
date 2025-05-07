import apiClient from "@/shared/api/axios";
import { NoHistoryResponse, StatusResponse } from "../model/types";
const getStatus = async (stockCode: string) => {
  const response = await apiClient.get<StatusResponse | NoHistoryResponse>(
    `/members/trade-history/${stockCode}`,
  );
  return response.data;
};

export default getStatus;
