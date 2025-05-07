import apiClient from "@/shared/api/axios";
import { CancelOrderResponse, NoHistoryResponse, StatusResponse } from "../model/types";

export const getStatus = async (stockCode: string) => {
  const response = await apiClient.get<StatusResponse | NoHistoryResponse>(
    `/members/trade-history/${stockCode}`,
  );
  return response.data;
};

export const cancelOrder = async (orderId: string) => {
  const response = await apiClient.delete<CancelOrderResponse>(
    `/stock/trading/cancel-order/${orderId}`,
  );
  return response.data;
};
