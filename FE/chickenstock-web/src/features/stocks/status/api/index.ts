import apiClient from "@/shared/api/axios";
import {
  CancelOrderResponse,
  NoHistoryResponse,
  PendingOrdersResponse,
  StatusResponse,
} from "../model/types";

export const getStatus = async (stockCode: string) => {
  const response = await apiClient.get<StatusResponse | NoHistoryResponse>(
    `/members/trade-history/${stockCode}`,
  );
  return response.data;
};

export const getPendingOrders = async (stockCode: string) => {
  const response = await apiClient.get<PendingOrdersResponse>(`stock/trading/pending-orders`);
  return response.data.filter((order) => order.stockCode === stockCode);
};

export const cancelOrder = async (orderId: string) => {
  const response = await apiClient.post<CancelOrderResponse>(
    `/stock/trading/cancel-order/${orderId}`,
  );
  return response.data;
};
