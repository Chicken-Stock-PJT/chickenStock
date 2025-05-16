import { MemberDashboardResponse } from "@/features/dashboard/model/types";
import { TransactionResponse } from "@/features/mypage/model/types";
import apiClient from "@/shared/api/axios";

export const getAiBotDashboard = async (botId: number): Promise<MemberDashboardResponse> => {
  const response = await apiClient.get<MemberDashboardResponse>(`/members/${botId}/dashboard`);
  return response.data;
};

export const getAiTradeHistory = async ({
  size,
  cursor,
  botId,
}: {
  size: number;
  cursor: string;
  botId: number;
}): Promise<TransactionResponse> => {
  const response = await apiClient.get<TransactionResponse>(
    `/trade-histories/${botId}?size=${size}` + (cursor.length > 0 ? `&cursor=${cursor}` : ""),
  );

  return response.data;
};
