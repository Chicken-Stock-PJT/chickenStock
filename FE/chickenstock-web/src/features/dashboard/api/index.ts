import { MemberDashboardResponse } from "@/features/dashboard/model/types";
import { TransactionResponse } from "@/features/mypage/model/types";
import apiClient from "@/shared/api/axios";

export const getMemberDashboard = async (memberId: number): Promise<MemberDashboardResponse> => {
  const response = await apiClient.get<MemberDashboardResponse>(`/members/${memberId}/dashboard`);
  return response.data;
};

export const getMemberTradeHistory = async ({
  size,
  cursor,
  memberId,
}: {
  size: number;
  cursor: string;
  memberId: number;
}): Promise<TransactionResponse> => {
  const response = await apiClient.get<TransactionResponse>(
    `/trade-histories/${memberId}?size=${size}` + (cursor.length > 0 ? `&cursor=${cursor}` : ""),
  );

  return response.data;
};
