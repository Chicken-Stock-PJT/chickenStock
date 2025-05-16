import { MemberDashboardResponse } from "@/features/dashboard/model/types";
import apiClient from "@/shared/api/axios";

export const getAiBotDashboard = async (botId: number): Promise<MemberDashboardResponse> => {
  const response = await apiClient.get<MemberDashboardResponse>(`/members/${botId}/dashboard`);
  return response.data;
};
