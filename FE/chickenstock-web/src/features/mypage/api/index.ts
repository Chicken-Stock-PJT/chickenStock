import apiClient from "@/shared/api/axios";
import { SimpleProfile } from "@/shared/store/types";

export const getUserInfo = async () => {
  const response = await apiClient.get<SimpleProfile>("/members/simple-profile");
  return response.data;
};
