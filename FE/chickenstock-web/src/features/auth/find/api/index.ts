import apiClient from "@/shared/api/axios";
import { AxiosError } from "axios";

interface SuccessResponse {
  message: string;
}

interface ErrorResponse {
  status: number;
  code: string;
  error: string;
  message: string;
  path: string;
  timestamp: string;
}

export const findPassword = async (email: string): Promise<SuccessResponse> => {
  try {
    const response = await apiClient.post<SuccessResponse>("/auth/reset-password-by-code", {
      email,
    });
    if (response.status === 200) {
      return response.data;
    } else {
      throw new Error("Failed to find password");
    }
  } catch (error) {
    if (error instanceof AxiosError) {
      const errorResponse = error.response?.data as ErrorResponse;
      if (errorResponse) {
        console.log(errorResponse);
        throw new Error(errorResponse.message);
      }
      throw new Error("서버와의 통신 중 오류가 발생했습니다");
    }
    throw new Error("비밀번호 찾기 중 오류가 발생했습니다");
  }
};
