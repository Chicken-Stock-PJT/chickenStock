import apiClient from "@/shared/api/axios";
import { SimpleProfile } from "@/shared/store/types";
import {
  DailyProfitRateResponse,
  PortfolioResponse,
  TransactionResponse,
  UpdateNicknameSuccess,
  UpdatePasswordError,
  UpdatePasswordRequest,
  UpdatePasswordResponse,
} from "../model/types";
import { UpdateNicknameError } from "../model/types";
import { ErrorResponse } from "react-router-dom";
import axios, { AxiosError } from "axios";

export const getUserInfo = async () => {
  const response = await apiClient.get<SimpleProfile>("/members/simple-profile");
  return response.data;
};

export const updateNickname = async (
  nickname: string,
): Promise<UpdateNicknameSuccess | UpdateNicknameError | AxiosError<ErrorResponse>> => {
  const response = await apiClient.patch<UpdateNicknameSuccess | UpdateNicknameError>(
    "/members/nickname",
    {
      nickname,
    },
  );
  return response.data;
};

export const updatePassword = async (
  passwordParams: UpdatePasswordRequest,
): Promise<UpdatePasswordResponse | AxiosError<UpdatePasswordError>> => {
  try {
    const response = await apiClient.post<UpdatePasswordResponse | UpdatePasswordError>(
      "/members/change-password",
      {
        ...passwordParams,
      },
    );
    alert("비밀번호가 변경되었습니다.");
    return response.data;
  } catch (error) {
    console.log(error);
    if (error instanceof AxiosError) {
      alert(error.response?.data.message);
      return error.response?.data as UpdatePasswordError;
    }
    throw error;
  }
};

export const getPortfolio = async (): Promise<PortfolioResponse | AxiosError<ErrorResponse>> => {
  const response = await apiClient.get<PortfolioResponse>("/members/portfolio");
  return response.data;
};

export const getTransactions = async ({
  size,
  cursor,
}: {
  size: number;
  cursor: string;
}): Promise<TransactionResponse | AxiosError<ErrorResponse>> => {
  console.log(cursor);
  try {
    const response = await apiClient.get<TransactionResponse>(
      `/trade-histories?size=${size}` + (cursor.length > 0 ? `&cursor=${cursor}` : ""),
    );
    return response.data;
  } catch (error) {
    // 에러 로깅, 변환 등의 처리
    console.error("API error:", error);

    // 에러를 더 의미 있는 형태로 변환 (선택사항)
    if (axios.isAxiosError(error)) {
      if (error.response?.status === 401) {
        throw new Error("Authentication required");
      } else if (error.response?.status === 404) {
        throw new Error("Transactions not found");
      }
    }

    // 기본 에러 throw
    throw error;
  }
};

// 일간 수익률 조회
export const getDailyProfitRate = async (): Promise<DailyProfitRateResponse> => {
  const response = await apiClient.get<DailyProfitRateResponse>(
    "/members/return-rate/period?period=daily",
  );
  console.log(response.data);
  return response.data;
};
