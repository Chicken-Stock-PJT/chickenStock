import apiClient from "@/shared/api/axios";
import { SimpleProfile } from "@/shared/store/types";
import {
  UpdateNicknameSuccess,
  UpdatePasswordError,
  UpdatePasswordRequest,
  UpdatePasswordResponse,
} from "../model/types";
import { UpdateNicknameError } from "../model/types";
import { ErrorResponse } from "react-router-dom";
import { AxiosError } from "axios";

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
