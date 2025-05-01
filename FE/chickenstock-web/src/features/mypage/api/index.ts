import apiClient from "@/shared/api/axios";
import { SimpleProfile } from "@/shared/store/types";
import { UpdateNicknameSuccess } from "../model/types";
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
