import {
  CheckDuplicateResponse,
  SignupData,
  SignupResponse,
  SuccessResponse,
} from "@/features/signup/model/types";
import axios, { AxiosResponse } from "axios";

const baseURL = import.meta.env.VITE_BASE_URL;

export const authApi = {
  /**
   * 이메일 중복 확인
   * @param email 확인할 이메일
   * @returns {Promise<SuccessResponse>} 중복 여부
   */
  checkEmail: async (email: string): Promise<SuccessResponse> => {
    const response = await axios.post<SuccessResponse>(
      `${baseURL}/auth/check-email`,
      { email },
      {
        // withCredentials: true,
        headers: {
          "Content-Type": "application/json",
        },
      },
    );

    return response.data;
  },

  /**
   * 닉네임 중복 확인
   * @param nickname 확인할 닉네임
   * @returns {Promise<{duplicate: boolean, message: string}>} 중복 여부
   */
  checkNickname: async (nickname: string): Promise<CheckDuplicateResponse> => {
    const response = await axios.post<CheckDuplicateResponse>(
      `${baseURL}/auth/check-nickname`,
      { nickname },
      {
        withCredentials: true,
        headers: {
          "Content-Type": "application/json",
        },
      },
    );

    return response.data;
  },

  /**
   * 인증번호 전송
   * @param email 전송할 이메일
   * @returns {Promise<void>}
   */
  sendCode: async (email: string): Promise<AxiosResponse | undefined> => {
    try {
      const response = await axios.post<AxiosResponse>(
        `${baseURL}/auth/send-code`,
        { email },
        {
          withCredentials: true,
          headers: {
            "Content-Type": "application/json",
          },
        },
      );

      return response;
    } catch (err) {
      console.error("인증코드 발송 실패:", err);
    }
  },

  /**
   * 인증번호 확인
   * @param {email, code} 전송할 이메일
   * @returns {Promise<SuccessResponse>}
   */
  verifyCode: async (email: string, code: string): Promise<SuccessResponse | undefined> => {
    console.log(email, code);
    try {
      const response = await axios.post<SuccessResponse>(
        `${baseURL}/auth/verify-code`,
        { email, code },
        {
          headers: {
            "Content-Type": "application/json",
          },
        },
      );
      return response.data;
    } catch (err) {
      console.error("인증 실패:", err);
    }
  },

  /**
   * 회원가입
   * @param {formData:SignupData} 이메일, 비번, 닉네임, 이름
   * @returns {Promise<SignupResponse>}
   */
  signup: async (formData: SignupData): Promise<SignupResponse> => {
    const response = await axios.post<SignupResponse>(
      `${baseURL}/auth/signup`,
      { ...formData },
      {
        withCredentials: true,
        headers: {
          "Content-Type": "application/json",
        },
      },
    );

    return response.data;
  },
};
