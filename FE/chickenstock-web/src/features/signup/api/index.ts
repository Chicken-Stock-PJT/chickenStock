import axios from "axios";
import { SignupData } from "../model/types";

export const authApi = {
  /**
   * 이메일 중복 확인
   * @param email 확인할 이메일
   * @returns {Promise<SuccessResponse>} 중복 여부
   */
  checkEmail: async (email: string) => {
    const response = await axios.post(
      `/auth/check-email`,
      { email },
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
   * 닉네임 중복 확인
   * @param nickname 확인할 닉네임
   * @returns {Promise<{duplicate: boolean, message: string}>} 중복 여부
   */
  checkNickname: async (nickname: string) => {
    const response = await axios.post(
      `/auth/check-nickname`,
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
  sendCode: async (email: string) => {
    try {
      const response = await axios.post(
        `/auth/send-code`,
        { email },
        {
          withCredentials: true,
          headers: {
            "Content-Type": "application/json",
          },
        },
      );

      return response.data;
    } catch (err) {
      console.error("인증코드 발송 실패:", err);
    }
  },

  /**
   * 인증번호 확인
   * @param {email, code} 전송할 이메일
   * @returns {Promise<SuccessResponse>}
   */
  verifyCode: async (email: string, code: string) => {
    try {
      const response = await axios.get(`/auth/verify-code`, {
        params: { email, code },
        withCredentials: true,
        headers: {
          "Content-Type": "application/json",
        },
      });

      return response.data;
    } catch (err) {
      console.error("인증 실패:", err);
    }
  },

  /**
   * 회원가입
   * @param {formData:SignupData} 이메일, 비번, 닉네임, 이름
   * @returns {Promise<{id: number, email: string, nickname: string, name: string}>} 중복 여부
   */
  signup: async (formData: SignupData) => {
    const response = await axios.post(
      `/auth/signup`,
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
