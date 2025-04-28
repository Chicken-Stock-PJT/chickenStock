import axios from "axios";
import { useAuthStore } from "../store/auth";

const baseURL = import.meta.env.VITE_API_URL;

/* 기본 설정을 위한 axios 인스턴스 생성 */
const apiClient = axios.create({
  baseURL,
  headers: {
    "Content-Type": "application/json",
  },
  withCredentials: true,
  timeout: 10000, // 10초
});

/* 요청 인터셉터 */
apiClient.interceptors.request.use(
  (config) => {
    // 토큰이 필요한 경우 여기에 추가
    const token = useAuthStore.getState().accessToken;
    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error: Error) => {
    return Promise.reject(error);
  },
);

apiClient.interceptors.response.use(
  (response) => response,
  async (error) => {
    const originalRequest = error.config;

    // 401 에러이고 재시도하지 않은 경우
    if (error.response?.status === 401 && !originalRequest._retry) {
      originalRequest._retry = true;

      try {
        // 리프레시 토큰은 쿠키에 있으므로 별도로 전송할 필요 없음
        const response = await axios.post(
          "auth/token/refresh-web",
          {}, // 빈 객체 전송
          { withCredentials: true },
        );

        // 새 액세스 토큰 저장
        const { accessToken } = response.data.accessToken;
        useAuthStore.getState().setAccessToken(accessToken);

        // 원래 요청 재시도
        originalRequest.headers.Authorization = `Bearer ${accessToken}`;
        return axios(originalRequest);
      } catch (refreshError) {
        // 리프레시 실패 시 로그아웃 처리
        useAuthStore.getState().clearAccessToken();
        // 로그인 페이지로 리다이렉트
        window.location.href = "/login";
        return Promise.reject(refreshError);
      }
    }

    return Promise.reject(error);
  },
);

export default apiClient;
