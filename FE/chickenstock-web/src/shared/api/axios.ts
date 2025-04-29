import axios, { AxiosError, AxiosResponse } from "axios";
import { useAuthStore } from "../store/auth";
import {
  ExtendedAxiosRequestConfig,
  QueueItem,
  RefreshAccessTokenResponse,
} from "@/shared/api/types";

const baseURL: string = import.meta.env.VITE_BASE_URL;

// axios 인스턴스스
const apiClient = axios.create({
  baseURL,
  headers: {
    "Content-Type": "application/json",
  },
  withCredentials: true,
  timeout: 10000, // 10초
});

// 요청 인터셉터터
apiClient.interceptors.request.use(
  (config) => {
    // 토큰이 필요한 경우 여기에 추가
    const token = useAuthStore.getState().accessToken;
    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error: AxiosError): Promise<AxiosError> => {
    return Promise.reject(error);
  },
);

// 응답 인터셉터 추가 - 401 에러 시 토큰 갱신
let isRefreshing = false;
let failedQueue: QueueItem[] = [];

const processQueue = (error: AxiosError | null, token: string | null = null): void => {
  failedQueue.forEach((prom) => {
    if (error) {
      prom.reject(error);
    } else {
      prom.resolve(token!);
    }
  });

  failedQueue = [];
};

apiClient.interceptors.response.use(
  (response: AxiosResponse): AxiosResponse => {
    return response;
  },
  async (error: AxiosError): Promise<any> => {
    const originalRequest = error.config as ExtendedAxiosRequestConfig;

    // 401 에러이고 재시도되지 않은 요청인 경우
    if (error.response?.status === 401 && !originalRequest._retry) {
      if (isRefreshing) {
        // 이미 토큰 갱신 중이면 큐에 추가
        return new Promise<string>((resolve, reject) => {
          failedQueue.push({ resolve, reject });
        })
          .then((token) => {
            if (originalRequest.headers) {
              originalRequest.headers.Authorization = `Bearer ${token}`;
            } else {
              originalRequest.headers = {
                Authorization: `Bearer ${token}`,
              };
            }
            return apiClient(originalRequest);
          })
          .catch((err) => {
            return Promise.reject(err);
          });
      }

      originalRequest._retry = true;
      isRefreshing = true;

      try {
        // 리프레시 토큰으로 새 액세스 토큰 요청
        // HTTP-only 쿠키의 리프레시 토큰은 자동으로 요청에 포함됨
        const response = await axios.post<RefreshAccessTokenResponse>(
          "/auth/token/refresh-web",
          {},
          {
            baseURL,
            withCredentials: true, // 쿠키를 포함하기 위해 필요
          },
        );

        // 새 액세스 토큰 저장
        const newAccessToken = response.data.accessToken;
        useAuthStore.getState().setAccessToken(newAccessToken);

        // 원래의 요청 헤더에 새 토큰 설정
        if (originalRequest.headers) {
          originalRequest.headers.Authorization = `Bearer ${newAccessToken}`;
        } else {
          originalRequest.headers = {
            Authorization: `Bearer ${newAccessToken}`,
          };
        }

        // 큐에 있는 요청들 처리
        processQueue(null, newAccessToken);

        // 원래 요청 재시도
        return apiClient(originalRequest);
      } catch (refreshError) {
        // 토큰 갱신 실패 처리
        processQueue(refreshError as AxiosError);

        // 로그아웃 처리 - 토큰 갱신에 실패했으므로 사용자를 로그아웃시킴
        useAuthStore.getState().logout();

        // 로그인 페이지로 리다이렉트 등의 추가 작업
        window.location.href = "/login"; // 또는 React Router를 사용하여 리다이렉트

        return Promise.reject(refreshError);
      } finally {
        isRefreshing = false;
      }
    }

    // 다른 오류인 경우 그대로 반환
    return Promise.reject(error);
  },
);

export default apiClient;
