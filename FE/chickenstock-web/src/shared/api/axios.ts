import axios, { AxiosError, AxiosResponse } from "axios";
import { useAuthStore } from "../store/auth";
import {
  ExtendedAxiosRequestConfig,
  QueueItem,
  RefreshAccessTokenResponse,
} from "@/shared/api/types";

const baseURL: string = import.meta.env.VITE_BASE_URL;

// axios 인스턴스
const apiClient = axios.create({
  baseURL,
  headers: {
    "Content-Type": "application/json",
  },
  withCredentials: true,
  timeout: 10000, // 10초
});

// 요청 인터셉터
apiClient.interceptors.request.use(
  (config) => {
    // 공개 API 엔드포인트는 토큰을 추가하지 않음
    const publicPaths = ["/auth/login", "/auth/register", "/auth/token/refresh-web"];
    const isPublicRequest = publicPaths.some((path) => config.url?.includes(path));

    if (!isPublicRequest) {
      // 토큰이 필요한 경우에만 추가
      const token = useAuthStore.getState().accessToken;
      if (token) {
        config.headers.Authorization = `Bearer ${token}`;
      }
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
  async (error: AxiosError): Promise<AxiosResponse | AxiosError> => {
    const originalRequest = error.config as ExtendedAxiosRequestConfig;

    // 로그인 페이지 관련 요청이거나 이미 리프레시 토큰 요청인 경우는 무시
    const isAuthRequest = originalRequest.url?.includes("/auth/token/refresh-web");

    // 401 에러이고 재시도되지 않은 요청이며 인증 요청이 아닌 경우에만 토큰 갱신 시도
    if (error.response?.status === 401 && !originalRequest._retry && !isAuthRequest) {
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
          .catch((err: unknown) => {
            const errorMessage = err instanceof Error ? err.message : "Unknown error";
            return Promise.reject(new Error(errorMessage));
          });
      }

      originalRequest._retry = true;
      isRefreshing = true;

      try {
        // 리프레시 토큰으로 새 액세스 토큰 요청
        // HTTP-only 쿠키의 리프레시 토큰은 자동으로 요청에 포함됨
        const response = await axios.post<RefreshAccessTokenResponse>(
          `${baseURL}/auth/token/refresh-web`,
          {},
          {
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

        // 현재 경로가 이미 로그인 페이지가 아닌 경우에만 로그아웃 및 리다이렉트 처리
        const currentPath = window.location.pathname;
        if (currentPath !== "/login") {
          // 로그아웃 처리 - 토큰 갱신에 실패했으므로 사용자를 로그아웃시킴
          await useAuthStore.getState().logout();
          console.log("로그아웃 처리");

          // 로그인 페이지로 리다이렉트
          window.location.href = "/login";
        }

        return Promise.reject(refreshError as AxiosError);
      } finally {
        isRefreshing = false;
      }
    }

    // 다른 오류인 경우 그대로 반환
    return Promise.reject(error);
  },
);

export default apiClient;
