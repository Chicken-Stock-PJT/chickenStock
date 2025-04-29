import { AxiosRequestConfig } from "axios";

// 응답 인터셉터 | 큐에 있는 요청들을 위한 인터페이스
export interface QueueItem {
  resolve: (value: string | PromiseLike<string>) => void;
  reject: (reason?: any) => void;
}

export interface RefreshAccessTokenResponse {
  accessToken: string;
  accessTokenExpiresIn: number;
}

// 원본 요청 설정에 _retry 필드를 추가하기 위한 확장 인터페이스
export interface ExtendedAxiosRequestConfig extends AxiosRequestConfig {
  _retry?: boolean;
}
