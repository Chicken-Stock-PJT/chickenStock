export interface AuthState {
  accessToken: string | null;
  setAccessToken: (token: string) => void;
  clearAccessToken: () => void;
  login: (email: string, password: string) => Promise<{ accessToken: string }>;
  socialLogin: (code: string) => Promise<{ accessToken: string }>;
  logout: () => Promise<void>;
  simpleProfile: SimpleProfile | null;
  getSimpleProfile: () => Promise<SimpleProfile>;
  setSimpleProfile: (profile: Partial<SimpleProfile>) => void;
  isLoggedIn: boolean;
}

export interface SimpleProfile {
  nickname: string;
  memberMoney: string;
  totalAsset: string;
  returnRate: string;
  isOauth: string;
}

export interface LoginResponse {
  accessToken: string;
  accesTokenExpiresIn: number;
}
