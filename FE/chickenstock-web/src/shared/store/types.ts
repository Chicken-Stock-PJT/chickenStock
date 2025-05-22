export interface AuthState {
  accessToken: string | null;
  setAccessToken: (token: string) => void;
  clearAccessToken: () => void;
  login: (email: string, password: string) => Promise<{ accessToken: string }>;
  socialLogin: (code: string) => Promise<{ accessToken: string }>;
  logout: () => Promise<void>;
  isLoggedIn: boolean;
}

export interface LoginResponse {
  accessToken: string;
  accesTokenExpiresIn: number;
}
