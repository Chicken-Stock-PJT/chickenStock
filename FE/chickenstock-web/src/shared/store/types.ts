export interface AuthState {
    accessToken: string | null;
    setAccessToken: (token: string) => void;
    clearAccessToken: () => void;
    login: (email: string, password: string) => Promise<void>;
    logout: () => Promise<void>;
    simpleProfile: SimpleProfile | null;
    getSimpleProfile: () => Promise<void>;
    setSimpleProfile: (profile: SimpleProfile) => void;
  }

  export interface SimpleProfile {
    "nickname": string
    "memberMoney": string
    "returnRate": "0.0",
    "isOauth": "false"
  }
