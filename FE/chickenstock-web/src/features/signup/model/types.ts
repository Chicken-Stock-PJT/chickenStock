export interface SignupData {
  email: string;
  name: string;
  nickname: string;
  password: string;
}

export interface SignupResponse {
  id: number;
  email: string;
  nickname: string;
  name: string;
}

export interface SignupError {
  status: number;
  code: string;
  error: string;
  message: string;
  path: string;
  timestamp: string;
}

export interface SuccessResponse {
  success: boolean;
  message: string;
}
