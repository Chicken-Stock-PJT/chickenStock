export interface UpdateNicknameSuccess {
  message: string;
}

export interface UpdateNicknameError {
  status: number;
  code: string;
  error: string;
  message: string;
  path: string;
  timestamp: string;
}
