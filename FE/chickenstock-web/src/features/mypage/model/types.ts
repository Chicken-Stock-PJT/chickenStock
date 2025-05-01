export interface UpdatePasswordRequest {
  currentPassword: string;
  newPassword: string;
  checkPassword: string;
}

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

export interface UpdatePasswordResponse {
  message: string;
}

export interface UpdatePasswordError extends UpdatePasswordResponse {
  status: number;
  code: string;
}
