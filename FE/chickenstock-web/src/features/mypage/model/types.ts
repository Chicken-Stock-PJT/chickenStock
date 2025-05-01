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

export interface PortfolioResponse {
  memberMoney: number; // 현금 보유액
  totalAsset: number; // 총 자산 (현금 + 주식 평가금액)
  totalInvestment: number; // 총 투자금액 (주식 매입에 사용한 금액)
  totalValuation: number; // 보유 주식 총 평가금액
  totalProfitLoss: number; // 총 손익 (평가금액 - 투자금액)
  totalReturnRate: number; // 총 수익률 (%)
  positions: Position[];
  updatedAt: string; // 데이터 갱신 시간
}

export interface Position {
  stockCode: string; // 종목코드
  stockName: string; // 종목명
  quantity: number; // 보유 수량
  averagePrice: number; // 평균 매입가
  currentPrice: number; // 현재가
  valuationAmount: number; // 평가금액 (현재가 × 수량)
  profitLoss: number; // 손익 (평가금액 - 매입금액)
  returnRate: number; // 수익률 (%)
}
