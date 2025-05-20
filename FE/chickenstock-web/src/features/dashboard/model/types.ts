export interface MemberDashboardResponse {
  memberMoney: number; // 예수금 잔고 (순 현금성 자산)
  cash: number; // 현금 (순 현금성 자산 + 지정가 매수 대기중인 금액)
  stockValuation: number; // 보유 주식 평가금액
  totalValuationAmount: number; // 총 평가금액 (보유 주식 평가금액)
  pendingOrderAmount: number; // 지정가 매수 대기중인 금액
  pendingSellAmount: number; // 지정가 매도 대기중인 금액 (정보 제공용)
  totalAsset: number; // 총 자산
  totalInvestment: number; // 총 투자금액
  totalProfitLoss: number; // 총 손익
  totalReturnRate: number; // 총 수익률
  todayProfitLoss: number; // 금일 수익
  todayReturnRate: number; // 금일 수익률
  todayTradeAmount: number; // 금일 매매 규모
  holdingStockCount: number;
  holdings: Holdings[];
}

export interface Holdings {
  stockCode: string;
  stockName: string;
  quantity: number; // 보유 수량
  averagePrice: number; // 평균 매입 단가
  currentPrice: number; // 현재가
  valuationAmount: number; // 평가 금액 = 현재가 × 수량
  profitLoss: number; // 손익 = 평가금액 - 투자금액
  returnRate: number; // 수익률 = 손익 ÷ 투자금액 × 100
  priceChange: string; // 전일 대비 가격 변동
  changeRate: string; // 전일 대비 변동률(%)
}
