export interface MemberDashboardResponse {
  memberMoney: number; // 사용자 보유 현금 (투자 가능한 현금)
  stockValuation: number; // 보유 주식의 현재 평가 금액 (현재가 × 보유수량의 합계)
  pendingOrderAmount: number; // 미체결 매수 주문 대기 금액 (이미 현금에서 차감됨)
  totalAsset: number; // 총 자산 = 현금 + 주식평가금액 (순자산 개념) ** 여기에 바로 위 체결 금액 더해서 쓰세요.
  totalInvestment: number; // 총 투자 금액 (평균매입가 × 보유수량의 합계)
  totalProfitLoss: number; // 총 손익 = 평가금액 - 투자금액 (현재 -258원 손실)
  totalReturnRate: number; // 총 수익률 = (총손익 ÷ 총투자금액) × 100
  todayProfitLoss: number; // 오늘 실현 손익 (오늘 매도한 주식의 손익)
  todayReturnRate: number; // 오늘 수익률
  holdingStockCount: number; // 보유 종목 수
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
