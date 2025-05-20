export interface OpenOrder {
  side: "BUY" | "SELL";
  remainingVolume: number;
  totalVolume: number;
  orderPrice: number;
  orderTime: Date;
}

export interface StatusResponse {
  stockCode: string; // 단축코드
  stockName: string; // 종목명
  tradeHistories: TradeHistory[];
  currentQuantity: number; // 현재 보유 수량
  averagePrice: number; // 평균 매수가
  returnRate: number; // 수익률
  message: string; // 응답 메시지
}

export interface TradeHistory {
  tradeId: number; // 거래 ID
  tradeType: "BUY" | "SELL"; // 거래 유형 (BUY: 매수, SELL: 매도)
  quantity: number; // 거래 수량
  unitPrice: number; // 단가
  totalPrice: number; // 총 거래금액
  tradedAt: string; // 거래 시각
}

export interface NoHistoryResponse {
  stockCode: string; // 단축코드
  stockName: string; // 종목명
  tradeHistories: []; // 빈 거래내역 목록
  message: string; // 응답 메시지
}

export interface ErrorResponse {
  status: number;
  message: string;
  errorCode: string;
}

export interface CancelOrderResponse {
  status: "success" | "error";
  message: string;
}

export type PendingOrdersResponse = PendingOrder[];

export interface PendingOrder {
  orderId: number; // 보류 주문 ID
  stockCode: string; // 단축코드
  stockName: string; // 종목명
  orderType: "BUY" | "SELL"; // 주문 유형 (매수)  BUY,SELL
  quantity: number; // 주문 수량
  targetPrice: number; // 목표가
  createdAt: string; // 생성 시간
  status: "PENDING"; // 주문 상태 (대기중)
}
