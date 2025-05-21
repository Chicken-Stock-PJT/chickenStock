// 지정가 매수/매도 요청 파라미터터
export interface LimitTradeRequest {
  stockCode: string;
  quantity: number;
  price: number;
}

// 시장가 매수/매도 요청 파라미터터
export interface MarketTradeRequest {
  stockCode: string;
  quantity: number;
}

// 거래 성공 시 응답
export interface TradeResponse {
  tradeHistoryId: number;
  stockCode: string;
  stockName: string;
  tradeType: string;
  quantity: number;
  unitPrice: number;
  totalPrice: number;
  tradedAt: string;
  status: string;
}

// 지정가 매수/매도 주문 접수 시 응답
export interface PendingResponse {
  orderId: number;
  stockCode: string;
  stockName: string;
  tradeType: string;
  quantity: number;
  unitPrice: number;
  totalPrice: number;
  tradedAt: string;
  status: string;
  message: string;
}

export interface ErrorResponse {
  status: number;
  code: string;
  error: string;
  message: string;
  path: string;
  timestamp: string;
}

export interface AvailableQuantityResponse {
  shortCode: string;
  availableQuantity: number;
}
