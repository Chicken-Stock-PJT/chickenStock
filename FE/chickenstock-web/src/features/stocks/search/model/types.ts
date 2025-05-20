// 서버로부터 받는 개별 주식 정보 타입
export interface StockInfo {
  shortCode: string;
  shortName: string;
  market: string;
  stockType: string;
  faceValue: string;
}

// API 응답 타입 (배열)
export type StockInfoResponse = StockInfo[];
