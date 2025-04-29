// 웹소켓
// import { StockInfo } from "../../types";

export interface WebSocketMessage {
  type: string;
  message?: string;
  action?: string;
  stockCode?: string;
}

export interface StockPriceData extends WebSocketMessage {
  type: "stockPrice";
  currentPrice: string;
  priceChange: string;
  changeRate: string;
  timestamp: string;
}

export interface StockBidAskData extends WebSocketMessage {
  type: "stockBidAsk";
  timestamp: string;
  askPrices: Record<string, string>;
  askVolumes: Record<string, string>;
  bidPrices: Record<string, string>;
  bidVolumes: Record<string, string>;
}

export type WebSocketResponse = WebSocketMessage | StockPriceData | StockBidAskData;

// 차트 헤더
export interface ChartHeaderProps {
  stockCode: string; // 종목 코드 (증권시장에서 주식을 식별하는 고유 번호)
  stockName: string; // 종목 이름 (회사명)
  // market: "KOSPI" | "KOSDAQ"; // 상장된 시장 (KOSPI, KOSDAQ)
  // stockType: string; // 주식 유형 (보통주, 우선주 등)
  // faceValue: string; // 액면가 (주식 1주당 표시된 금액, 단위: 원)
  priceChange: string; // 전일대비 변동 금액 (오늘 가격 - 어제 가격, 단위: 원)
  changeRate: string; // 등락률 (전일대비 변동률, 단위: %)
  currentPrice: string;
}

// 차트 바디
export interface ChartData {
  date: string;
  currentPrice: string;
  openPrice: string;
  highPrice: string;
  lowPrice: string;
  volume: string; // 거래량
  // tradingValue: string; // 거래대금
  // modifiedRatio: string; // 수정비율
  // previousClosePrice: string; // 전일종가
}

export interface ChartRequest {
  stockCode: string;
  chartType: string;
  hasNext: boolean;
  nextKey: string;
}

export interface ChartResponse {
  stockCode: "DAILY" | "WEEKLY" | "MONTHLY" | "YEARLY" | "MINUTE";
  chartType: string;
  chartData: ChartData[];
  hasNext: boolean;
  nextKey: string;
  code: number;
  message: string;
}
