export interface ChartData {
  date: string;
  currentPrice: string;
  openPrice: string;
  highPrice: string;
  lowPrice: string;
  volume: string;
  tradingValue: string;
}

export interface ChartResponse {
  stockCode: string;
  chartType: string;
  chartData: ChartData[];
  hasNext: boolean;
  nextKey: string;
  code: number;
  message: string;
}
