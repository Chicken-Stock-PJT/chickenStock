export type MarketType = "000" | "001" | "101";
export type RankingType = "tradeAmount" | "volume" | "fluctuationRate";

export interface StockListHeaderProps {
  marketType: MarketType;
  rankingType: RankingType;
  onMarketTypeChange: (type: MarketType) => void;
  onRankingTypeChange: (type: RankingType) => void;
}

export interface Stock {
  stockCode: string;
  stockName: string;
  currentPrice: string;
  previousDayCompareSign: string;
  previousDayCompare: string;
  fluctuationRate: string;
}

export interface TradeAmountRankingItem extends Stock {
  currentRank: string; // 순위
  previousRank: string; // 전일순위
  currentTradeVolume: string; // 현재거래량
  previousTradeVolume: string; // 전일거래량
  tradeAmount: string; // 거래대금
}

export interface FluctutaionRankingItem extends Stock {
  currentTradeVolume: string; // 현재거래량
  sellRemaining: string; // 매도잔량
  buyRemaining: string; // 매수잔량
  contractStrength: string; // 체결강도
}

export interface VolumeRankingItem extends Stock {
  tradeAmount: string; // 거래대금
  tradeVolume: string; // 거래량
  previousRatio: string; // 전일비
  tradeTurnoverRate: string; // 거래회전율
}

export interface StockListResponse {
  rankingType: "TRADE_AMOUNT" | "FLUCTUATION_RATE" | "TRADE_VOLUME";
  rankingItems: TradeAmountRankingItem[] | FluctutaionRankingItem[] | VolumeRankingItem[];
  hasNext: boolean;
  nextKey: string;
  code: number;
  message: string;
}

export type StockProps = {
  rankingType: RankingType;
  rank: number;
} & (
  | ({ rankingType: "tradeAmount" } & TradeAmountRankingItem)
  | ({ rankingType: "volume" } & VolumeRankingItem)
  | ({ rankingType: "fluctuationRate" } & FluctutaionRankingItem)
);

export interface StockChartResponse {
  stockCode: string;
  stockName: string;
  chartData: StockChartData[];
  hasNext: boolean;
  nextKey: string;
  code: number;
  message: string;
}

export interface StockChartData {
  date: string;
  currentPrice: string;
  openPrice: string;
  highPrice: string;
  lowPrice: string;
  volume: string;
  tradingValue: string;
  modifiedRatio: string;
  previousClosePrice: string;
}
