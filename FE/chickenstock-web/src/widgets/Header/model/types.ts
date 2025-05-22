export interface TotalAssetRanking {
  rank: number;
  nickname: string;
  totalAsset: number;
  memberId: number;
}

export interface ReturnRateRanking {
  rank: number;
  nickname: string;
  returnRate: number;
  memberId: number;
}

export interface TotalAssetRankingResponse {
  topRankings: TotalAssetRanking[];
  myRank?: TotalAssetRanking; // 로그인한 경우에만 존재
}

export interface ReturnRateRankingResponse {
  topRankings: ReturnRateRanking[];
  myRank?: ReturnRateRanking; // 로그인한 경우에만 존재
}

export type RankingType = "totalAsset" | "returnRate";
