export interface Ranking {
  rank: number;
  nickname: string;
  totalAsset: number;
}

export interface RankingResponse {
  topRankings: Ranking[];
  myRank?: Ranking; // 로그인한 경우에만 존재
}
