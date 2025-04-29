export interface FilledOrder {
  side: "BUY" | "SELL"; // 거래 유형
  executedVolume: number; // 체결량
  executionPrice: number; // 체결가
  submittedAt: Date; // 신청 시간
  executedAt: Date; // 체결 시간
}

export interface OpenOrder {
  side: "BUY" | "SELL";
  remainingVolume: number;
  totalVolume: number;
  orderPrice: number;
  orderTime: Date;
}
