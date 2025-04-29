export interface StockInfo {
  shortCode: string; // 종목 코드 (증권시장에서 주식을 식별하는 고유 번호)
  shortName: string; // 종목 이름 (회사명)
  market: "KOSPI" | "KOSDAQ"; // 상장된 시장 (KOSPI, KOSDAQ)
  stockType: string; // 주식 유형 (보통주, 우선주 등)
  faceValue: string; // 액면가 (주식 1주당 표시된 금액, 단위: 원)
  prevDayCompare: string; // 전일대비 변동 금액 (오늘 가격 - 어제 가격, 단위: 원)
  fluctuationRate: string; // 등락률 (전일대비 변동률, 단위: %)
}
