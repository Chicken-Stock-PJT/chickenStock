import nxtStocksUrl from "@/assets/nxtStocks.csv?url";
// NXT 상장 종목 코드 목록
let nxtStockCodes: Set<string> | null = null;

// CSV 파일에서 NXT 상장 종목 코드 목록을 파싱하는 함수
const parseNxtStocks = async () => {
  if (nxtStockCodes) return nxtStockCodes;

  try {
    const response = await fetch(nxtStocksUrl);
    const text = await response.text();
    const lines = text.split("\n");

    nxtStockCodes = new Set();

    // 첫 번째 줄은 헤더이므로 건너뜁니다
    for (let i = 1; i < lines.length; i++) {
      const line = lines[i].trim();
      if (!line) continue;

      const [code] = line.split(",");
      if (code) {
        // 'A' 접두사 제거
        nxtStockCodes.add(code.substring(1));
      }
    }
    return nxtStockCodes;
  } catch (error) {
    console.error("NXT 상장 종목 목록 파싱 중 오류 발생:", error);
    return new Set();
  }
};

// 특정 종목이 NXT 상장 종목인지 확인하는 함수
export const isNxtStock = async (stockCode: string): Promise<boolean> => {
  const codes = await parseNxtStocks();
  return codes.has(stockCode);
};

// NXT 상장 종목 목록을 초기화하는 함수
export const initializeNxtStocks = async () => {
  await parseNxtStocks();
};
