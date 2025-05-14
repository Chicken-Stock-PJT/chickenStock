// 전역 훅 생성 (별도 파일로 분리해도 좋음)
import { useQuery } from "@tanstack/react-query";
import { searchStocks } from "../api/api";

export function useStocksQuery() {
  return useQuery({
    queryKey: ["stocks"],
    queryFn: () => searchStocks(),
    // 데이터 유지 시간 설정 (예: 1시간)
    staleTime: 1000 * 60 * 60,
    // 최초 1회만 fetching하도록 설정 (refetchOnMount 등 비활성화)
    refetchOnMount: false,
    refetchOnWindowFocus: false,
    refetchOnReconnect: false,
  });
}
