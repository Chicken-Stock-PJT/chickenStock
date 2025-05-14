import { useQuery } from "@tanstack/react-query";
import { useAuthStore } from "@/shared/store/auth";
import { fetchRanking } from "../api/rankingApi";
import { RankingResponse } from "./types";

export const useRankingQuery = () => {
  const isLogin = useAuthStore((state) => state.isLoggedIn);

  return useQuery<RankingResponse>({
    queryKey: ["ranking", "total-asset", isLogin], // 로그인 상태에 따라 다른 캐시 키
    queryFn: fetchRanking,
    staleTime: 0, // 항상 최신 데이터를 가져오도록 설정
    gcTime: 0, // 캐시를 저장하지 않음
    refetchOnWindowFocus: true, // 포커스 시 재조회
    refetchOnMount: true, // 마운트 시 재조회
  });
};
