import { useQuery } from "@tanstack/react-query";
import { useAuthStore } from "@/shared/store/auth";
import { fetchRanking } from "../api/rankingApi";
import { RankingResponse } from "./types";

export const useRankingQuery = () => {
  const isLogin = useAuthStore((state) => state.isLoggedIn);

  return useQuery<RankingResponse>({
    queryKey: ["ranking", "total-asset", isLogin], // 로그인 상태에 따라 다른 캐시 키
    queryFn: fetchRanking,
    staleTime: 5 * 60 * 1000, // 5분간 캐시
    gcTime: 10 * 60 * 1000, // 10분간 캐시 보관
  });
};
