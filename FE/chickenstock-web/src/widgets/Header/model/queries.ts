import { useQuery } from "@tanstack/react-query";
import { useAuthStore } from "@/shared/store/auth";
import { fetchTotalAssetRanking, fetchReturnRateRanking } from "../api/rankingApi";
import { TotalAssetRankingResponse, ReturnRateRankingResponse } from "./types";

export const useTotalAssetRankingQuery = () => {
  const isLogin = useAuthStore((state) => state.isLoggedIn);

  return useQuery<TotalAssetRankingResponse>({
    queryKey: ["ranking", "total-asset", isLogin], // 로그인 상태에 따라 다른 캐시 키
    queryFn: fetchTotalAssetRanking,
    staleTime: 0, // 항상 최신 데이터를 가져오도록 설정
    gcTime: 0, // 캐시를 저장하지 않음
    refetchOnWindowFocus: true, // 포커스 시 재조회
    refetchOnMount: true, // 마운트 시 재조회
  });
};

export const useReturnRateRankingQuery = () => {
  const isLogin = useAuthStore((state) => state.isLoggedIn);

  return useQuery<ReturnRateRankingResponse>({
    queryKey: ["ranking", "return-rate", isLogin], // 로그인 상태에 따라 다른 캐시 키
    queryFn: fetchReturnRateRanking,
    staleTime: 0, // 항상 최신 데이터를 가져오도록 설정
    gcTime: 0, // 캐시를 저장하지 않음
    refetchOnWindowFocus: true, // 포커스 시 재조회
    refetchOnMount: true, // 마운트 시 재조회
  });
};
