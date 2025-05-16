import { useQuery, useInfiniteQuery, InfiniteData } from "@tanstack/react-query";
import {
  getDailyProfitRate,
  getPortfolio,
  getTransactions,
  fetchPendingOrders,
  getMemberDashboard,
} from "../api";
import { AxiosResponse } from "axios";
import { ErrorResponse, DailyProfitRateResponse, TransactionResponse, PendingOrder } from "./types";
import { MemberDashboardResponse } from "@/features/dashboard/model/types";

export const useGetPortfolio = () => {
  const { data, isLoading, error, refetch } = useQuery({
    queryKey: ["portfolio"],
    queryFn: getPortfolio,
    staleTime: 0,
    gcTime: 0,
  });
  return { data, isLoading, error, refetch };
};

export const useGetTransactions = () => {
  const { data, isLoading, error, fetchNextPage, hasNextPage, isFetchingNextPage } =
    useInfiniteQuery<
      TransactionResponse, // 쿼리 함수가 반환하는 데이터
      Error, // 에러 타입
      InfiniteData<TransactionResponse>, // 최종 데이터 형태
      ["transactions"], // 쿼리 키 타입
      string // 페이지 파라미터 타입
    >({
      queryKey: ["transactions"],
      queryFn: async ({ pageParam = "" }) => {
        const result = await getTransactions({ size: 10, cursor: pageParam });
        if (result instanceof Error) {
          throw result;
        }
        return result;
      },
      getNextPageParam: (lastPage) => (lastPage.hasNext ? lastPage.nextCursor : undefined),
      initialPageParam: "",
    });

  return { data, isLoading, error, fetchNextPage, hasNextPage, isFetchingNextPage };
};

export const useGetDailyProfitRate = () => {
  return useQuery<DailyProfitRateResponse, AxiosResponse<ErrorResponse>>({
    queryKey: ["dailyProfitRate"],
    queryFn: getDailyProfitRate,
  });
};

export const usePendingOrdersQuery = () => {
  return useQuery<PendingOrder[]>({
    queryKey: ["pendingOrders"],
    queryFn: fetchPendingOrders,
    refetchInterval: 30000, // 30초마다 자동 새로고침
  });
};

export const useMemberDashboardQuery = () => {
  return useQuery<MemberDashboardResponse>({
    queryKey: ["memberDashboard"],
    queryFn: getMemberDashboard,
    staleTime: 0,
    gcTime: 0,
  });
};
