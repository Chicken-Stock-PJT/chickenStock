import { useQuery, useInfiniteQuery, InfiniteData } from "@tanstack/react-query";
import { getDailyProfitRate, getPortfolio, getTransactions } from "../api";
import { AxiosResponse } from "axios";
import { ErrorResponse, DailyProfitRateResponse, TransactionResponse } from "./types";

export const useGetPortfolio = () => {
  const { data, isLoading, error } = useQuery({
    queryKey: ["portfolio"],
    queryFn: getPortfolio,
    staleTime: 0,
    gcTime: 0,
  });
  return { data, isLoading, error };
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
