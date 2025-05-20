import { InfiniteData, useInfiniteQuery, useQuery } from "@tanstack/react-query";
import { getAiBotDashboard, getAiTradeHistory } from "../api";
import { TransactionResponse } from "@/features/mypage/model/types";

export const useAiBotDashboardQuery = (botId: number) => {
  return useQuery({
    queryKey: ["aiBotDashboard", botId],
    queryFn: () => getAiBotDashboard(botId),
    staleTime: 0,
    gcTime: 0,
  });
};

// export const useAiTradeHistoryQuery = (botId: number) => {
//   return useQuery({
//     queryKey: ["aiTradeHistory", botId],
//     queryFn: () => getAiTradeHistory(botId),
//     staleTime: 0,
//     gcTime: 0,
//   });
// };

export const useAiTradeHistoryQuery = (botId: number) => {
  const { data, isLoading, error, fetchNextPage, hasNextPage, isFetchingNextPage } =
    useInfiniteQuery<
      TransactionResponse, // 쿼리 함수가 반환하는 데이터
      Error, // 에러 타입
      InfiniteData<TransactionResponse>, // 최종 데이터 형태
      ["aiTradeHistory", number], // 쿼리 키 타입
      string // 페이지 파라미터 타입
    >({
      queryKey: ["aiTradeHistory", botId],
      queryFn: async ({ pageParam = "" }) => {
        const result = await getAiTradeHistory({ size: 10, cursor: pageParam, botId });
        if (result instanceof Error) {
          throw result;
        }
        return result;
      },
      getNextPageParam: (lastPage) => (lastPage.hasNext ? lastPage.nextCursor : undefined),
      initialPageParam: "",
      staleTime: 0,
      gcTime: 0,
    });

  return { data, isLoading, error, fetchNextPage, hasNextPage, isFetchingNextPage };
};
