import { InfiniteData, useInfiniteQuery, useQuery } from "@tanstack/react-query";
import { getMemberDashboard, getMemberTradeHistory } from "@/features/dashboard/api";
import { TransactionResponse } from "@/features/mypage/model/types";

export const useMemberDashboardQuery = (memberId: number) => {
  return useQuery({
    queryKey: ["memberDashboard", memberId],
    queryFn: () => getMemberDashboard(memberId),
    staleTime: 0,
    gcTime: 0,
  });
};

export const useMemberTradeHistoryQuery = (memberId: number) => {
  const { data, isLoading, error, fetchNextPage, hasNextPage, isFetchingNextPage } =
    useInfiniteQuery<
      TransactionResponse, // 쿼리 함수가 반환하는 데이터
      Error, // 에러 타입
      InfiniteData<TransactionResponse>, // 최종 데이터 형태
      ["memberTradeHistory", number], // 쿼리 키 타입
      string // 페이지 파라미터 타입
    >({
      queryKey: ["memberTradeHistory", memberId],
      queryFn: async ({ pageParam = "" }) => {
        const result = await getMemberTradeHistory({ size: 10, cursor: pageParam, memberId });
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
