import { useMutation, useQuery, useInfiniteQuery, InfiniteData } from "@tanstack/react-query";
import { useAuthStore } from "@/shared/store/auth";
import { getDailyProfitRate, getPortfolio, getTransactions, updateNickname } from "../api";
import { AxiosError, AxiosResponse } from "axios";
import { DailyProfitRateResponse, TransactionResponse } from "./types";

interface ErrorResponse {
  status: number;
  code: string;
  error: string;
  message: string;
  path: string;
  timestamp: string;
}

export const useUpdateNickname = () => {
  //   const queryClient = useQueryClient();
  const { mutateAsync } = useMutation({
    mutationFn: (nickname: string) => updateNickname(nickname),
    onSuccess: (_, nickname) => {
      // 쿼리 무효화
      //   void queryClient.invalidateQueries({ queryKey: ["profile"] });
      useAuthStore.getState().setSimpleProfile({ nickname });
    },
    onError: (error: AxiosError<ErrorResponse>) => {
      const errorMessage = error.response?.data.message ?? "닉네임 수정에 실패했습니다.";
      alert(errorMessage);
    },
  });
  return { mutateAsync };
};

export const useGetPortfolio = () => {
  const { data, isLoading, error } = useQuery({
    queryKey: ["portfolio"],
    queryFn: getPortfolio,
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
