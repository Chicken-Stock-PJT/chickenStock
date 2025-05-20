import { useQuery } from "@tanstack/react-query";
import { useAuthStore } from "@/shared/store/auth";
import { fetchComments } from "../api";
import { CommentsParams } from "./types";

/**
 * 종목별 댓글 목록을 조회하는 React Query 훅
 */
export const useCommentsQuery = (params: CommentsParams) => {
  const { stockCode, limit = 10, cursor } = params;
  const { isLoggedIn } = useAuthStore();

  return useQuery({
    queryKey: ["comments", stockCode, limit, cursor, isLoggedIn],
    queryFn: () => fetchComments({ stockCode, limit, cursor }),
    enabled: !!stockCode,
  });
};
