import apiClient from "@/shared/api/axios";
import { CommentsParams, CommentsResponse, Comment } from "../model/types";

/**
 * 종목별 댓글과 대댓글을 조회하는 API
 * @param params 조회 매개변수 (종목코드, 페이지 크기, 커서)
 * @returns 댓글 목록과 다음 페이지 커서
 */
export const fetchComments = async (params: CommentsParams): Promise<CommentsResponse> => {
  const { stockCode, limit, cursor } = params;

  let url = `/stocks/${stockCode}/comments`;
  const queryParams = new URLSearchParams();

  if (limit) {
    queryParams.append("limit", limit.toString());
  }

  if (cursor) {
    queryParams.append("cursor", cursor);
  }

  const queryString = queryParams.toString();
  if (queryString) {
    url += `?${queryString}`;
  }

  const response = await apiClient.get<CommentsResponse>(url);
  return response.data;
};

/**
 * 댓글을 작성하는 API
 * @param stockCode 종목 단축코드
 * @param content 댓글 내용
 * @returns 생성된 댓글 정보
 */
export const createComment = async (stockCode: string, content: string): Promise<Comment> => {
  const response = await apiClient.post<Comment>(`/stocks/${stockCode}/comments`, { content });
  return response.data;
};

/**
 * 대댓글을 작성하는 API
 * @param stockCode 종목 단축코드
 * @param content 대댓글 내용
 * @param parentId 부모 댓글 ID
 * @returns 생성된 대댓글 정보
 */
export const createReply = async (
  stockCode: string,
  content: string,
  parentId: number,
): Promise<Comment> => {
  const response = await apiClient.post<Comment>(`/stocks/${stockCode}/comments/reply`, {
    content,
    parentId,
  });
  return response.data;
};

/**
 * 댓글/대댓글을 수정하는 API
 * @param stockCode 종목 단축코드
 * @param commentId 댓글 ID
 * @param content 수정할 내용
 * @returns 수정된 댓글 정보
 */
export const updateComment = async (
  stockCode: string,
  commentId: number,
  content: string,
): Promise<Comment> => {
  const response = await apiClient.put<Comment>(`/stocks/${stockCode}/comments/${commentId}`, {
    content,
  });
  return response.data;
};

/**
 * 댓글/대댓글을 삭제하는 API
 * @param stockCode 종목 단축코드
 * @param commentId 댓글 ID
 * @returns 빈 응답
 */
export const deleteComment = async (stockCode: string, commentId: number): Promise<void> => {
  await apiClient.delete(`/stocks/${stockCode}/comments/${commentId}`);
};

/**
 * 댓글/대댓글 좋아요 토글 API
 * @param stockCode 종목 단축코드
 * @param commentId 댓글 ID
 * @returns 좋아요 상태 정보
 */
export interface LikeResponse {
  likeCount: number;
  liked: boolean;
}

export const toggleLike = async (stockCode: string, commentId: number): Promise<LikeResponse> => {
  const response = await apiClient.post<LikeResponse>(
    `/stocks/${stockCode}/comments/${commentId}/like`,
  );
  return response.data;
};
