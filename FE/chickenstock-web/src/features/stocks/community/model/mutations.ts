import { useMutation, useQueryClient } from "@tanstack/react-query";
import { createComment, createReply, updateComment, deleteComment, toggleLike } from "../api";

/**
 * 댓글을 작성하는 React Query Mutation 훅
 */
export const useCreateCommentMutation = (stockCode: string) => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (content: string) => createComment(stockCode, content),
    onSuccess: () => {
      // 댓글 작성 성공 시 댓글 목록을 다시 불러옵니다
      void queryClient.invalidateQueries({ queryKey: ["comments", stockCode] });
    },
  });
};

/**
 * 대댓글을 작성하는 React Query Mutation 훅
 */
export const useCreateReplyMutation = (stockCode: string) => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: ({ content, parentId }: { content: string; parentId: number }) =>
      createReply(stockCode, content, parentId),
    onSuccess: () => {
      // 대댓글 작성 성공 시 댓글 목록을 다시 불러옵니다
      void queryClient.invalidateQueries({ queryKey: ["comments", stockCode] });
    },
  });
};

/**
 * 댓글/대댓글을 수정하는 React Query Mutation 훅
 */
export const useUpdateCommentMutation = (stockCode: string) => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: ({ commentId, content }: { commentId: number; content: string }) =>
      updateComment(stockCode, commentId, content),
    onSuccess: () => {
      // 댓글 수정 성공 시 댓글 목록을 다시 불러옵니다
      void queryClient.invalidateQueries({ queryKey: ["comments", stockCode] });
    },
  });
};

/**
 * 댓글/대댓글을 삭제하는 React Query Mutation 훅
 */
export const useDeleteCommentMutation = (stockCode: string) => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (commentId: number) => deleteComment(stockCode, commentId),
    onSuccess: () => {
      // 댓글 삭제 성공 시 댓글 목록을 다시 불러옵니다
      void queryClient.invalidateQueries({ queryKey: ["comments", stockCode] });
    },
  });
};

/**
 * 댓글/대댓글 좋아요 토글 React Query Mutation 훅
 */
export const useToggleLikeMutation = (stockCode: string) => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (commentId: number) => toggleLike(stockCode, commentId),
    onSuccess: () => {
      // 좋아요 토글 성공 시 댓글 목록을 다시 불러옵니다
      void queryClient.invalidateQueries({ queryKey: ["comments", stockCode] });
    },
  });
};
