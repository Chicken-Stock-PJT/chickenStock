// @/features/stocks/community/ui/ReplyItem.tsx
import React, { useState } from "react";
import { useQueryClient } from "@tanstack/react-query";
import { Comment } from "../model/types";
import EditCommentForm from "./EditCommentForm";
import ConfirmModal from "./ConfirmModal";
import AlertModal from "./AlertModal";
import { useDeleteCommentMutation, useToggleLikeMutation } from "../model/mutations";
import { formatRelativeDate } from "../utils/formatDate";
import { HeartIcon } from "./icons";
import { useAuthStore } from "@/shared/store/auth";

interface ReplyItemProps {
  reply: Comment;
  stockCode: string;
}

const ReplyItem: React.FC<ReplyItemProps> = ({ reply, stockCode }) => {
  const [isEditing, setIsEditing] = useState(false);
  const [showDeleteConfirm, setShowDeleteConfirm] = useState(false);
  const queryClient = useQueryClient();
  const { isLoggedIn } = useAuthStore();
  const profileData = queryClient.getQueryData<{ nickname: string }>(["simpleProfile"]);
  const currentUserNickname = profileData?.nickname;
  const deleteCommentMutation = useDeleteCommentMutation(stockCode);
  const toggleLikeMutation = useToggleLikeMutation(stockCode);
  const [alertModal, setAlertModal] = useState({
    show: false,
    title: "",
    message: "",
  });

  // 현재 사용자가 작성한 댓글인지 확인
  const isMyReply = currentUserNickname && reply.nickname === currentUserNickname;

  // 좋아요 토글 처리
  const handleLikeToggle = () => {
    if (!isLoggedIn) {
      showAlert("알림", "로그인이 필요한 기능입니다.");
      return;
    }

    toggleLikeMutation.mutate(reply.id, {
      onError: (error) => {
        showAlert(
          "오류",
          `좋아요 처리 중 오류가 발생했습니다: ${error instanceof Error ? error.message : "알 수 없는 오류"}`,
        );
      },
    });
  };

  // 수정 폼 토글
  const toggleEditForm = () => {
    setIsEditing(!isEditing);
  };

  // 삭제 확인 모달 표시
  const showDeleteModal = () => {
    setShowDeleteConfirm(true);
  };

  // 댓글 삭제 처리
  const handleDeleteReply = () => {
    deleteCommentMutation.mutate(reply.id, {
      onSuccess: () => {
        setShowDeleteConfirm(false);
        showAlert("성공", "답글이 삭제되었습니다.");
      },
      onError: (error) => {
        setShowDeleteConfirm(false);
        showAlert(
          "오류",
          `답글 삭제 중 오류가 발생했습니다: ${error instanceof Error ? error.message : "알 수 없는 오류"}`,
        );
      },
    });
  };

  const showAlert = (title: string, message: string) => {
    setAlertModal({
      show: true,
      title,
      message,
    });
  };

  const closeAlert = () => {
    setAlertModal({
      ...alertModal,
      show: false,
    });
  };

  return (
    <div className="mb-3 last:mb-0">
      {/* 대댓글 헤더 */}
      <div className="mb-1 flex items-center justify-between">
        <span className="text-sm font-medium">{reply.nickname}</span>
        <div className="flex items-center">
          {/* 수정/삭제 버튼 (본인 댓글인 경우만 표시) */}
          {isMyReply && !reply.deleted && !isEditing && (
            <div className="mr-2 flex items-center space-x-2">
              <button
                onClick={toggleEditForm}
                className="text-xs text-gray-500 hover:text-gray-700"
              >
                수정
              </button>
              <span className="text-gray-500">|</span>
              <button
                onClick={showDeleteModal}
                className="text-xs text-gray-500 hover:text-gray-700"
              >
                삭제
              </button>
            </div>
          )}
          <span className="text-xs text-gray-500">{formatRelativeDate(reply.createdAt)}</span>
        </div>
      </div>

      {/* 대댓글 내용 */}
      {isEditing ? (
        <EditCommentForm
          stockCode={stockCode}
          commentId={reply.id}
          initialContent={reply.content}
          onCancel={toggleEditForm}
          isReply={true}
        />
      ) : (
        <div className="mb-2 text-left">
          {reply.deleted ? (
            <span className="text-sm italic text-gray-400">삭제된 댓글입니다.</span>
          ) : (
            <p className="text-sm">{reply.content}</p>
          )}
        </div>
      )}

      {/* 대댓글 좋아요 버튼 */}
      {!isEditing && !reply.deleted && (
        <div className="flex items-center text-xs text-gray-600">
          <button
            className="flex items-center"
            onClick={handleLikeToggle}
            disabled={toggleLikeMutation.isPending}
          >
            <HeartIcon
              className={`mr-1 size-3 ${reply.likedByMe ? "fill-red-500 text-red-500" : "fill-none"}`}
              filled={reply.likedByMe}
              size={12}
            />
            <span>{reply.likeCount}</span>
          </button>
        </div>
      )}

      {/* 삭제 확인 모달 */}
      <ConfirmModal
        show={showDeleteConfirm}
        title="답글 삭제"
        message="정말 이 답글을 삭제하시겠습니까?"
        onConfirm={handleDeleteReply}
        onCancel={() => setShowDeleteConfirm(false)}
        confirmText="삭제"
      />

      {/* 알림 모달 */}
      <AlertModal
        show={alertModal.show}
        title={alertModal.title}
        message={alertModal.message}
        onConfirm={closeAlert}
      />
    </div>
  );
};

export default ReplyItem;
