import React, { useState } from "react";
import { useParams } from "react-router-dom";
import { useQueryClient } from "@tanstack/react-query";
import { Comment } from "../model/types";
import ReplyForm from "./ReplyForm";
import EditCommentForm from "./EditCommentForm";
import ReplyItem from "./ReplyItem";
import ConfirmModal from "./ConfirmModal";
import AlertModal from "./AlertModal";
import { useAuthStore } from "@/shared/store/auth";
import { useDeleteCommentMutation, useToggleLikeMutation } from "../model/mutations";
import { formatRelativeDate } from "../utils/formatDate";
import { HeartIcon, CommentIcon } from "./icons";

interface CommentItemProps {
  comment: Comment;
}

const CommentItem: React.FC<CommentItemProps> = ({ comment }) => {
  const { stockCode } = useParams<{ stockCode: string }>();
  const [showReplyForm, setShowReplyForm] = useState(false);
  const [showReplies, setShowReplies] = useState(false);
  const [isEditing, setIsEditing] = useState(false);
  const [showDeleteConfirm, setShowDeleteConfirm] = useState(false);
  const { isLoggedIn } = useAuthStore();
  const queryClient = useQueryClient();
  const profileData = queryClient.getQueryData<{ nickname: string }>(["simpleProfile"]);
  const currentUserNickname = profileData?.nickname;
  const deleteCommentMutation = useDeleteCommentMutation(stockCode ?? "");
  const toggleLikeMutation = useToggleLikeMutation(stockCode ?? "");
  const [alertModal, setAlertModal] = useState({
    show: false,
    title: "",
    message: "",
  });

  // 현재 사용자가 작성한 댓글인지 확인
  const isMyComment = currentUserNickname && comment.nickname === currentUserNickname;

  // 대댓글 개수 가져오기
  const replyCount = comment.children?.length || 0;

  // 답글 작성 버튼 처리
  const handleReplyClick = () => {
    if (isLoggedIn) {
      setShowReplyForm(!showReplyForm);
      // 답글 버튼을 클릭했을 때 항상 대댓글도 표시
      if (!showReplies) {
        setShowReplies(true);
      }
    } else {
      showAlert("알림", "로그인이 필요한 기능입니다.");
    }
  };

  // 좋아요 토글 처리
  const handleLikeToggle = () => {
    if (!isLoggedIn) {
      showAlert("알림", "로그인이 필요한 기능입니다.");
      return;
    }

    toggleLikeMutation.mutate(comment.id, {
      onError: (error) => {
        showAlert(
          "오류",
          `좋아요 처리 중 오류가 발생했습니다: ${error instanceof Error ? error.message : "알 수 없는 오류"}`,
        );
      },
    });
  };

  const handleCancelReply = () => {
    setShowReplyForm(false);
  };

  // 대댓글 토글 함수
  const toggleReplies = () => {
    setShowReplies(!showReplies);
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
  const handleDeleteComment = () => {
    if (!stockCode) return;

    deleteCommentMutation.mutate(comment.id, {
      onSuccess: () => {
        setShowDeleteConfirm(false);
        showAlert("성공", "댓글이 삭제되었습니다.");
      },
      onError: (error) => {
        setShowDeleteConfirm(false);
        showAlert(
          "오류",
          `댓글 삭제 중 오류가 발생했습니다: ${error instanceof Error ? error.message : "알 수 없는 오류"}`,
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
    <div className="rounded-lg bg-white p-4 shadow-sm">
      {/* 댓글 헤더 */}
      <div className="mb-2 flex items-center justify-between">
        <span className="font-medium">{comment.nickname}</span>
        <div className="flex items-center">
          {/* 수정/삭제 버튼 (본인 댓글인 경우만 표시) */}
          {isMyComment && !comment.deleted && !isEditing && (
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
          <span className="text-xs text-gray-500">{formatRelativeDate(comment.createdAt)}</span>
        </div>
      </div>

      {/* 댓글 내용 */}
      {isEditing ? (
        <EditCommentForm
          stockCode={stockCode ?? ""}
          commentId={comment.id}
          initialContent={comment.content}
          onCancel={toggleEditForm}
        />
      ) : (
        <div className="mb-3 text-left">
          {comment.deleted ? (
            <span className="italic text-gray-400">삭제된 댓글입니다.</span>
          ) : (
            <p>{comment.content}</p>
          )}
        </div>
      )}

      {/* 좋아요 및 액션 버튼 */}
      {!isEditing && !comment.deleted && (
        <div className="flex items-center text-sm text-gray-600">
          <button
            className="mr-4 flex items-center"
            onClick={handleLikeToggle}
            disabled={toggleLikeMutation.isPending}
          >
            <HeartIcon
              className={`mr-1 size-4 ${comment.likedByMe ? "fill-red-500 text-red-500" : "fill-none"}`}
              filled={comment.likedByMe}
            />
            <span>{comment.likeCount}</span>
          </button>

          <button className="flex items-center" onClick={handleReplyClick}>
            <CommentIcon className="mr-1 size-4" />
            <span>{showReplyForm ? "답글 취소" : "답글"}</span>
          </button>

          {/* 대댓글이 있는 경우 토글 버튼 추가 */}
          {replyCount > 0 && (
            <button
              className="ml-4 flex items-center text-xs text-blue-500 hover:text-blue-700"
              onClick={toggleReplies}
            >
              {showReplies ? "답글 숨기기" : `답글 ${replyCount}개 보기`}
            </button>
          )}
        </div>
      )}
      {/* 삭제된 댓글이지만 대댓글이 있는 경우에도 토글 버튼 표시 */}
      {comment.deleted && replyCount > 0 && (
        <div className="flex items-center text-sm text-gray-600 mt-2">
          <button
            className="flex items-center text-xs text-blue-500 hover:text-blue-700"
            onClick={toggleReplies}
          >
            {showReplies ? "답글 숨기기" : `답글 ${replyCount}개 보기`}
          </button>
        </div>
      )}
      {/* 답글 작성 폼 */}
      {showReplyForm && stockCode && !comment.deleted && (
        <div className="mt-2">
          <ReplyForm stockCode={stockCode} parentId={comment.id} onCancel={handleCancelReply} />
        </div>
      )}

      {/* 대댓글 영역 - showReplies가 true일 때만 표시 */}
      {replyCount > 0 && showReplies && (
        <div className="mt-4 ml-6 border-l-2 border-gray-200 pl-4">
          {comment.children.map((reply) => (
            <ReplyItem key={reply.id} reply={reply} stockCode={stockCode ?? ""} />
          ))}
        </div>
      )}

      {/* 삭제 확인 모달 */}
      <ConfirmModal
        show={showDeleteConfirm}
        title="댓글 삭제"
        message="정말 이 댓글을 삭제하시겠습니까?"
        onConfirm={handleDeleteComment}
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

export default CommentItem;
