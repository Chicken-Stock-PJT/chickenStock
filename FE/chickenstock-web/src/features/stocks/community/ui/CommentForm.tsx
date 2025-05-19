// @/features/stocks/community/ui/CommentForm.tsx
import React, { useState } from "react";
import { useCreateCommentMutation } from "../model/mutations";
import { useAuthStore } from "@/shared/store/auth";
import AlertModal from "./AlertModal";

interface CommentFormProps {
  stockCode: string;
}

const CommentForm: React.FC<CommentFormProps> = ({ stockCode }) => {
  const [content, setContent] = useState("");
  const { isLoggedIn } = useAuthStore();
  const createCommentMutation = useCreateCommentMutation(stockCode);
  const [alertModal, setAlertModal] = useState({
    show: false,
    title: "",
    message: "",
  });

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

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    if (!content.trim()) {
      showAlert("알림", "댓글 내용을 입력해주세요.");
      return;
    }

    if (!isLoggedIn) {
      showAlert("알림", "로그인이 필요한 기능입니다.");
      return;
    }

    createCommentMutation.mutate(content, {
      onSuccess: () => {
        setContent(""); // 입력 필드 초기화
        showAlert("성공", "댓글이 작성되었습니다.");
      },
      onError: (error) => {
        showAlert(
          "오류",
          `댓글 작성 중 오류가 발생했습니다: ${error instanceof Error ? error.message : "알 수 없는 오류"}`,
        );
      },
    });
  };

  return (
    <>
      <form onSubmit={handleSubmit} className="mb-4 rounded-lg bg-white p-4 shadow-sm">
        <textarea
          className="w-full resize-none rounded-lg border border-gray-200 p-3 focus:outline-none focus:ring-2 focus:ring-yellow-200"
          rows={3}
          placeholder={isLoggedIn ? "댓글을 작성해주세요" : "로그인 후 댓글을 작성할 수 있습니다"}
          value={content}
          onChange={(e) => setContent(e.target.value)}
          disabled={!isLoggedIn || createCommentMutation.isPending}
        />

        <div className="mt-2 flex justify-end">
          <button
            type="submit"
            disabled={!isLoggedIn || !content.trim() || createCommentMutation.isPending}
            className={`rounded-md px-4 py-2 text-sm font-medium ${
              !isLoggedIn || !content.trim() || createCommentMutation.isPending
                ? "cursor-not-allowed bg-gray-100 text-gray-400"
                : "bg-yellow-200 text-gray-900 hover:bg-yellow-100"
            }`}
          >
            {createCommentMutation.isPending ? "작성 중..." : "댓글 작성"}
          </button>
        </div>
      </form>

      <AlertModal
        show={alertModal.show}
        title={alertModal.title}
        message={alertModal.message}
        onConfirm={closeAlert}
      />
    </>
  );
};

export default CommentForm;
