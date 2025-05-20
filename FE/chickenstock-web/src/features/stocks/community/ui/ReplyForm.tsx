import React, { useState } from "react";
import { useCreateReplyMutation } from "../model/mutations";
import { useAuthStore } from "@/shared/store/auth";
import AlertModal from "./AlertModal";

interface ReplyFormProps {
  stockCode: string;
  parentId: number;
  onCancel: () => void;
}

const ReplyForm: React.FC<ReplyFormProps> = ({ stockCode, parentId, onCancel }) => {
  const [content, setContent] = useState("");
  const { isLoggedIn } = useAuthStore();
  const createReplyMutation = useCreateReplyMutation(stockCode);
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

    createReplyMutation.mutate(
      { content, parentId },
      {
        onSuccess: () => {
          setContent(""); // 입력 필드 초기화
          showAlert("성공", "답글이 작성되었습니다.");
          onCancel(); // 폼 닫기
        },
        onError: (error) => {
          showAlert(
            "오류",
            `답글 작성 중 오류가 발생했습니다: ${error instanceof Error ? error.message : "알 수 없는 오류"}`,
          );
        },
      },
    );
  };

  return (
    <>
      <form onSubmit={handleSubmit} className="mb-3 mt-2">
        <textarea
          className="w-full resize-none rounded-lg border border-gray-200 p-2 text-sm focus:outline-none focus:ring-2 focus:ring-yellow-200"
          rows={2}
          placeholder="답글을 작성해주세요"
          value={content}
          onChange={(e) => setContent(e.target.value)}
          disabled={!isLoggedIn || createReplyMutation.isPending}
        />

        <div className="mt-2 flex justify-end space-x-2">
          <button
            type="button"
            onClick={onCancel}
            className="rounded-md px-3 py-1 text-xs font-medium text-gray-700 hover:bg-gray-100"
          >
            취소
          </button>
          <button
            type="submit"
            disabled={!isLoggedIn || !content.trim() || createReplyMutation.isPending}
            className={`rounded-md px-3 py-1 text-xs font-medium ${
              !isLoggedIn || !content.trim() || createReplyMutation.isPending
                ? "cursor-not-allowed bg-gray-100 text-gray-400"
                : "bg-yellow-200 text-gray-900 hover:bg-yellow-100"
            }`}
          >
            {createReplyMutation.isPending ? "작성 중..." : "답글 작성"}
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

export default ReplyForm;
