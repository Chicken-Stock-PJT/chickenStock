// @/features/stocks/community/ui/EditCommentForm.tsx
import React, { useState } from "react";
import { useUpdateCommentMutation } from "../model/mutations";
import AlertModal from "./AlertModal";

interface EditCommentFormProps {
  stockCode: string;
  commentId: number;
  initialContent: string;
  onCancel: () => void;
  isReply?: boolean;
}

const EditCommentForm: React.FC<EditCommentFormProps> = ({
  stockCode,
  commentId,
  initialContent,
  onCancel,
  isReply = false,
}) => {
  const [content, setContent] = useState(initialContent);
  const updateCommentMutation = useUpdateCommentMutation(stockCode);
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
      showAlert("알림", "내용을 입력해주세요.");
      return;
    }

    updateCommentMutation.mutate(
      { commentId, content },
      {
        onSuccess: () => {
          showAlert("성공", `${isReply ? "답글" : "댓글"}이 수정되었습니다.`);
          onCancel(); // 폼 닫기
        },
        onError: (error) => {
          showAlert(
            "오류",
            `수정 중 오류가 발생했습니다: ${error instanceof Error ? error.message : "알 수 없는 오류"}`,
          );
        },
      },
    );
  };

  return (
    <>
      <form onSubmit={handleSubmit} className="mb-3 mt-2">
        <textarea
          className={`w-full resize-none rounded-lg border border-gray-200 p-2 ${isReply ? "text-sm" : ""} focus:outline-none focus:ring-2 focus:ring-yellow-200`}
          rows={isReply ? 2 : 3}
          placeholder={`${isReply ? "답글" : "댓글"} 내용을 수정해주세요`}
          value={content}
          onChange={(e) => setContent(e.target.value)}
          disabled={updateCommentMutation.isPending}
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
            disabled={!content.trim() || updateCommentMutation.isPending}
            className={`rounded-md px-3 py-1 text-xs font-medium ${
              !content.trim() || updateCommentMutation.isPending
                ? "cursor-not-allowed bg-gray-100 text-gray-400"
                : "bg-yellow-200 text-gray-900 hover:bg-yellow-100"
            }`}
          >
            {updateCommentMutation.isPending ? "수정 중..." : "수정"}
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

export default EditCommentForm;
