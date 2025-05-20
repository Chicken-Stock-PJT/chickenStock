import React from "react";

interface AlertModalProps {
  show: boolean;
  title: string;
  message: string;
  onConfirm: () => void;
  confirmText?: string;
}

export const AlertModal: React.FC<AlertModalProps> = ({
  show,
  title,
  message,
  onConfirm,
  confirmText = "확인",
}) => {
  if (!show) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
      {/* 배경 어둡게 */}
      <div className="fixed inset-0 bg-black/50" onClick={onConfirm} />

      {/* 모달 컨테이너 */}
      <div className="relative z-10 w-full max-w-sm">
        <div className="rounded-lg bg-white p-6 shadow-xl">
          {/* 제목 */}
          <h3 className="mb-2 text-lg font-semibold text-gray-900">{title}</h3>

          {/* 메시지 */}
          <p className="mb-6 text-sm text-gray-600">{message}</p>

          {/* 버튼 */}
          <div className="flex justify-end">
            <button
              onClick={onConfirm}
              className="rounded-md bg-yellow-200 px-4 py-2 text-sm font-medium text-gray-900 transition-colors hover:bg-yellow-100 focus:outline-none focus:ring-2 focus:ring-yellow-300 focus:ring-offset-2"
            >
              {confirmText}
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default AlertModal;
