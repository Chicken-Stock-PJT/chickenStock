interface LogoutModalProps {
  show: boolean;
  onConfirm: () => void;
  onCancel: () => void;
}

export const LogoutModal = ({ show, onConfirm, onCancel }: LogoutModalProps) => {
  if (!show) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
      {/* 배경 어둡게 */}
      <div className="fixed inset-0 bg-black/50" onClick={onCancel} />

      {/* 모달 컨테이너 */}
      <div className="relative z-10 w-full max-w-sm">
        <div className="rounded-lg bg-white p-6 shadow-xl">
          {/* 제목 */}
          <h3 className="mb-2 text-lg font-semibold text-gray-900">로그아웃</h3>

          {/* 메시지 */}
          <p className="mb-6 text-sm text-gray-600">정말 로그아웃 하시겠습니까?</p>

          {/* 버튼들 */}
          <div className="flex gap-3">
            <button
              onClick={onCancel}
              className="flex-1 rounded-md border border-gray-300 bg-white px-4 py-2 text-sm font-medium text-gray-700 transition-colors hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-gray-500 focus:ring-offset-2"
            >
              취소
            </button>
            <button
              onClick={onConfirm}
              className="flex-1 rounded-md bg-red-600 px-4 py-2 text-sm font-medium text-white transition-colors hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-red-500 focus:ring-offset-2"
            >
              로그아웃
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};
