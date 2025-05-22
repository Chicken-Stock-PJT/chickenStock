import { Alert, AlertDescription, AlertTitle } from "@/shared/libs/ui/alert";
import { AlertCircle, CheckCircle2, X } from "lucide-react";

interface TradeAlertProps {
  show: boolean;
  type: "error" | "success";
  title: string;
  message: string;
  onClose: () => void;
}

export const TradeAlert = ({ show, type, title, message, onClose }: TradeAlertProps) => {
  if (!show) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center p-4">
      {/* 배경 어둡게 */}
      <div className="fixed inset-0 bg-black/70" onClick={onClose} />

      {/* Alert 컨테이너 - 너비를 더 크게 */}
      <div className="relative z-10" style={{ width: "500px", maxWidth: "90vw" }}>
        <Alert
          variant={type === "error" ? "destructive" : "default"}
          className={`
            relative
            border-2 
            shadow-2xl 
            ${type === "error" ? "border-red-600 bg-red-50" : "border-green-600 bg-green-50"}
          `}
        >
          {/* 닫기 버튼 */}
          <button
            onClick={onClose}
            className="absolute right-2 top-2 z-10 rounded-full p-1 hover:bg-gray-200"
          >
            <X className="size-4" />
          </button>

          {/* 중앙 정렬을 위한 flex 컨테이너 */}
          <div className="flex flex-col items-center px-8 py-2 text-center">
            {/* 아이콘을 별도 div로 감싸서 중앙 정렬 */}
            <div className="mb-2">
              {type === "error" ? (
                <AlertCircle className="size-5" />
              ) : (
                <CheckCircle2 className="size-5" />
              )}
            </div>

            <AlertTitle className="mb-1 text-lg font-bold">{title}</AlertTitle>
            <AlertDescription className="whitespace-nowrap text-base">{message}</AlertDescription>
          </div>
        </Alert>
      </div>
    </div>
  );
};
