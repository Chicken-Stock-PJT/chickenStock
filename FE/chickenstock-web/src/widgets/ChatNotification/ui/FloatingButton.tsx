import { MessageCircle } from "lucide-react";
import { cn } from "@/shared/libs/utils";

interface FloatingButtonProps {
  onClick: () => void;
  unreadCount: number;
}

export default function FloatingButton({ onClick, unreadCount }: FloatingButtonProps) {
  return (
    <button
      onClick={onClick}
      className={cn(
        "fixed bottom-5 right-5 z-50",
        "flex items-center justify-center",
        "p-4",
        "rounded-full shadow-lg",
        "bg-amber-500 text-white",
        "transition-all duration-200",
        "hover:bg-amber-600 hover:shadow-xl",
      )}
    >
      <MessageCircle size={24} />
      {/* 알림 뱃지 */}
      {unreadCount > 0 && (
        <span className="absolute -right-1 -top-1 z-10 flex size-5 items-center justify-center rounded-full bg-red-500 text-xs font-bold text-white">
          {unreadCount}
        </span>
      )}
    </button>
  );
}
