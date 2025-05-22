import { useChatNotificationStore } from "../model/store";
import { cn } from "@/shared/libs/utils";
import ChatTab from "./ChatTab";
import NotificationTab from "./NotificationTab";
import FloatingButton from "./FloatingButton";
import { useEffect, useRef } from "react";

export default function ChatNotification() {
  const { isOpen, setOpen, notifications, activeTab, setActiveTab } = useChatNotificationStore();
  const panelRef = useRef<HTMLDivElement>(null);
  const buttonRef = useRef<HTMLDivElement>(null);

  // 읽지 않은 알림 개수
  const unreadCount = notifications.filter((n) => !n.isRead).length;

  // 외부 클릭 감지
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        isOpen &&
        panelRef.current &&
        !panelRef.current.contains(event.target as Node) &&
        buttonRef.current &&
        !buttonRef.current.contains(event.target as Node)
      ) {
        setOpen(false);
      }
    };

    document.addEventListener("mousedown", handleClickOutside);
    return () => {
      document.removeEventListener("mousedown", handleClickOutside);
    };
  }, [isOpen, setOpen]);

  return (
    <>
      {/* 플로팅 버튼 */}
      <div ref={buttonRef}>
        <FloatingButton onClick={() => setOpen(!isOpen)} unreadCount={unreadCount} />
      </div>

      {/* 채팅/알림 패널 */}
      {isOpen && (
        <div
          ref={panelRef}
          className="fixed bottom-24 right-5 z-50 h-[calc(100vh-8rem)] max-h-[600px] w-[calc(100vw-2.5rem)] max-w-[400px] overflow-hidden rounded-xl bg-white shadow-2xl sm:bottom-8 sm:right-8 sm:w-[400px]"
        >
          <div className="flex h-full flex-col">
            {/* 탭 버튼 */}
            <div className="bg-white px-4 pt-4">
              <div className="flex rounded-lg bg-gray-100 p-1">
                <TabButton active={activeTab === "chat"} onClick={() => setActiveTab("chat")}>
                  채팅
                </TabButton>
                <TabButton
                  active={activeTab === "notification"}
                  onClick={() => setActiveTab("notification")}
                >
                  알림
                  {unreadCount > 0 && (
                    <span className="ml-2 rounded-full bg-red-500 px-2 py-0.5 text-xs text-white">
                      {unreadCount}
                    </span>
                  )}
                </TabButton>
              </div>
            </div>

            {/* 콘텐츠 영역 */}
            <div className="flex-1 overflow-y-auto">
              {activeTab === "chat" ? <ChatTab /> : <NotificationTab />}
            </div>
          </div>
        </div>
      )}
    </>
  );
}

// 탭 버튼 컴포넌트
function TabButton({
  children,
  active,
  onClick,
}: {
  children: React.ReactNode;
  active: boolean;
  onClick: () => void;
}) {
  return (
    <button
      onClick={onClick}
      className={cn(
        "flex-1 rounded-md px-4 py-2",
        "text-sm font-medium",
        "transition-all",
        active ? "bg-white text-amber-600 shadow-sm" : "text-gray-600 hover:text-gray-800",
      )}
    >
      {children}
    </button>
  );
}
