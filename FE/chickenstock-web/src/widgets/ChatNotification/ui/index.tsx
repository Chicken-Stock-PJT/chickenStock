import { useWebSocket } from "../hooks/useWebSocket";
import { useChatNotificationStore } from "../model/store";
import { cn } from "@/shared/libs/utils";
import ChatTab from "./ChatTab";
import NotificationTab from "./NotificationTab";
import FloatingButton from "./FloatingButton";

export default function ChatNotification() {
  const { isOpen, setOpen, notifications, activeTab, setActiveTab } = useChatNotificationStore();
  useWebSocket();

  const unreadCount = notifications.length;

  return (
    <>
      {/* 플로팅 버튼 */}
      <FloatingButton onClick={() => setOpen(!isOpen)} unreadCount={unreadCount} />

      {/* 채팅/알림 패널 */}
      {isOpen && (
        <div className="fixed bottom-20 right-5 z-50 h-[600px] w-96 rounded-lg bg-white shadow-2xl">
          <div className="flex h-full flex-col">
            {/* 헤더 */}
            <div className="flex items-center justify-between rounded-t-lg border-b bg-amber-50 p-4">
              <h3 className="font-semibold text-gray-800">채팅 & 알림</h3>
              <button
                onClick={() => setOpen(false)}
                className="text-gray-500 transition-colors hover:text-gray-700"
              >
                ✕
              </button>
            </div>

            {/* 탭 버튼 */}
            <div className="flex border-b bg-gray-50">
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

            {/* 콘텐츠 영역 */}
            <div className="flex-1 overflow-hidden bg-gray-50">
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
        "flex-1 px-4 py-3",
        "text-sm font-medium",
        "transition-colors",
        active
          ? "border-b-2 border-amber-500 text-amber-600"
          : "text-gray-600 hover:bg-gray-100 hover:text-gray-800",
      )}
    >
      {children}
    </button>
  );
}
