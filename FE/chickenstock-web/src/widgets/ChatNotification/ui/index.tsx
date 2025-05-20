import { useChatNotificationStore } from "../model/store";
import { cn } from "@/shared/libs/utils";
import ChatTab from "./ChatTab";
import NotificationTab from "./NotificationTab";
import FloatingButton from "./FloatingButton";

export default function ChatNotification() {
  const { isOpen, setOpen, notifications, activeTab, setActiveTab } = useChatNotificationStore();

  // 읽지 않은 알림 개수
  const unreadCount = notifications.filter((n) => !n.isRead).length;
  return (
    <>
      {/* 플로팅 버튼 */}
      <FloatingButton onClick={() => setOpen(!isOpen)} unreadCount={unreadCount} />

      {/* 채팅/알림 패널 - 크기 증가, 여백 추가 */}
      {isOpen && (
        <div className="fixed bottom-24 right-5 z-50 h-[650px] w-[450px] overflow-hidden rounded-xl bg-white shadow-2xl">
          <div className="flex h-full flex-col">
            {/* 탭 버튼 - 상단 여백 추가 */}
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
            <div className="flex-1 overflow-hidden">
              {activeTab === "chat" ? <ChatTab /> : <NotificationTab />}
            </div>
          </div>
        </div>
      )}
    </>
  );
}

// 탭 버튼 컴포넌트 - 더 부드러운 스타일
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
