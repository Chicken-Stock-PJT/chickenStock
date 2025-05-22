import { Bell, Package, MessageSquare, Heart, Check } from "lucide-react";
import { useChatNotificationStore } from "../model/store";
import { webSocketManager } from "../api/webSocket";
import { cn } from "@/shared/libs/utils";
import { useEffect, useState } from "react";
import { NotificationMessage } from "@/widgets/ChatNotification/model/types";

export default function NotificationTab() {
  const { notifications, markAllNotificationsAsRead } = useChatNotificationStore();
  const [nofificationState, setNotificationState] = useState<NotificationMessage[]>([]);
  const unreadCount = notifications.filter((n) => !n.isRead).length;

  // 탭이 열릴 때 알림 목록 요청
  useEffect(() => {
    webSocketManager.getNotifications("all");
  }, []);

  useEffect(() => {
    setNotificationState(notifications);
  }, [notifications]);

  // 알림 클릭 시 읽음 처리
  const handleNotificationClick = (notificationId: number, isRead: boolean) => {
    if (!isRead) {
      webSocketManager.markAsRead(notificationId);
      console.log("알림 읽음 처리 요청:", notificationId);
    }
  };

  // 알림 타입별 아이콘 매핑
  const getNotificationIcon = (type: string) => {
    switch (type) {
      case "TRADE":
        return <Package size={20} className="text-green-600" />;
      case "COMMENT":
        return <MessageSquare size={20} className="text-blue-600" />;
      case "LIKE":
        return <Heart size={20} className="text-red-600" />;
      default:
        return <Bell size={20} className="text-gray-600" />;
    }
  };

  return (
    <div className="h-full overflow-y-auto">
      <div className="sticky top-0 z-10 bg-white p-3 shadow-sm ">
        <div className="flex items-center justify-between">
          <span className="text-sm font-medium">
            {unreadCount > 0 && `읽지 않은 알림 ${unreadCount}개`}
          </span>
          {unreadCount > 0 && (
            <button
              onClick={markAllNotificationsAsRead}
              className="flex items-center gap-1 rounded-md bg-gray-100 px-2 py-1 text-xs font-medium text-gray-700 hover:bg-gray-200"
            >
              <Check size={14} />
              모두 읽음
            </button>
          )}
        </div>
      </div>
      {nofificationState.length === 0 ? (
        <div className="my-auto flex h-full items-center justify-center text-center text-gray-500">
          새로운 알림이 없습니다.
        </div>
      ) : (
        <div className="space-y-3">
          {nofificationState.map((notif) => (
            <div
              key={notif.notificationId}
              onClick={() => handleNotificationClick(notif.notificationId, notif.isRead)}
              className={cn(
                "rounded-lg border p-4",
                "bg-white hover:bg-gray-50",
                "transition-colors cursor-pointer",
                notif.isRead ? "opacity-70" : "",
              )}
            >
              <div className="flex gap-3">
                <div className="shrink-0">{getNotificationIcon(notif.notificationType)}</div>
                <div className="flex-1">
                  <div className="font-medium text-gray-900">{notif.title}</div>
                  <div className="mt-1 text-sm text-gray-600">{notif.message}</div>
                  <div className="mt-2 text-xs text-gray-500">
                    {new Date(notif.timestamp || Date.now()).toLocaleString()}
                  </div>
                </div>
                {!notif.isRead && <div className="size-2 rounded-full bg-blue-500"></div>}
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
