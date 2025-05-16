import { useEffect } from "react";
import { webSocketManager } from "../api/webSocket";
import { useAuthStore } from "@/shared/store/auth";
import { useChatNotificationStore } from "../model/store";

export const useWebSocket = () => {
  const accessToken = useAuthStore((state) => state.accessToken);

  useEffect(() => {
    if (accessToken) {
      webSocketManager.connect();

      // 연결 후 읽지 않은 알림 요청
      const timer = setTimeout(() => {
        if (useChatNotificationStore.getState().connected) {
          webSocketManager.getNotifications("unread");
        }
      }, 1000);

      return () => {
        clearTimeout(timer);
        webSocketManager.disconnect();
      };
    }
  }, [accessToken]);

  return {
    sendChatMessage: webSocketManager.sendChatMessage,
  };
};
