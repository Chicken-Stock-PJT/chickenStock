import { useEffect } from "react";
import { webSocketManager } from "../api/webSocket";
import { useAuthStore } from "@/shared/store/auth";

export const useWebSocket = () => {
  const accessToken = useAuthStore((state) => state.accessToken);

  useEffect(() => {
    // 토큰이 있을 때만 연결
    if (accessToken) {
      webSocketManager.connect();
    }

    return () => {
      // 컴포넌트 언마운트 시 정리
      webSocketManager.disconnect();
    };
  }, [accessToken]);

  return {
    sendChatMessage: webSocketManager.sendChatMessage,
  };
};
