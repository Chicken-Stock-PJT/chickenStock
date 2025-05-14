import {
  WebSocketMessage,
  AuthenticateMessage,
  ChatMessageRequest,
  PingMessage,
  ChatMessageResponse,
  ConnectedMessage,
  AuthenticatedMessage,
  UserJoinedMessage,
  UserLeftMessage,
  NotificationMessage,
  ErrorMessage,
  GetNotificationsMessage,
  MarkAsReadMessage,
  NotificationListMessage,
  NotificationReadMessage,
} from "../model/types";
import { useChatNotificationStore } from "../model/store";
import { useAuthStore } from "@/shared/store/auth";

// WebSocket 인스턴스와 관련 상태들
let ws: WebSocket | null = null;
let reconnectTimeout: NodeJS.Timeout | null = null;
let pingInterval: NodeJS.Timeout | null = null;
let reconnectAttempts = 0;
const maxReconnectAttempts = 5;

// 타입 가드 함수들
const isConnectedMessage = (msg: WebSocketMessage): msg is ConnectedMessage =>
  msg.type === "connected";
const isAuthenticatedMessage = (msg: WebSocketMessage): msg is AuthenticatedMessage =>
  msg.type === "authenticated";
const isUserJoinedMessage = (msg: WebSocketMessage): msg is UserJoinedMessage =>
  msg.type === "userJoined";
const isUserLeftMessage = (msg: WebSocketMessage): msg is UserLeftMessage =>
  msg.type === "userLeft";
const isChatMessage = (msg: WebSocketMessage): msg is ChatMessageResponse => msg.type === "chat";
const isNotificationMessage = (msg: WebSocketMessage): msg is NotificationMessage =>
  msg.type === "notification";
const isNotificationListMessage = (msg: WebSocketMessage): msg is NotificationListMessage =>
  msg.type === "notificationList";
const isNotificationReadMessage = (msg: WebSocketMessage): msg is NotificationReadMessage =>
  msg.type === "notificationRead";
const isErrorMessage = (msg: WebSocketMessage): msg is ErrorMessage => msg.type === "error";

// WebSocket 연결
export const connect = () => {
  if (ws?.readyState === WebSocket.OPEN) {
    return;
  }

  const wsUrl = `${import.meta.env.VITE_BASE_WS_URL}/ws/notification`;
  console.log("WebSocket 연결 시도:", wsUrl);

  ws = new WebSocket(wsUrl);

  ws.onopen = () => {
    console.log("WebSocket 연결 성공");
    useChatNotificationStore.getState().setConnected(true);
    reconnectAttempts = 0;

    // 30초마다 ping 전송
    pingInterval = setInterval(() => {
      sendMessage({ type: "ping" } as PingMessage);
    }, 30000);
  };

  ws.onmessage = (event: MessageEvent) => {
    try {
      const parsedData = JSON.parse(event.data as string) as WebSocketMessage;
      handleMessage(parsedData);
    } catch (error) {
      console.error(
        "메시지 파싱 오류:",
        error instanceof Error ? error.message : "알 수 없는 오류",
      );
    }
  };

  ws.onclose = () => {
    console.log("WebSocket 연결 종료");
    useChatNotificationStore.getState().setConnected(false);
    useChatNotificationStore.getState().setAuthenticated(false);

    // ping 인터벌 정리
    if (pingInterval) {
      clearInterval(pingInterval);
    }

    // 재연결 시도
    attemptReconnect();
  };

  ws.onerror = (event) => {
    console.error("WebSocket 오류:", event);
  };
};

// 메시지 처리
const handleMessage = (data: WebSocketMessage) => {
  const store = useChatNotificationStore.getState();

  if (isConnectedMessage(data)) {
    console.log("서버 연결 확인:", data.message);
    // 자동 인증
    const token = useAuthStore.getState().accessToken;
    if (token) {
      authenticate(token);
    }
  } else if (isAuthenticatedMessage(data)) {
    console.log("인증 성공:", data.nickname);
    store.setAuthenticated(true);
    store.setCurrentUser({
      nickname: data.nickname,
      memberId: data.memberId,
    });
  } else if (isUserJoinedMessage(data)) {
    store.addUser({
      nickname: data.nickname,
      timestamp: data.timestamp,
    });
  } else if (isUserLeftMessage(data)) {
    store.removeUser(data.nickname);
  } else if (isChatMessage(data)) {
    store.addMessage(data);
  } else if (isNotificationMessage(data)) {
    store.addNotification(data);
  } else if (isNotificationListMessage(data)) {
    store.setNotifications(data.notifications);
  } else if (isNotificationReadMessage(data)) {
    if (data.success) {
      store.markNotificationAsRead(data.notificationId);
    }
  } else if (isErrorMessage(data)) {
    console.error("서버 오류:", data.message);
  } else if (data.type === "pong") {
    // pong 응답 받음 (연결 정상)
  } else {
    console.log("알 수 없는 메시지 타입:", data);
  }
};

// 인증
const authenticate = (token: string) => {
  const message: AuthenticateMessage = {
    type: "authenticate",
    token: `Bearer ${token}`,
  };
  sendMessage(message);
};

// 메시지 전송
const sendMessage = (data: WebSocketMessage) => {
  if (ws?.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify(data));
  } else {
    console.error("WebSocket이 연결되지 않았습니다");
  }
};

// 채팅 메시지 전송
export const sendChatMessage = (message: string) => {
  const chatMessage: ChatMessageRequest = {
    type: "chat",
    message,
  };
  sendMessage(chatMessage);
};

// 알림 목록 조회
export const getNotifications = (filter: "all" | "unread" = "unread") => {
  const message: GetNotificationsMessage = {
    type: "getNotifications",
    filter,
  };
  sendMessage(message);
};

// 알림 읽음 처리
export const markAsRead = (notificationId: number) => {
  const message: MarkAsReadMessage = {
    type: "markAsRead",
    notificationId,
  };
  sendMessage(message);
};

// 재연결 시도
const attemptReconnect = () => {
  if (reconnectAttempts >= maxReconnectAttempts) {
    console.error("재연결 최대 시도 횟수 초과");
    return;
  }

  reconnectAttempts++;
  const delay = Math.min(10000, 1000 * Math.pow(2, reconnectAttempts));

  console.log(`${delay}ms 후 재연결 시도 (${reconnectAttempts}/${maxReconnectAttempts})`);

  reconnectTimeout = setTimeout(() => {
    connect();
  }, delay);
};

// 연결 종료
export const disconnect = () => {
  if (ws) {
    ws.close();
  }
  if (reconnectTimeout) {
    clearTimeout(reconnectTimeout);
  }
  if (pingInterval) {
    clearInterval(pingInterval);
  }
};

// WebSocket 관리 객체
export const webSocketManager = {
  connect,
  disconnect,
  sendChatMessage,
  getNotifications,
  markAsRead,
};
