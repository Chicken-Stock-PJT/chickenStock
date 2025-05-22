// WebSocket 메시지 타입
export type MessageType =
  | "connected"
  | "authenticate"
  | "authenticated"
  | "userJoined"
  | "userLeft"
  | "chat"
  | "notification"
  | "ping"
  | "pong"
  | "error"
  | "getNotifications"
  | "notificationList"
  | "markAsRead"
  | "notificationRead"
  | "getUserCount"
  | "userCount"
  | "markAllAsRead"
  | "allNotificationsRead";

// 베이스 메시지
export interface BaseMessage {
  type: MessageType;
  timestamp?: number;
}

// 연결 메시지
export interface ConnectedMessage extends BaseMessage {
  type: "connected";
  message: string;
}

// 인증 요청
export interface AuthenticateMessage extends BaseMessage {
  type: "authenticate";
  token: string;
}

// 인증 응답
export interface AuthenticatedMessage extends BaseMessage {
  type: "authenticated";
  nickname: string;
  memberId: number;
}

// 채팅 메시지 (전송용)
export interface ChatMessageRequest extends BaseMessage {
  type: "chat";
  message: string;
}

// 채팅 메시지 (수신용)
export interface ChatMessageResponse extends BaseMessage {
  type: "chat";
  message: string;
  memberId: number;
  nickname: string;
}

// 채팅 메시지 (스토어용 - Response 타입 사용)
export type ChatMessage = ChatMessageResponse;

// 알림 메시지
export interface NotificationMessage extends BaseMessage {
  type: "notification";
  notificationType: "TRADE" | "COMMENT" | "LIKE";
  title: string;
  message: string;
  notificationId: number;
  timestamp: number;
  isRead: boolean;
  relatedId?: number;
}

// 알림 목록 요청
export interface GetNotificationsMessage extends BaseMessage {
  type: "getNotifications";
  filter: "all" | "unread";
}

// 알림 목록 응답
export interface NotificationListMessage extends BaseMessage {
  type: "notificationList";
  notifications: NotificationMessage[];
}

// 알림 읽음 처리 요청
export interface MarkAsReadMessage extends BaseMessage {
  type: "markAsRead";
  notificationId: number;
}

// 알림 읽음 처리 응답
export interface NotificationReadMessage extends BaseMessage {
  type: "notificationRead";
  notificationId: number;
  success: boolean;
}

// 사용자 입장/퇴장
export interface UserJoinedMessage extends BaseMessage {
  type: "userJoined";
  nickname: string;
  timestamp: number;
}

export interface UserLeftMessage extends BaseMessage {
  type: "userLeft";
  nickname: string;
  timestamp: number;
}

// 에러 메시지
export interface ErrorMessage extends BaseMessage {
  type: "error";
  message: string;
}

// Ping/Pong
export interface PingMessage extends BaseMessage {
  type: "ping";
}

export interface PongMessage extends BaseMessage {
  type: "pong";
  timestamp: number;
}

// 사용자 수 요청
export interface GetUserCountMessage extends BaseMessage {
  type: "getUserCount";
}

// 사용자 수 응답/업데이트
export interface UserCountMessage extends Omit<BaseMessage, "timestamp"> {
  type: "userCount";
  authenticatedCount: number;
  totalCount: number;
  timestamp: string;
}

// 모든 알림 읽음 처리 요청
export interface MarkAllAsReadMessage extends BaseMessage {
  type: "markAllAsRead";
}

// 모든 알림 읽음 처리 응답
export interface AllNotificationsReadMessage extends BaseMessage {
  type: "allNotificationsRead";
  notificationIds: number[];
  success: boolean;
}
// 유니온 타입
export type WebSocketMessage =
  | ConnectedMessage
  | AuthenticateMessage
  | AuthenticatedMessage
  | ChatMessageRequest
  | ChatMessageResponse
  | NotificationMessage
  | UserJoinedMessage
  | UserLeftMessage
  | ErrorMessage
  | PingMessage
  | PongMessage
  | GetNotificationsMessage
  | NotificationListMessage
  | MarkAsReadMessage
  | NotificationReadMessage
  | GetUserCountMessage
  | UserCountMessage
  | MarkAllAsReadMessage
  | AllNotificationsReadMessage;

// 스토어 상태
export interface ChatNotificationState {
  isOpen: boolean;
  activeTab: "chat" | "notification";
  connected: boolean;
  authenticated: boolean;
  messages: ChatMessage[];
  notifications: NotificationMessage[];
  users: { nickname: string; timestamp: number }[];
  currentUser: {
    nickname: string;
    memberId: number;
  } | null;
  authenticatedCount: number;
  totalCount: number;
  lastCountUpdate: string;

  // Actions
  setOpen: (isOpen: boolean) => void;
  setActiveTab: (tab: "chat" | "notification") => void;
  setConnected: (connected: boolean) => void;
  setAuthenticated: (authenticated: boolean) => void;
  addMessage: (message: ChatMessage) => void;
  addNotification: (notification: NotificationMessage) => void;
  setNotifications: (notifications: NotificationMessage[]) => void;
  markNotificationAsRead: (notificationId: number) => void;
  addUser: (user: { nickname: string; timestamp: number }) => void;
  removeUser: (nickname: string) => void;
  setCurrentUser: (user: { nickname: string; memberId: number } | null) => void;
  reset: () => void;
  setUserCount: (authenticatedCount: number, totalCount: number, timestamp: string) => void;
  getUserCount: () => void;
  markAllNotificationsAsRead: () => void;
  markMultipleNotificationsAsRead: (notificationIds: number[]) => void; // 추가
}
