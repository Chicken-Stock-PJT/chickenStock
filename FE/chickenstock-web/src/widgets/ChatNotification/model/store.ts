import { create } from "zustand";
import { ChatNotificationState } from "./types";
import { webSocketManager } from "../api/webSocket";
// 초기 상태
const initialState = {
  isOpen: false,
  activeTab: "chat" as const,
  connected: false,
  authenticated: false,
  messages: [],
  notifications: [],
  users: [],
  currentUser: null,
};

// Zustand 스토어 생성
export const useChatNotificationStore = create<ChatNotificationState>((set, get) => ({
  ...initialState,
  authenticatedCount: 0,
  totalCount: 0,
  lastCountUpdate: "",
  get unreadCount() {
    return get().notifications.filter((n) => !n.isRead).length;
  },
  // 모든 알림을 읽음 처리
  markAllNotificationsAsRead: () => {
    // 웹소켓으로 요청 전송
    if (webSocketManager) {
      webSocketManager.markAllAsRead();
    }
  },
  // 여러 알림 읽음 처리 (응답 처리용)
  markMultipleNotificationsAsRead: (notificationIds: number[]) =>
    set((state) => ({
      notifications: state.notifications.map((notif) =>
        notificationIds.includes(notif.notificationId) ? { ...notif, isRead: true } : notif,
      ),
    })),
  // UI 관련 액션
  setOpen: (isOpen) => set({ isOpen }),
  setActiveTab: (activeTab) => set({ activeTab }),

  // 연결 상태 관련 액션
  setConnected: (connected) => set({ connected }),
  setAuthenticated: (authenticated) => set({ authenticated }),

  // 데이터 관련 액션
  addMessage: (message) =>
    set((state) => {
      // 이미 동일한 메시지가 있는지 확인
      const isDuplicate = state.messages.some(
        (m) =>
          m.timestamp === message.timestamp &&
          m.message === message.message &&
          m.memberId === message.memberId,
      );

      if (isDuplicate) {
        console.log("중복 메시지 필터링:", message);
        return state; // 중복이면 상태 변경 없음
      }

      return {
        messages: [...state.messages, message],
      };
    }),

  addNotification: (notification) =>
    set((state) => ({
      notifications: [notification, ...state.notifications],
    })),

  setNotifications: (notifications) => set({ notifications }),

  markNotificationAsRead: (notificationId) =>
    set((state) => ({
      notifications: state.notifications.map((notif) =>
        notif.notificationId === notificationId ? { ...notif, isRead: true } : notif,
      ),
    })),

  addUser: (user) =>
    set((state) => ({
      users: [...state.users, user],
    })),

  removeUser: (nickname) =>
    set((state) => ({
      users: state.users.filter((u) => u.nickname !== nickname),
    })),

  setCurrentUser: (user) => set({ currentUser: user }),
  setUserCount: (authenticatedCount, totalCount, timestamp) =>
    set({
      authenticatedCount,
      totalCount,
      // 숫자 타임스탬프라면 변환해서 저장
      lastCountUpdate:
        typeof timestamp === "number" ? new Date(timestamp).toLocaleString() : timestamp,
    }),

  getUserCount: () => {
    // 웹소켓으로 요청 전송
    if (webSocketManager) {
      webSocketManager.getUserCount();
    }
  },
  // 초기화
  reset: () => set(initialState),
}));
