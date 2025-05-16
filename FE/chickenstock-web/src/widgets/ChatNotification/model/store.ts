import { create } from "zustand";
import { ChatNotificationState } from "./types";

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

  get unreadCount() {
    return get().notifications.filter((n) => !n.isRead).length;
  },
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
      notifications: [...state.notifications, notification],
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

  // 초기화
  reset: () => set(initialState),
}));
