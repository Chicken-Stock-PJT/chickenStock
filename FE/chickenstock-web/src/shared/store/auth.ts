import apiClient from "@/shared/api/axios";
import { create } from "zustand";

interface AuthState {
  accessToken: string | null;
  setAccessToken: (token: string) => void;
  clearAccessToken: () => void;
  logout: () => Promise<void>;
}

const baseURL = import.meta.env.VITE_BASE_URL;

export const useAuthStore = create<AuthState>()((set) => ({
  accessToken: null,
  setAccessToken: (token) => set({ accessToken: token }),
  clearAccessToken: () => set({ accessToken: null }),
  logout: async () => {
    try {
      await apiClient.post(`${baseURL}/auth/logout`, { withCredentials: true });
      set({ accessToken: null });
    } catch (err) {
      console.error(err);
    }
  },
}));
