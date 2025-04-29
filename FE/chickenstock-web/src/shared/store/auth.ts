import apiClient from "@/shared/api/axios";
import { AuthState, SimpleProfile } from "@/shared/store/types";
import { create } from "zustand";

const baseURL = import.meta.env.VITE_BASE_URL;

interface LoginResponse {
  accessToken: string;
}

export const useAuthStore = create<AuthState>()((set) => ({
  accessToken: null,
  setAccessToken: (token) => set({ accessToken: token }),
  clearAccessToken: () => set({ accessToken: null }),
  simpleProfile: null,
  getSimpleProfile: async () => {
    const response = await apiClient.get<SimpleProfile>(`${baseURL}/members/simple-profile`);
    console.log(response.data);
    set({ simpleProfile: response.data });
    return response.data;
  },
  setSimpleProfile: (profile) => set({ simpleProfile: profile }),
  login: async (email: string, password: string) => {
    try {
      const response = await apiClient.post<LoginResponse>(`${baseURL}/auth/login`, {
        email,
        password,
        platform: "web",
      });
      return response.data;
    } catch (err) {
      console.error(err);
      throw err;
    }
  },
  logout: async () => {
    try {
      await apiClient.post(`${baseURL}/auth/logout`, { withCredentials: true });
      set({ accessToken: null });
    } catch (err) {
      console.error(err);
    }
  },
}));
