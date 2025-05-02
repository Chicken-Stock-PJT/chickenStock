import useWatchlistStore from "@/features/watchlist/model/store";
import apiClient from "@/shared/api/axios";
import { AuthState, SimpleProfile } from "@/shared/store/types";
import { create } from "zustand";
import { devtools, persist } from "zustand/middleware";

const baseURL = import.meta.env.VITE_BASE_URL;

interface LoginResponse {
  accessToken: string;
}

export const useAuthStore = create<AuthState>()(
  devtools(
    persist(
      (set) => ({
        accessToken: null,
        setAccessToken: (token) => set({ accessToken: token }),
        clearAccessToken: () => set({ accessToken: null }),

        simpleProfile: null,
        getSimpleProfile: async () => {
          const response = await apiClient.get<SimpleProfile>(`${baseURL}/members/simple-profile`);
          set({ simpleProfile: response.data });
          return response.data;
        },
        setSimpleProfile: (profile) =>
          set((state) => ({
            simpleProfile: state.simpleProfile
              ? { ...state.simpleProfile, ...profile }
              : (profile as SimpleProfile),
          })),

        isLoggedIn: false,
        login: async (email: string, password: string) => {
          const response = await apiClient.post<LoginResponse>(`${baseURL}/auth/login`, {
            email,
            password,
            platform: "web",
          });
          set({ accessToken: response.data.accessToken, isLoggedIn: true });
          return response.data;
        },
        socialLogin: async (code: string) => {
          const response = await apiClient.post<LoginResponse>(
            `${import.meta.env.VITE_BASE_URL}/auth/exchange`,
            {
              oneTimeCode: code,
              platform: "web",
            },
          );
          set({ accessToken: response.data.accessToken, isLoggedIn: true });
        },
        logout: async () => {
          useWatchlistStore.getState().setWatchlist([]);
          const res = await apiClient.post(`${baseURL}/auth/logout`, {}, { withCredentials: true });
          console.log("logout", res);
          set({ accessToken: null, isLoggedIn: false, simpleProfile: null });
          set({ simpleProfile: null });
        },
      }),
      {
        name: "auth-store", // DevTools에 표시될 이름
        // serialize: true,       // 필요 시 serialize 옵션 등 추가 가능
        // anonymousActionType: true,
      },
    ),
  ),
);
