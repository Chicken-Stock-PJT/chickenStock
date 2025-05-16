import useWatchlistStore from "@/features/watchlist/model/store";
import apiClient from "@/shared/api/axios";
import { AuthState } from "@/shared/store/types";
import { create } from "zustand";
import { devtools, persist } from "zustand/middleware";
import { queryClient } from "../api/queryClient";

const baseURL = import.meta.env.VITE_BASE_URL;

interface LoginResponse {
  accessToken: string;
}

export const useAuthStore = create<AuthState>()(
  devtools(
    persist(
      (set, get) => ({
        accessToken: null,
        isLoggedIn: false,
        setAccessToken: (token: string) => {
          set({
            accessToken: token,
            isLoggedIn: !!token,
          });
        },
        clearAccessToken: () =>
          set({
            accessToken: null,
            isLoggedIn: false,
          }),
        login: async (email: string, password: string) => {
          const response = await apiClient.post<LoginResponse>(`${baseURL}/auth/login`, {
            email,
            password,
            platform: "web",
          });
          set({
            accessToken: response.data.accessToken,
            isLoggedIn: true,
          });
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
          console.log("socialLogin", response.data);
          set({
            accessToken: response.data.accessToken,
            isLoggedIn: true,
          });
          console.log("socialLogin", get().accessToken);
          return response.data;
        },
        logout: async () => {
          useWatchlistStore.getState().setWatchlist([]);
          try {
            const res = await apiClient.post(
              `${baseURL}/auth/logout`,
              {},
              { withCredentials: true },
            );
            console.log("logout", res);
          } catch (error) {
            console.log("logout 실패", error);
          } finally {
            set({
              accessToken: null,
              isLoggedIn: false,
            });
            void queryClient.removeQueries({ queryKey: ["simpleProfile"] });
          }
        },
      }),
      {
        name: "auth-store",
      },
    ),
  ),
);
