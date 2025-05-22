import { create } from "zustand";
import { devtools, persist } from "zustand/middleware";

interface WatchlistState {
  watchlist: string[];
  setWatchlist: (watchlist: string[]) => void;
  addToWatchlist: (stockCode: string) => void;
  removeFromWatchlist: (stockCode: string) => void;
  isInWatchlist: (stockCode: string) => boolean;
}

const useWatchlistStore = create<WatchlistState>()(
  devtools(
    persist(
      (set, get) => ({
        watchlist: [],
        setWatchlist: (watchlist: string[]) => set({ watchlist }),
        addToWatchlist: (stockCode: string) =>
          set((state) => {
            // 이미 존재하는지 확인
            if (state.watchlist.includes(stockCode)) {
              return state;
            }
            return { watchlist: [...state.watchlist, stockCode] };
          }),
        removeFromWatchlist: (stockCode: string) =>
          set((state) => ({
            watchlist: state.watchlist.filter((code) => code !== stockCode),
          })),
        isInWatchlist: (stockCode: string) =>
          get().watchlist.includes(stockCode) || get().watchlist.includes(stockCode + "_AL"),
      }),
      {
        name: "watchlist",
      },
    ),
  ),
);

export default useWatchlistStore;
