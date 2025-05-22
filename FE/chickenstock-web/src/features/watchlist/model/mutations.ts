import { useMutation, useQueryClient } from "@tanstack/react-query";
import { addWatchlist, deleteWatchlist } from "../api";
import useWatchlistStore from "./store";
import { GetWatchlistSuccessResponse, Watchlist } from "./types";

export const useAddWatchlist = () => {
  const queryClient = useQueryClient();
  const { addToWatchlist: addToStore } = useWatchlistStore();
  return useMutation({
    mutationFn: addWatchlist,
    onMutate: async (stockCode) => {
      await queryClient.cancelQueries({ queryKey: ["watchlist"] });
      const previousData = queryClient.getQueryData<GetWatchlistSuccessResponse>(["watchlist"]);
      addToStore(stockCode);

      if (previousData) {
        const dummyItem: Watchlist = {
          stockCode,
          stockName: "로딩 중...",
          currentPrice: 0,
          priceChange: "0",
          changeRate: "0%",
          tradingVolume: "0",
          timestamp: new Date().toISOString(),
        };

        queryClient.setQueryData<GetWatchlistSuccessResponse>(["watchlist"], {
          message: previousData.message,
          watchList: [...previousData.watchList, dummyItem],
          updatedAt: new Date().toISOString(),
        });
      }
      return { previousData };
    },
    onError: (err, stockCode, context) => {
      console.log(err);
      if (context?.previousData) {
        queryClient.setQueryData(["watchlist"], context.previousData);
      }
      const { removeFromWatchlist } = useWatchlistStore.getState();
      removeFromWatchlist(stockCode);
    },
    onSettled: () => {
      void queryClient.invalidateQueries({ queryKey: ["watchlist"] });

      console.log(useWatchlistStore.getState().watchlist);
    },
  });
};

export const useDeleteWatchlist = () => {
  const queryClient = useQueryClient();
  const { removeFromWatchlist: removeFromStore } = useWatchlistStore();

  return useMutation({
    mutationFn: deleteWatchlist,
    onMutate: async (stockCode) => {
      await queryClient.cancelQueries({ queryKey: ["watchlist"] });
      const previousData = queryClient.getQueryData<GetWatchlistSuccessResponse>(["watchlist"]);
      removeFromStore(stockCode);
      if (previousData) {
        queryClient.setQueryData<GetWatchlistSuccessResponse>(["watchlist"], {
          message: previousData.message,
          watchList: previousData.watchList.filter((item) => item.stockCode !== stockCode),
          updatedAt: new Date().toISOString(),
        });
      }
      return { previousData };
    },
    onError: (err, stockCode, context) => {
      console.log(err);
      if (context?.previousData) {
        queryClient.setQueryData(["watchlist"], context.previousData);
      }
      const { addToWatchlist } = useWatchlistStore.getState();
      addToWatchlist(stockCode);
    },
    onSuccess: async () => {
      await queryClient.invalidateQueries({ queryKey: ["watchlist"] });
      console.log(useWatchlistStore.getState().watchlist);
    },
  });
};
