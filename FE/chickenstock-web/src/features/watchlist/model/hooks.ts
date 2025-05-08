import { useAddWatchlist } from "./mutations";
import { useDeleteWatchlist } from "./mutations";
import useWatchlistStore from "./store";

export const useWatchlistToggle = () => {
  const { isInWatchlist } = useWatchlistStore();
  const addMutation = useAddWatchlist();
  const removeMutation = useDeleteWatchlist();

  const toggleWatchlist = (stockCode: string) => {
    if (isInWatchlist(stockCode)) {
      removeMutation.mutate(stockCode);
    } else {
      addMutation.mutate(stockCode);
    }
  };

  return {
    toggleWatchlist,
    isAdding: addMutation.isPending,
    isRemoving: removeMutation.isPending,
  };
};
