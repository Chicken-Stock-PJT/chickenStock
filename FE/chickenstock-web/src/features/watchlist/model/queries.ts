import { useQuery } from "@tanstack/react-query";
import { getWatchlist } from "../api";
import useWatchlistStore from "./store";
import { useEffect } from "react";

export const useGetWatchlist = () => {
  const setWatchlist = useWatchlistStore((state) => state.setWatchlist);
  const query = useQuery({
    queryKey: ["watchlist"],
    queryFn: getWatchlist,
  });

  useEffect(() => {
    console.log("쿼리 데이터 변경:", query.data);
    if (query.data && "watchList" in query.data) {
      const data = query.data;
      console.log("스토어 업데이트 전:", useWatchlistStore.getState().watchlist);
      setWatchlist(data.watchList.map((watchlist) => watchlist.stockCode));
      console.log("스토어 업데이트 후:", useWatchlistStore.getState().watchlist);
    }
  }, [query.data, setWatchlist]);

  return query;
};
