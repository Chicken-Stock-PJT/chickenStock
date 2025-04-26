import WatchList from "./ui/WatchList";
import WatchListGuide from "./ui/WatchListGuide";

const Watchlist = () => {
  return (
    <div className="space-y-6 text-left">
      <WatchList />
      <WatchListGuide />
    </div>
  );
};

export default Watchlist;
