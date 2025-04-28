import Banner from "@/features/mainpage/ui/Banner";
import StockList from "@/pages/StockList";

const HomePage = () => {
  return (
    <div className="absolute left-0 top-0 w-screen">
      <Banner />
      <div className="mx-auto mb-10 max-w-[1200px]">
        <StockList />
      </div>
    </div>
  );
};
export default HomePage;
