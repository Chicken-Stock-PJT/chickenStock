import PortfolioChart from "./ui/PortfolioChart";
import HoldingsList from "./ui/HoldingsList";
import PortfolioOverview from "./ui/PortfolioOverview";

const Portfolio = () => {
  return (
    <div className="space-y-6 text-left">
      {/* <HoldingsList /> */}
      <div className="grid grid-cols-1 gap-6 md:grid-cols-2">
        <PortfolioChart />
        <PortfolioOverview />
      </div>
      <div>
        <HoldingsList />
      </div>
    </div>
  );
};

export default Portfolio;
