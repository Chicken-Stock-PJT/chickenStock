import PortfolioChart from "./ui/PortfolioChart";
import HoldingsList from "./ui/HoldingsList";
import PortfolioOverview from "./ui/PortfolioOverview";
import { useGetPortfolio } from "./model/queries";
import { PortfolioResponse } from "./model/types";

const Portfolio = () => {
  const { data, isLoading, error } = useGetPortfolio();

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  const portfolio = data as PortfolioResponse;
  const sectors = portfolio.positions.length;

  return (
    <div className="space-y-6 text-left">
      {/* <HoldingsList /> */}
      <div className="grid grid-cols-1 gap-6 md:grid-cols-2">
        <PortfolioChart
          memberMoney={portfolio.memberMoney}
          positions={portfolio.positions}
          totalValuation={portfolio.totalValuation}
        />
        <PortfolioOverview
          totalAsset={portfolio.totalAsset}
          totalProfitLoss={portfolio.totalProfitLoss}
          totalReturnRate={portfolio.totalReturnRate}
          sectors={sectors}
        />
      </div>
      <div>
        <HoldingsList holdings={portfolio.positions} />
      </div>
    </div>
  );
};

export default Portfolio;
