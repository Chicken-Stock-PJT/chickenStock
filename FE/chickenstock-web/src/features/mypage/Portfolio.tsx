import PortfolioChart from "./ui/PortfolioChart";
import HoldingsList from "./ui/HoldingsList";
import PortfolioOverview from "./ui/PortfolioOverview";
import { useGetPortfolio } from "./model/queries";
import {
  PortfolioResponse,
  // PortfolioSocketResponse
} from "./model/types";
// import { useEffect } from "react";
// import { useAuthStore } from "@/shared/store/auth";

const Portfolio = () => {
  // useEffect(() => {
  //   const token = useAuthStore.getState().accessToken;

  //   const portfolioSocket = new WebSocket(`${import.meta.env.VITE_WS_BASE_URL}/portfolio`);

  //   portfolioSocket.send(
  //     JSON.stringify({
  //       action: "authenticate",
  //       token: `Bearer ${token}`,
  //     }),
  //   );

  //   portfolioSocket.onopen = () => {
  //     console.log("Portfolio socket opened");
  //   };

  //   portfolioSocket.onmessage = (event) => {
  //     try {
  //       const data = JSON.parse(event.data as string) as PortfolioSocketResponse;
  //       console.log(data);

  //       switch (data.type) {
  //         case "fullPortfolioUpdate": {
  //           console.log("data", data);
  //           break;
  //         }
  //         case "stockUpdate": {
  //           console.log("data", data);
  //           break;
  //         }
  //       }
  //     } catch (error) {
  //       console.error("WebSocket 메시지 파싱 에러:", error);
  //     }
  //   };
  //   portfolioSocket.onclose = () => {
  //     console.log("Portfolio socket closed");
  //   };
  //   portfolioSocket.onerror = (event) => {
  //     console.log("Portfolio socket error", event);
  //   };
  //   return () => {
  //     portfolioSocket.close();
  //   };
  // }, []);

  const { data, isLoading, error, refetch } = useGetPortfolio();

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
          memberMoney={portfolio.memberMoney}
          totalProfitLoss={portfolio.totalProfitLoss}
          totalReturnRate={portfolio.totalReturnRate}
          sectors={sectors}
        />
      </div>
      <div>
        <HoldingsList
          holdings={portfolio.positions}
          onClick={() => void refetch()}
          isLoading={isLoading}
        />
      </div>
    </div>
  );
};

export default Portfolio;
