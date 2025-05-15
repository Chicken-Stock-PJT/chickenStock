import PortfolioChart from "./ui/PortfolioChart";
import HoldingsList from "../dashboard/ui/HoldingsList";
import PortfolioOverview from "../dashboard/PortfolioOverview";
import { useMemberDashboardQuery } from "./model/queries";
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

  const { data, isLoading, error, refetch } = useMemberDashboardQuery();

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  const portfolio = data!;

  return (
    <div className="space-y-6 text-left">
      {/* <HoldingsList /> */}
      <div className="grid grid-cols-1 gap-6 md:grid-cols-2">
        <PortfolioChart
          memberMoney={portfolio.memberMoney}
          holdings={portfolio.holdings}
          stockValuation={portfolio.stockValuation}
        />
        <PortfolioOverview
          totalAsset={portfolio.totalAsset}
          memberMoney={portfolio.memberMoney}
          totalProfitLoss={portfolio.totalProfitLoss}
          totalReturnRate={portfolio.totalReturnRate}
          todayProfitLoss={portfolio.todayProfitLoss}
          todayReturnRate={portfolio.todayReturnRate}
          sectors={portfolio.holdingStockCount}
        />
      </div>
      <div>
        <HoldingsList
          holdings={portfolio.holdings}
          onClick={() => void refetch()}
          isLoading={isLoading}
        />
      </div>
    </div>
  );
};

export default Portfolio;
