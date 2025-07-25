import PortfolioChart from "./ui/PortfolioChart";
import HoldingsList from "../dashboard/ui/HoldingsList";
import PortfolioOverview from "../dashboard/PortfolioOverview";
import { useMemberDashboardQuery } from "./model/queries";
import { useEffect } from "react";
import { queryClient } from "@/shared/api/queryClient";
import { SimpleProfile } from "@/features/mypage/model/types";

const Portfolio = () => {
  const { data, isLoading, error, refetch } = useMemberDashboardQuery();

  useEffect(() => {
    if (data) {
      // simpleProfile 쿼리 데이터 직접 업데이트
      queryClient.setQueryData<SimpleProfile>(["simpleProfile"], (oldData) => {
        if (!oldData) return oldData;
        return {
          ...oldData,
          totalAsset: data.totalAsset.toString(),
          memberMoney: data.memberMoney.toString(),
          pendingOrderAmount: data.pendingOrderAmount.toString(),
          stockValuation: data.stockValuation.toString(),
          returnRate: data.totalReturnRate.toString(),
        };
      });
    }
  }, [data]);

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;
  if (!data) return <div>No data available</div>;

  return (
    <div className="space-y-6 text-left">
      <div className="grid grid-cols-1 gap-6 md:grid-cols-2">
        <PortfolioChart
          cash={data.memberMoney}
          holdings={data.holdings}
          stockValuation={data.stockValuation}
        />
        <PortfolioOverview
          totalAsset={data.totalAsset + data.pendingOrderAmount}
          totalProfitLoss={data.totalProfitLoss}
          totalReturnRate={data.totalReturnRate}
          memberMoney={data.memberMoney}
          pendingOrderAmount={data.pendingOrderAmount}
          stockValuation={data.stockValuation}
          todayTradeAmount={data.todayTradeAmount}
          todayProfitLoss={data.todayProfitLoss}
          todayReturnRate={data.todayReturnRate}
        />
      </div>
      <div>
        <HoldingsList
          holdings={data.holdings}
          onClick={() => void refetch()}
          isLoading={isLoading}
        />
      </div>
    </div>
  );
};

export default Portfolio;
