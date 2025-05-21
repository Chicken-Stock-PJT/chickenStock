import PortfolioChart from "@/features/mypage/ui/PortfolioChart";
import { MemberDashboardResponse } from "@/features/dashboard/model/types";
import MemberInfo from "@/features/dashboard/ui/MemberInfo";

const MemberDashboard = ({
  name,
  portfolio,
}: {
  name: string;
  portfolio: MemberDashboardResponse;
}) => {
  return (
    <div className="grid grid-cols-1 gap-4 text-left md:grid-cols-2">
      {/* 기본정보 -> 추후 ai봇 정보로 수정 */}
      <MemberInfo
        name={name}
        totalAsset={portfolio.totalAsset}
        totalReturnRate={portfolio.totalReturnRate}
        totalProfitLoss={portfolio.totalProfitLoss}
        memberMoney={portfolio.memberMoney}
        todayReturnRate={portfolio.todayReturnRate}
        todayProfitLoss={portfolio.todayProfitLoss}
      />

      {/* 포트폴리오 구성 */}
      <PortfolioChart
        holdings={portfolio.holdings}
        stockValuation={portfolio.stockValuation}
        cash={portfolio.cash}
      />
    </div>
  );
};

export default MemberDashboard;
