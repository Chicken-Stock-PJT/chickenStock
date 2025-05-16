import PortfolioChart from "@/features/mypage/ui/PortfolioChart";
import AiInfo from "./AiInfo";
import { MemberDashboardResponse } from "@/features/dashboard/model/types";

const AiBotDashboard = ({
  portfolio,
  aiBotId,
}: {
  portfolio: MemberDashboardResponse;
  aiBotId: number;
}) => {
  return (
    <div className="grid grid-cols-1 gap-4 text-left md:grid-cols-2">
      {/* 기본정보 -> 추후 ai봇 정보로 수정 */}
      <AiInfo
        aiBotId={aiBotId}
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
        memberMoney={portfolio.memberMoney}
      />
    </div>
  );
};

export default AiBotDashboard;
