import PortfolioChart from "@/features/mypage/ui/PortfolioChart";
import { Card, CardContent, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import AiBotAssetOverview from "./AiBotAssetOverview";
import { MemberDashboardResponse } from "@/features/dashboard/model/types";
import AiInfo from "./AiInfo";

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
        memberMoney={portfolio.memberMoney}
      />

      {/* 요약 */}
      <AiBotAssetOverview portfolio={portfolio} />

      {/* 성과추이 */}
      <Card>
        <CardHeader>
          <CardTitle>성과추이</CardTitle>
        </CardHeader>
        <CardContent>
          <p>성과추이</p>
        </CardContent>
      </Card>

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
