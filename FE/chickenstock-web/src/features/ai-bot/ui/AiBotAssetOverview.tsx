import { Card, CardHeader, CardTitle, CardContent } from "@/shared/libs/ui/card";
import { MemberDashboardResponse } from "@/features/dashboard/model/types";

const AiBotAssetOverview = ({ portfolio }: { portfolio: MemberDashboardResponse }) => {
  return (
    <div className="flex flex-col gap-2">
      <Card>
        <CardHeader>
          <CardTitle>총 자산</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{portfolio.totalAsset.toLocaleString()}원</div>
        </CardContent>
      </Card>
      <Card>
        <CardHeader>
          <CardTitle>금일 실현 수익</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{portfolio.todayProfitLoss}원</div>
        </CardContent>
      </Card>
      <Card>
        <CardHeader>
          <CardTitle>예수금</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{portfolio.memberMoney.toLocaleString()}원</div>
        </CardContent>
      </Card>
      <Card>
        <CardHeader>
          <CardTitle>매수 가능 금액</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{portfolio.pendingOrderAmount}원</div>
        </CardContent>
      </Card>
    </div>
  );
};

export default AiBotAssetOverview;
