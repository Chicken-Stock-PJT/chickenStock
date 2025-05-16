import { Card, CardContent, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { ArrowUpIcon, ArrowDownIcon } from "lucide-react";

interface PortfolioOverviewProps {
  totalAsset: number; // 총 자산 (현금 + 주식 평가금액)
  memberMoney: number; // 예수금 잔고
  totalProfitLoss: number; // 총 손익 (평가금액 - 투자금액)
  totalReturnRate: number; // 총 수익률 (%)
  todayProfitLoss: number; // 금일 손익
  todayReturnRate: number; // 금일 수익률
  sectors: number; // 보유 종목 수
}
const PortfolioOverview = ({
  totalAsset,
  memberMoney,
  totalProfitLoss,
  totalReturnRate,
  todayProfitLoss,
  todayReturnRate,
  sectors,
}: PortfolioOverviewProps) => {
  const sectorMention = () => {
    if (sectors === 0) {
      return "첫 투자를 시작해보세요!";
    } else if (sectors > 0 && sectors < 5) {
      return "핵심 종목에 집중하고 있어요.";
    } else if (sectors >= 5 && sectors < 10) {
      return "균형 잡힌 포트폴리오를 구성 중입니다";
    } else if (sectors >= 10 && sectors < 15) {
      return "다양한 종목에 분산 투자 중";
    } else if (sectors >= 15 && sectors < 20) {
      return "폭넓게 포트폴리오를 확장 중입니다";
    }
  };

  return (
    <div className="grid grid-cols-1 gap-6">
      <Card>
        <CardHeader className="pb-2">
          <CardTitle>총 자산</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{totalAsset.toLocaleString()}원</div>
          <div className="mt-2 flex items-center">
            <div
              className={`flex items-center ${totalProfitLoss > 0 ? "text-chart-red" : totalProfitLoss < 0 ? "text-chart-blue" : ""}`}
            >
              {totalProfitLoss >= 0 ? "+" : ""}
              {totalProfitLoss.toLocaleString()}원 ({totalProfitLoss >= 0 ? "+" : ""}
              {totalReturnRate.toFixed(2)}% )
            </div>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader className="pb-2">
          <CardTitle>예수금 잔고</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{memberMoney.toLocaleString()}원</div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader className="pb-2">
          <CardTitle>금일 수익</CardTitle>
        </CardHeader>
        <CardContent>
          {todayReturnRate ? (
            <>
              <div
                className={`text-2xl font-bold ${todayProfitLoss > 0 ? "text-chart-red" : todayProfitLoss < 0 ? "text-chart-blue" : ""}`}
              >
                {todayProfitLoss >= 0 ? "+" : ""}
                {todayProfitLoss.toLocaleString()}원
              </div>
              <div className="mt-2 flex items-center">
                <div
                  className={`flex items-center ${todayReturnRate > 0 ? "text-chart-red" : todayReturnRate < 0 ? "text-chart-blue" : ""}`}
                >
                  {todayReturnRate >= 0 ? (
                    <ArrowUpIcon className="mr-1 size-4" />
                  ) : (
                    <ArrowDownIcon className="mr-1 size-4" />
                  )}
                  <span>
                    {todayReturnRate >= 0 ? "+" : ""}
                    {todayReturnRate.toFixed(2)}%
                  </span>
                </div>
              </div>
            </>
          ) : (
            <>
              <div className={`text-2xl font-bold`}>0원</div>
              <div className="mt-2 flex items-center">
                <div className={`flex items-center`}>
                  <span>0.0%</span>
                </div>
              </div>
            </>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardHeader className="pb-2">
          <CardTitle>보유 종목 수</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{sectors}개</div>
          <div className="mt-2 text-sm text-muted-foreground">{sectorMention()}</div>
        </CardContent>
      </Card>
    </div>
  );
};

export default PortfolioOverview;
