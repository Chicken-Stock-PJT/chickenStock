import { Card, CardContent, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { ArrowUpIcon, ArrowDownIcon } from "lucide-react";

const PortfolioOverview = () => {
  const user = {
    email: "user@example.com",
    nickname: "치킨러버",
    joinDate: "2023-01-15",
    accountBalance: 12500000,
    totalProfit: 1850000,
    profitRate: 17.3,
    todayProfit: 125000,
    todayProfitRate: 1.2,
    ranking: 342,
    totalUsers: 10542,
    rankingPercentile: 3.2,
  };
  return (
    <div className="grid grid-cols-1 gap-6">
      <Card>
        <CardHeader className="pb-2">
          <CardTitle>총 자산</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{user.accountBalance.toLocaleString()}원</div>
          <div className="mt-2 flex items-center">
            <div
              className={`flex items-center ${user.totalProfit >= 0 ? "text-green-500" : "text-red-500"}`}
            >
              {user.totalProfit >= 0 ? (
                <ArrowUpIcon className="mr-1 size-4" />
              ) : (
                <ArrowDownIcon className="mr-1 size-4" />
              )}
              <span>
                {user.totalProfit >= 0 ? "+" : ""}
                {user.profitRate.toFixed(2)}% ({user.totalProfit >= 0 ? "+" : ""}
                {user.totalProfit.toLocaleString()}원)
              </span>
            </div>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader className="pb-2">
          <CardTitle>금일 수익</CardTitle>
        </CardHeader>
        <CardContent>
          <div
            className={`text-2xl font-bold ${user.todayProfit >= 0 ? "text-green-500" : "text-red-500"}`}
          >
            {user.todayProfit >= 0 ? "+" : ""}
            {user.todayProfit.toLocaleString()}원
          </div>
          <div className="mt-2 flex items-center">
            <div
              className={`flex items-center ${user.todayProfit >= 0 ? "text-green-500" : "text-red-500"}`}
            >
              {user.todayProfit >= 0 ? (
                <ArrowUpIcon className="mr-1 size-4" />
              ) : (
                <ArrowDownIcon className="mr-1 size-4" />
              )}
              <span>
                {user.todayProfit >= 0 ? "+" : ""}
                {user.todayProfitRate.toFixed(2)}%
              </span>
            </div>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader className="pb-2">
          <CardTitle>보유 종목 수</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-2xl font-bold">{5}개</div>
          <div className="mt-2 text-sm text-muted-foreground">다양한 섹터에 분산 투자 중</div>
        </CardContent>
      </Card>
    </div>
  );
};

export default PortfolioOverview;
