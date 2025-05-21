import { Card, CardContent, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { ArrowUp, ArrowDown } from "lucide-react";

interface PortfolioOverviewProps {
  totalAsset: number; // 총 자산 (현금 + 주식 평가금액)
  totalProfitLoss: number; // 총 손익 (평가금액 - 투자금액)
  totalReturnRate: number; // 총 수익률 (%)
  memberMoney: number; // 예수금 잔고
  pendingOrderAmount: number; // 미체결 주문에 묶인 자금
  stockValuation: number; // 주식 평가금액
  todayTradeAmount: number; // 금일 거래 금액
  todayProfitLoss: number; // 금일 수익
  todayReturnRate: number; // 금일 수익률
}
const PortfolioOverview = ({
  totalAsset,
  totalProfitLoss,
  totalReturnRate,
  memberMoney,
  pendingOrderAmount,
  stockValuation,
  todayTradeAmount,
  todayProfitLoss,
  todayReturnRate,
}: PortfolioOverviewProps) => {
  return (
    <div className="grid grid-cols-1 gap-6">
      {/* Total Assets Card */}
      <Card className="overflow-hidden">
        <CardHeader>
          <CardTitle>총 자산</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-3xl font-bold text-gray-900">{totalAsset.toLocaleString()}원</div>
          <div className="mt-3 flex items-center">
            <div
              className={`flex items-center text-base ${
                totalProfitLoss > 0 ? "text-red-500" : totalProfitLoss < 0 ? "text-blue-500" : ""
              }`}
            >
              {totalProfitLoss > 0 ? (
                <ArrowUp className="mr-1 size-4" />
              ) : totalProfitLoss < 0 ? (
                <ArrowDown className="mr-1 size-4" />
              ) : null}
              {totalProfitLoss >= 0 ? "+" : ""}
              {totalProfitLoss.toLocaleString()}원 ({totalProfitLoss >= 0 ? "+" : ""}
              {totalReturnRate.toFixed(2)}%)
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Cash Balance Card */}
      <Card className="overflow-hidden">
        <CardHeader>
          <CardTitle>예수금 잔고</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="mb-4 text-3xl font-bold text-gray-900">
            {memberMoney.toLocaleString()}원
          </div>

          {/* Asset Liquidity Section */}
          <div>
            <div className="mb-2 text-sm font-medium text-gray-600">자산 유동성</div>
            {/* Liquidity Bar Chart */}
            <div className="mb-3 flex h-5 w-full overflow-hidden rounded-full">
              <div
                className="bg-blue-500 transition-all duration-500"
                style={{ width: `${(memberMoney / totalAsset) * 100}%` }}
                title="즉시 활용 가능 현금"
              />
              <div
                className="bg-[#FDD141] transition-all duration-500"
                style={{ width: `${(pendingOrderAmount / totalAsset) * 100}%` }}
                title="미체결 주문에 묶인 자금"
              />
              <div
                className="bg-green-500 transition-all duration-500"
                style={{ width: `${(stockValuation / totalAsset) * 100}%` }}
                title="주식 평가금액"
              />
            </div>

            {/* Liquidity Legend and Amounts */}
            <div className="grid grid-cols-3 gap-4 text-sm">
              <div>
                <div className="mb-1 flex items-center">
                  <span className="mr-2 inline-block size-3 rounded-sm bg-blue-500"></span>
                  <span className="text-gray-600">가용 현금</span>
                </div>
                <div className="text-xs font-semibold md:text-sm">
                  {memberMoney.toLocaleString()}원
                </div>
                <div className="text-xs text-gray-500">
                  {((memberMoney / totalAsset) * 100).toFixed(1)}%
                </div>
              </div>
              <div>
                <div className="mb-1 flex items-center">
                  <span className="mr-2 inline-block size-3 rounded-sm bg-[#FDD141]"></span>
                  <span className="text-gray-600">매수 대기</span>
                </div>
                <div className="text-xs font-semibold md:text-sm">
                  {pendingOrderAmount.toLocaleString()}원
                </div>
                <div className="text-xs text-gray-500">
                  {((pendingOrderAmount / totalAsset) * 100).toFixed(1)}%
                </div>
              </div>
              <div>
                <div className="mb-1 flex items-center">
                  <span className="mr-2 inline-block size-3 rounded-sm bg-green-500"></span>
                  <span className="text-gray-600">주식</span>
                </div>
                <div className="text-xs font-semibold md:text-sm">
                  {stockValuation.toLocaleString()}원
                </div>
                <div className="text-xs text-gray-500">
                  {((stockValuation / totalAsset) * 100).toFixed(1)}%
                </div>
              </div>
            </div>
          </div>
        </CardContent>
      </Card>

      <Card className="overflow-hidden">
        <CardHeader>
          <CardTitle>금일 매매 활동</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="mb-5 grid grid-cols-1 gap-6 md:grid-cols-2">
            <div>
              <div className="text-sm font-medium text-gray-600">거래 금액</div>
              <div className="text-2xl font-bold text-gray-900">
                {todayTradeAmount.toLocaleString()}원
              </div>
            </div>
            <div>
              <div className="text-sm font-medium text-gray-600">금일 실현 수익</div>
              <div
                className={`flex items-center gap-2 text-2xl font-bold ${
                  todayProfitLoss > 0
                    ? "text-red-500"
                    : todayProfitLoss < 0
                      ? "text-blue-500"
                      : "text-gray-900"
                }`}
              >
                {todayProfitLoss > 0 ? (
                  <ArrowUp className="mr-1 size-5" />
                ) : todayProfitLoss < 0 ? (
                  <ArrowDown className="mr-1 size-5" />
                ) : null}
                <span>
                  {todayProfitLoss >= 0 ? "+" : ""}
                  {todayProfitLoss.toLocaleString()}원
                </span>
                <span
                  className={`flex items-center text-sm ${
                    todayReturnRate > 0
                      ? "text-red-500"
                      : todayReturnRate < 0
                        ? "text-blue-500"
                        : "text-gray-900"
                  }`}
                >
                  <span className="ml-1">
                    {todayReturnRate > 0 ? (
                      <ArrowUp className="mr-1 inline size-4" />
                    ) : todayReturnRate < 0 ? (
                      <ArrowDown className="mr-1 inline size-4" />
                    ) : null}
                    {todayReturnRate >= 0 ? "+" : ""}
                    {todayReturnRate.toFixed(2)}%
                  </span>
                </span>
              </div>
            </div>
          </div>

          <div className="mb-4 flex items-center justify-between">
            <div className="text-sm">
              <span className="flex items-center">
                <span className="text-gray-600">매매회전율: </span>
                <span className="ml-1 font-medium text-gray-900">
                  {((todayTradeAmount / totalAsset) * 100).toFixed(2)}%
                </span>
              </span>
            </div>
          </div>

          <div>
            <div className="mb-1 flex justify-between text-xs text-gray-500">
              <span>0%</span>
              <span>10%</span>
            </div>
            <div className="h-3 w-full rounded-full bg-gray-100">
              <div
                className="h-3 rounded-full bg-[#FDD141] transition-all duration-500"
                style={{ width: `${Math.min((todayTradeAmount / totalAsset) * 100 * 10, 100)}%` }}
              />
            </div>
            <div className="mt-2 text-xs text-gray-500">총 자산 대비 오늘 거래 비율</div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
};

export default PortfolioOverview;
