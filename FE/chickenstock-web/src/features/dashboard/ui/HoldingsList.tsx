import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Position } from "@/features/mypage/model/types";
import { useNavigate } from "react-router-dom";
import { RefreshCw, TrendingUp, TrendingDown, ArrowUpRight, ArrowDownRight } from "lucide-react";

interface HoldingsListProps {
  holdings: Position[];
  onClick: () => void;
  isLoading: boolean;
}

const HoldingsList = ({ holdings, onClick, isLoading }: HoldingsListProps) => {
  const navigate = useNavigate();

  const getPriceDifference = (currentPrice: number, averagePrice: number) => {
    const difference = currentPrice - averagePrice;
    const percentage = (difference / averagePrice) * 100;
    return {
      difference,
      percentage,
      isPositive: difference > 0,
    };
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle>
          <div className="flex items-center gap-4">
            <span>보유 종목 현황</span>
            <button
              className="cursor-pointer rounded-full bg-gray-100 p-1.5 text-gray-600 transition-colors hover:bg-gray-200"
              onClick={onClick}
            >
              <RefreshCw className={`size-4 ${isLoading ? "animate-spin" : ""}`} />
            </button>
          </div>
        </CardTitle>
        <CardDescription>현재 보유 중인 주식 종목과 수익률입니다.</CardDescription>
      </CardHeader>
      <CardContent className="overflow-auto p-0">
        {/* 데스크톱 헤더 */}
        <div className="hidden grid-cols-12 gap-2 border-y px-6 py-4 font-medium lg:grid">
          <div className="col-span-3">종목명</div>
          <div className="col-span-1 text-right">보유수량</div>
          <div className="col-span-2 text-right">평균단가</div>
          <div className="col-span-2 text-right">현재가</div>
          <div className="col-span-2 text-right">평가금액</div>
          <div className="col-span-2 text-right">손익</div>
        </div>
        <div className="divide-y">
          {holdings.length > 0 ? (
            holdings.map((stock) => {
              const priceDiff = getPriceDifference(stock.currentPrice, stock.averagePrice);
              return (
                <div
                  key={stock.stockCode}
                  className="cursor-pointer p-4 transition-colors hover:bg-gray-50 lg:px-6"
                  onClick={() => void navigate(`/stocks/${stock.stockCode}`)}
                >
                  {/* 모바일 뷰 */}
                  <div className="px-2 lg:hidden">
                    <div className="flex items-start justify-between">
                      <div>
                        <div className="items-center gap-2 sm:flex">
                          <div className="font-medium">{stock.stockName}</div>
                          <div className="text-xs text-muted-foreground">{stock.stockCode}</div>
                        </div>
                        <div className="mt-1 flex flex-col items-start text-left">
                          <span className="text-sm text-muted-foreground">
                            {stock.quantity.toLocaleString()}주
                          </span>
                          <span className="text-sm text-muted-foreground">
                            {stock.averagePrice.toLocaleString()}원
                          </span>
                        </div>
                      </div>
                      <div className="h-1 text-right">
                        <div className="font-medium">{stock.currentPrice.toLocaleString()}원</div>
                        <div className="mb-0 flex flex-col items-end justify-between text-sm sm:flex-row">
                          <span
                            className={`${
                              priceDiff.isPositive ? "text-chart-red" : "text-chart-blue"
                            } flex items-center gap-1`}
                          >
                            {priceDiff.isPositive ? (
                              <TrendingUp className="size-3 text-chart-red" />
                            ) : (
                              <TrendingDown className="size-3 text-chart-blue" />
                            )}
                            <span>
                              {priceDiff.isPositive ? "+" : ""}
                              {priceDiff.difference.toLocaleString()}원
                            </span>
                          </span>
                          <span
                            className={priceDiff.isPositive ? "text-chart-red" : "text-chart-blue"}
                          >
                            ({priceDiff.isPositive ? "+" : ""}
                            {priceDiff.percentage.toFixed(2)}%)
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>

                  {/* 데스크톱 뷰 */}
                  <div className="hidden grid-cols-12 items-center gap-2 lg:grid">
                    <div className="col-span-3">
                      <div className="font-medium">{stock.stockName}</div>
                      <div className="text-xs text-muted-foreground">{stock.stockCode}</div>
                    </div>
                    <div className="col-span-1 text-right">{stock.quantity.toLocaleString()}주</div>
                    <div className="col-span-2 text-right">
                      <div>{stock.averagePrice.toLocaleString()}원</div>
                      <div className="flex items-center justify-end gap-1 text-xs">
                        {priceDiff.isPositive ? (
                          <TrendingUp className="size-3 text-chart-red" />
                        ) : (
                          <TrendingDown className="size-3 text-chart-blue" />
                        )}
                        <span
                          className={priceDiff.isPositive ? "text-chart-red" : "text-chart-blue"}
                        >
                          {priceDiff.isPositive ? "+" : ""}
                          {priceDiff.difference.toLocaleString()}원
                        </span>
                      </div>
                    </div>
                    <div className="col-span-2 text-right">
                      <div>{stock.currentPrice.toLocaleString()}원</div>
                      <div
                        className={`text-xs ${
                          priceDiff.isPositive ? "text-chart-red" : "text-chart-blue"
                        }`}
                      >
                        ({priceDiff.isPositive ? "+" : ""}
                        {priceDiff.percentage.toFixed(2)}%)
                      </div>
                    </div>
                    <div className="col-span-2 text-right">
                      {stock.valuationAmount.toLocaleString()}원
                    </div>
                    <div className="col-span-2 text-right">
                      <div
                        className={
                          stock.profitLoss > 0
                            ? "text-chart-red"
                            : stock.profitLoss < 0
                              ? "text-chart-blue"
                              : ""
                        }
                      >
                        {stock.profitLoss > 0 ? "+" : ""}
                        {stock.profitLoss.toLocaleString()}원
                      </div>
                      <div
                        className={`text-xs ${
                          stock.returnRate > 0
                            ? "text-chart-red"
                            : stock.returnRate < 0
                              ? "text-chart-blue"
                              : ""
                        }`}
                      >
                        ({stock.returnRate > 0 ? "+" : ""}
                        {stock.returnRate.toFixed(2)}%)
                      </div>
                    </div>
                  </div>
                </div>
              );
            })
          ) : (
            <div className="my-20 p-4 text-center text-muted-foreground">보유 종목이 없습니다.</div>
          )}
        </div>
      </CardContent>
    </Card>
  );
};

export default HoldingsList;
