import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Position } from "../model/types";
import { useNavigate } from "react-router-dom";
import { RefreshCw } from "lucide-react";
interface HoldingsListProps {
  holdings: Position[];
  onClick: () => void;
  isLoading: boolean;
}

const HoldingsList = ({ holdings, onClick, isLoading }: HoldingsListProps) => {
  const navigate = useNavigate();

  return (
    <Card>
      <CardHeader>
        <CardTitle>
          <div className="flex items-center gap-4">
            <span>보유 종목 현황</span>
            <button className="cursor-pointer rounded-full bg-gray-400 p-1 text-white">
              <RefreshCw
                className={`size-4 ${isLoading ? "animate-spin" : ""}`}
                onClick={onClick}
              />
            </button>
          </div>
        </CardTitle>
        <CardDescription>현재 보유 중인 주식 종목과 수익률입니다.</CardDescription>
      </CardHeader>
      <CardContent>
        <div className="rounded-md border">
          <div className="grid grid-cols-12 gap-2 border-b p-4 font-medium">
            <div className="col-span-3">종목명</div>
            <div className="col-span-1 text-right">보유수량</div>
            <div className="col-span-2 text-right">평균단가</div>
            <div className="col-span-2 text-right">현재가</div>
            <div className="col-span-2 text-right">평가금액</div>
            <div className="col-span-2 text-right">손익</div>
            {/* <div className="text-right">비중</div> */}
          </div>
          <div className="divide-y">
            {holdings.length > 0 ? (
              holdings.map((stock) => (
                <div
                  key={stock.stockCode}
                  className="grid cursor-pointer grid-cols-12 items-center gap-2 p-4 hover:bg-gray-50"
                  onClick={() => void navigate(`/stocks/${stock.stockCode}`)}
                >
                  <div className="col-span-3">
                    <div className="font-medium">{stock.stockName}</div>
                    <div className="text-xs text-muted-foreground">{stock.stockCode}</div>
                  </div>
                  <div className="col-span-1 text-right">{stock.quantity.toLocaleString()}주</div>
                  <div className="col-span-2 text-right">
                    {stock.averagePrice.toLocaleString()}원
                  </div>
                  <div className="col-span-2 text-right">
                    {stock.currentPrice.toLocaleString()}원
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
                      className={`text-xs ${stock.returnRate > 0 ? "text-chart-red" : stock.returnRate < 0 ? "text-chart-blue" : ""}`}
                    >
                      {stock.returnRate > 0 ? "+" : ""}
                      {stock.returnRate.toFixed(2)}%
                    </div>
                  </div>
                  {/* <div className="text-right">{stock.weight}%</div> */}
                </div>
              ))
            ) : (
              <div className="my-20 p-4 text-center text-muted-foreground">
                보유 종목이 없습니다.
              </div>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

export default HoldingsList;
