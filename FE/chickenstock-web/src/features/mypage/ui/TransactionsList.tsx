import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { useGetTransactions } from "../model/queries";
import { TradeHistory } from "@/features/mypage/model/types";

const TransactionsList = () => {
  const { data, isLoading, error } = useGetTransactions();

  if (isLoading) {
    return <div>Loading...</div>;
  }

  if (error) {
    return <div>Error: {error.message}</div>;
  }
  return (
    <Card>
      <CardHeader>
        <CardTitle>거래 내역</CardTitle>
        <CardDescription>최근 거래 내역을 확인합니다.</CardDescription>
      </CardHeader>
      <CardContent>
        <div className="rounded-md border">
          <div className="grid grid-cols-6 gap-4 border-b p-4 font-medium">
            <div>날짜</div>
            <div>거래유형</div>
            <div>종목명</div>
            <div className="text-right">수량</div>
            <div className="text-right">가격</div>
            <div className="text-right">총액</div>
          </div>
          <div className="divide-y">
            {data.tradeHistories.map((transaction: TradeHistory) => (
              <div key={transaction.id} className="grid grid-cols-6 items-center gap-4 p-4">
                <div>
                  {new Date(transaction.tradedAt).toLocaleDateString("ko-KR", {
                    year: "numeric",
                    month: "2-digit",
                    day: "2-digit",
                  })}
                </div>
                <div>
                  <div
                    className={`inline-block rounded-full border-transparent px-2 py-1 text-xs text-gray-50 shadow ${transaction.type === "BUY" ? "bg-chart-red" : "bg-chart-blue"}`}
                  >
                    {transaction.tradeType === "BUY" ? "매수" : "매도"}
                  </div>
                  {/* <Badge variant={transaction.type === "buy" ? "default" : "destructive"}>
                  {transaction.type === "buy" ? "매수" : "매도"}
                </Badge> */}
                </div>
                <div>
                  <div className="font-medium">{transaction.stockName}</div>
                  <div className="text-xs text-muted-foreground">{transaction.stockCode}</div>
                </div>
                <div className="text-right">{transaction.quantity.toLocaleString()}주</div>
                <div className="text-right">{transaction.unitPrice.toLocaleString()}원</div>
                <div className="text-right">
                  {(transaction.quantity * transaction.unitPrice).toLocaleString()}원
                </div>
              </div>
            ))}
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

export default TransactionsList;
