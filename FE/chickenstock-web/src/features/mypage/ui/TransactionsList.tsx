import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";

// 모의 데이터 - 거래 내역
const transactions = [
  {
    id: 1,
    date: "2023-04-26T10:15:30",
    type: "buy",
    code: "005930",
    name: "삼성전자",
    quantity: 20,
    price: 72500,
    total: 1450000,
  },
  {
    id: 2,
    date: "2023-04-25T14:30:45",
    type: "sell",
    code: "035720",
    name: "카카오",
    quantity: 10,
    price: 56700,
    total: 567000,
  },
  {
    id: 3,
    date: "2023-04-24T11:20:15",
    type: "buy",
    code: "000660",
    name: "SK하이닉스",
    quantity: 5,
    price: 168000,
    total: 840000,
  },
  {
    id: 4,
    date: "2023-04-23T09:45:30",
    type: "buy",
    code: "051910",
    name: "LG화학",
    quantity: 2,
    price: 495000,
    total: 990000,
  },
  {
    id: 5,
    date: "2023-04-22T15:10:20",
    type: "sell",
    code: "035420",
    name: "NAVER",
    quantity: 3,
    price: 215000,
    total: 645000,
  },
];

const TransactionsList = () => {
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
            {transactions.map((transaction) => (
              <div key={transaction.id} className="grid grid-cols-6 items-center gap-4 p-4">
                <div>
                  {new Date(transaction.date).toLocaleDateString("ko-KR", {
                    year: "numeric",
                    month: "2-digit",
                    day: "2-digit",
                  })}
                </div>
                <div>
                  <div
                    className={`inline-block rounded-full border-transparent px-2 py-1 text-xs text-gray-50 shadow ${transaction.type === "buy" ? "bg-chart-red" : "bg-chart-blue"}`}
                  >
                    {transaction.type === "buy" ? "매수" : "매도"}
                  </div>
                  {/* <Badge variant={transaction.type === "buy" ? "default" : "destructive"}>
                  {transaction.type === "buy" ? "매수" : "매도"}
                </Badge> */}
                </div>
                <div>
                  <div className="font-medium">{transaction.name}</div>
                  <div className="text-xs text-muted-foreground">{transaction.code}</div>
                </div>
                <div className="text-right">{transaction.quantity.toLocaleString()}주</div>
                <div className="text-right">{transaction.price.toLocaleString()}원</div>
                <div className="text-right">{transaction.total.toLocaleString()}원</div>
              </div>
            ))}
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

export default TransactionsList;
