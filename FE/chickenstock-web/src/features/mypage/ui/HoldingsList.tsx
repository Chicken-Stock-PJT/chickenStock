import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";

const HoldingsList = () => {
  // 모의 데이터 - 보유 종목
  const holdings = [
    {
      code: "005930",
      name: "삼성전자",
      quantity: 100,
      avgPrice: 70000,
      currentPrice: 72800,
      value: 7280000,
      weight: 35,
    },
    {
      code: "000660",
      name: "SK하이닉스",
      quantity: 30,
      avgPrice: 160000,
      currentPrice: 168500,
      value: 5055000,
      weight: 24,
    },
    {
      code: "035720",
      name: "카카오",
      quantity: 50,
      avgPrice: 55000,
      currentPrice: 56700,
      value: 2835000,
      weight: 14,
    },
    {
      code: "035420",
      name: "NAVER",
      quantity: 10,
      avgPrice: 210000,
      currentPrice: 216500,
      value: 2165000,
      weight: 10,
    },
    {
      code: "051910",
      name: "LG화학",
      quantity: 5,
      avgPrice: 490000,
      currentPrice: 498000,
      value: 2490000,
      weight: 12,
    },
    {
      code: "기타",
      name: "기타",
      quantity: 0,
      avgPrice: 0,
      currentPrice: 0,
      value: 1000000,
      weight: 5,
    },
  ];

  return (
    <Card>
      <CardHeader>
        <CardTitle>보유 종목 현황</CardTitle>
        <CardDescription>현재 보유 중인 주식 종목과 수익률입니다.</CardDescription>
      </CardHeader>
      <CardContent>
        <div className="rounded-md border">
          <div className="grid grid-cols-7 gap-4 border-b p-4 font-medium">
            <div>종목명</div>
            <div className="text-right">보유수량</div>
            <div className="text-right">평균단가</div>
            <div className="text-right">현재가</div>
            <div className="text-right">평가금액</div>
            <div className="text-right">손익</div>
            <div className="text-right">비중</div>
          </div>
          <div className="divide-y">
            {holdings
              .filter((stock) => stock.name !== "기타")
              .map((stock) => {
                const profit = stock.currentPrice - stock.avgPrice;
                const profitRate = (profit / stock.avgPrice) * 100;
                const totalProfit = profit * stock.quantity;

                return (
                  <div key={stock.code} className="grid grid-cols-7 gap-4 p-4">
                    <div>
                      <div className="font-medium">{stock.name}</div>
                      <div className="text-xs text-muted-foreground">{stock.code}</div>
                    </div>
                    <div className="text-right">{stock.quantity.toLocaleString()}주</div>
                    <div className="text-right">{stock.avgPrice.toLocaleString()}원</div>
                    <div className="text-right">{stock.currentPrice.toLocaleString()}원</div>
                    <div className="text-right">{stock.value.toLocaleString()}원</div>
                    <div className="text-right">
                      <div className={profit >= 0 ? "text-green-500" : "text-red-500"}>
                        {profit >= 0 ? "+" : ""}
                        {totalProfit.toLocaleString()}원
                      </div>
                      <div className={`text-xs ${profit >= 0 ? "text-green-500" : "text-red-500"}`}>
                        {profit >= 0 ? "+" : ""}
                        {profitRate.toFixed(2)}%
                      </div>
                    </div>
                    <div className="text-right">{stock.weight}%</div>
                  </div>
                );
              })}
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

export default HoldingsList;
