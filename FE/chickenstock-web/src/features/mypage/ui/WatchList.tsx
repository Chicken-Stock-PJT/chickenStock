import { useState } from "react";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Input } from "@/shared/libs/ui/input";
import { Search, Heart, ArrowUpIcon, ArrowDownIcon } from "lucide-react";

// 모의 데이터 - 관심종목
const initialWatchlist = [
  {
    code: "005930",
    name: "삼성전자",
    price: 72800,
    change: 1300,
    changePercent: 1.82,
    volume: 12345678,
  },
  {
    code: "000660",
    name: "SK하이닉스",
    price: 168500,
    change: 3500,
    changePercent: 2.12,
    volume: 8765432,
  },
  {
    code: "035720",
    name: "카카오",
    price: 56700,
    change: -1200,
    changePercent: -2.07,
    volume: 3456789,
  },
  {
    code: "035420",
    name: "NAVER",
    price: 216500,
    change: 2500,
    changePercent: 1.17,
    volume: 2345678,
  },
  {
    code: "051910",
    name: "LG화학",
    price: 498000,
    change: -7000,
    changePercent: -1.39,
    volume: 987654,
  },
  {
    code: "373220",
    name: "LG에너지솔루션",
    price: 435000,
    change: 5000,
    changePercent: 1.16,
    volume: 876543,
  },
  {
    code: "207940",
    name: "삼성바이오로직스",
    price: 789000,
    change: -11000,
    changePercent: -1.37,
    volume: 765432,
  },
];

const WatchList = () => {
  const [watchlist, setWatchlist] = useState(initialWatchlist);
  const [searchTerm, setSearchTerm] = useState("");

  // 관심종목 삭제
  const handleWatchList = (code: string) => {
    setWatchlist(watchlist.filter((stock) => stock.code !== code));
  };

  // 검색 필터링
  const filteredWatchlist = watchlist.filter(
    (stock) =>
      stock.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
      stock.code.includes(searchTerm),
  );

  return (
    <Card>
      <CardHeader>
        <div className="flex flex-col justify-between gap-4 md:flex-row md:items-center">
          <div>
            <CardTitle>관심종목</CardTitle>
            <CardDescription>관심 있는 주식 종목을 관리합니다.</CardDescription>
          </div>
          <div className="relative w-full md:w-64">
            <Search className="absolute left-2.5 top-2.5 size-4 text-muted-foreground" />
            <Input
              type="search"
              placeholder="종목명 또는 코드 검색"
              className="pl-8"
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
            />
          </div>
        </div>
      </CardHeader>
      <CardContent>
        <div className="rounded-md border">
          <div className="grid grid-cols-5 gap-4 border-b p-4 font-medium">
            <div className="ml-8">종목명</div>
            <div className="text-right">현재가</div>
            <div className="text-right">전일대비</div>
            <div className="text-right">등락률</div>
            <div className="text-right">거래량</div>
          </div>
          <div className="divide-y">
            {filteredWatchlist.length > 0 ? (
              filteredWatchlist.map((stock) => (
                <div key={stock.code} className="grid grid-cols-5 items-center gap-4 p-4">
                  <div className="flex items-center">
                    <Heart
                      className="size-4"
                      stroke="red"
                      fill="red"
                      onClick={() => handleWatchList(stock.code)}
                    />
                    <div className="ml-4">
                      <div className="font-medium">{stock.name}</div>
                      <div className="text-xs text-muted-foreground">{stock.code}</div>
                    </div>
                  </div>
                  <div className="text-right">{stock.price.toLocaleString()}원</div>
                  <div
                    className={`text-right ${
                      stock.change > 0
                        ? "text-red-500"
                        : stock.change < 0
                          ? "text-blue-500"
                          : "text-gray-500"
                    }`}
                  >
                    {stock.change > 0 ? "+" : ""}
                    {stock.change.toLocaleString()}원
                  </div>
                  <div
                    className={`flex items-center justify-end text-right ${
                      stock.change > 0
                        ? "text-red-500"
                        : stock.change < 0
                          ? "text-blue-500"
                          : "text-gray-500"
                    }`}
                  >
                    {stock.change > 0 && <ArrowUpIcon className="mr-1 size-4" />}
                    {stock.change < 0 && <ArrowDownIcon className="mr-1 size-4" />}
                    {stock.change > 0 ? "+" : ""}
                    {stock.changePercent.toFixed(2)}%
                  </div>
                  <div className="text-right">{stock.volume.toLocaleString()}</div>
                </div>
              ))
            ) : (
              <div className="p-8 text-center text-muted-foreground">
                {searchTerm ? "검색 결과가 없습니다." : "관심종목이 없습니다."}
              </div>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

export default WatchList;
