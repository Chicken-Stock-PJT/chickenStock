import { useEffect, useState } from "react";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Input } from "@/shared/libs/ui/input";
import { Search, Heart } from "lucide-react";
import { useGetWatchlist } from "@/features/watchlist/model/queries";
import { Watchlist } from "@/features/watchlist/model/types";
import { useDeleteWatchlist } from "@/features/watchlist/model/mutations";
import { useNavigate } from "react-router-dom";
// 모의 데이터 - 관심종목

const WatchList = () => {
  const navigate = useNavigate();
  const { data: initialWatchlist } = useGetWatchlist();

  const [watchlist, setWatchlist] = useState<Watchlist[]>([]);
  useEffect(() => {
    if (initialWatchlist && "watchList" in initialWatchlist) {
      setWatchlist(initialWatchlist.watchList);
    }
  }, [initialWatchlist]);

  const [searchTerm, setSearchTerm] = useState("");

  const deleteMutation = useDeleteWatchlist();

  const handleWatchList = (code: string) => {
    deleteMutation.mutate(code, {});
  };
  // 검색 필터링
  const filteredWatchlist = watchlist.filter(
    (stock) =>
      stock.stockName.toLowerCase().includes(searchTerm.toLowerCase()) ||
      stock.stockCode.includes(searchTerm),
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
                <div key={stock.stockCode} className="flex items-center">
                  <Heart
                    className="ml-4 size-4 cursor-pointer"
                    stroke="red"
                    fill="red"
                    onClick={() => handleWatchList(stock.stockCode)}
                  />
                  <div
                    className="grid w-full cursor-pointer grid-cols-5 items-center gap-4 p-4"
                    onClick={() => void navigate(`/stocks/${stock.stockCode}`)}
                  >
                    <div className="flex items-center">
                      <div>
                        <div className="font-medium">{stock.stockName}</div>
                        <div className="text-xs text-muted-foreground">{stock.stockCode}</div>
                      </div>
                    </div>
                    <div className="text-right">{stock.currentPrice.toLocaleString()}원</div>
                    <div
                      className={`text-right ${
                        stock.priceChange.startsWith("+")
                          ? "text-red-500"
                          : stock.priceChange.startsWith("-")
                            ? "text-blue-500"
                            : "text-gray-500"
                      }`}
                    >
                      {stock.priceChange}원
                    </div>
                    <div
                      className={`flex items-center justify-end text-right ${
                        stock.priceChange.startsWith("+")
                          ? "text-red-500"
                          : stock.priceChange.startsWith("-")
                            ? "text-blue-500"
                            : "text-gray-500"
                      }`}
                    >
                      {stock.changeRate}%
                    </div>
                    <div className="text-right">{stock.tradingVolume}</div>
                  </div>
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
