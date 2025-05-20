import { useEffect, useState } from "react";
import { Search, Heart, ChevronRight } from "lucide-react";
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
    <div className="w-full rounded-lg border bg-white shadow">
      <div className="flex flex-col justify-between gap-4 border-b p-4 lg:flex-row lg:items-center">
        <div>
          <h3 className="text-lg font-semibold">관심종목</h3>
          <p className="text-sm text-gray-500">관심 있는 주식 종목을 관리합니다.</p>
        </div>
        <div className="relative w-full lg:w-64">
          <Search className="absolute left-2.5 top-2.5 size-4 text-gray-500" />
          <input
            type="search"
            placeholder="종목명 또는 코드 검색"
            className="w-full rounded-md border border-gray-300 py-2 pl-8 text-sm"
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
          />
        </div>
      </div>

      {/* 데스크톱 뷰 - 전통적인 테이블 레이아웃 */}
      <div className="hidden lg:block">
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
              <div
                key={stock.stockCode}
                className="flex items-center hover:bg-gray-50"
                onClick={(e) => {
                  e.stopPropagation();
                  void navigate(`/stocks/${stock.stockCode}`);
                }}
              >
                <Heart
                  className="ml-4 size-4 cursor-pointer"
                  stroke="red"
                  fill="red"
                  onClick={(e) => {
                    e.stopPropagation();
                    handleWatchList(stock.stockCode);
                  }}
                />
                <div className="grid w-full cursor-pointer grid-cols-5 items-center gap-4 p-4">
                  <div className="flex items-center">
                    <div>
                      <div className="font-medium">{stock.stockName}</div>
                      <div className="text-xs text-gray-500">{stock.stockCode.slice(0, 6)}</div>
                    </div>
                  </div>
                  <div className="text-right">{stock.currentPrice.toLocaleString()}원</div>
                  <div
                    className={`text-right ${
                      stock.priceChange.startsWith("+")
                        ? "text-red-500"
                        : stock.priceChange.startsWith("-")
                          ? "text-blue-500"
                          : ""
                    }`}
                  >
                    {Number(stock.priceChange).toLocaleString()}원
                  </div>
                  <div
                    className={`flex items-center justify-end text-right ${
                      stock.priceChange.startsWith("+")
                        ? "text-red-500"
                        : stock.priceChange.startsWith("-")
                          ? "text-blue-500"
                          : ""
                    }`}
                  >
                    {stock.changeRate}%
                  </div>
                  <div className="text-right">{Number(stock.tradingVolume).toLocaleString()}</div>
                </div>
              </div>
            ))
          ) : (
            <div className="my-20 p-8 text-center text-gray-500">
              {searchTerm ? "검색 결과가 없습니다." : "관심종목이 없습니다."}
            </div>
          )}
        </div>
      </div>

      {/* 모바일 뷰 - 카드 스타일 레이아웃 */}
      <div className="lg:hidden">
        {filteredWatchlist.length > 0 ? (
          <div className="divide-y">
            {filteredWatchlist.map((stock) => (
              <div
                key={stock.stockCode}
                className="group relative flex cursor-pointer items-center justify-between p-4 transition-colors hover:bg-gray-50 active:bg-gray-100"
                onClick={(e) => {
                  e.stopPropagation();
                  void navigate(`/stocks/${stock.stockCode}`);
                }}
              >
                {/* 왼쪽 컨테이너 */}
                <div className="flex flex-1 items-center gap-3">
                  {/* 종목 정보 */}
                  <div className="flex flex-1 flex-col gap-1">
                    <div className="flex items-center gap-2">
                      <Heart
                        className="size-4 cursor-pointer transition-transform hover:scale-110 active:scale-95"
                        stroke="red"
                        fill="red"
                        onClick={(e) => {
                          e.stopPropagation();
                          handleWatchList(stock.stockCode);
                        }}
                      />
                      <div className="flex flex-col">
                        <span className="font-medium">{stock.stockName}</span>
                        <span className="text-xs text-gray-500">{stock.stockCode.slice(0, 6)}</span>
                        {/* 가격정보 */}
                        <div className="flex justify-start gap-2 sm:hidden">
                          <div className="font-medium">{stock.currentPrice.toLocaleString()}원</div>

                          <div
                            className={`my-auto text-xs ${
                              stock.priceChange.startsWith("+")
                                ? "text-red-500"
                                : stock.priceChange.startsWith("-")
                                  ? "text-blue-500"
                                  : ""
                            }`}
                          >
                            {stock.changeRate}%
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>

                  {/* 가격 정보 */}
                  <div className="hidden flex-col items-end gap-1 sm:flex">
                    <div className="font-medium">{stock.currentPrice.toLocaleString()}원</div>
                    <div
                      className={`text-sm font-medium ${
                        stock.priceChange.startsWith("+")
                          ? "text-red-500"
                          : stock.priceChange.startsWith("-")
                            ? "text-blue-500"
                            : "text-gray-500"
                      }`}
                    >
                      {stock.changeRate}%
                    </div>
                  </div>
                </div>

                {/* 화살표 아이콘 */}
                <ChevronRight
                  className="ml-2 size-5 text-gray-400 transition-transform group-hover:translate-x-1"
                  onClick={(e) => {
                    e.stopPropagation();
                    void navigate(`/stocks/${stock.stockCode}`);
                  }}
                />
              </div>
            ))}
          </div>
        ) : (
          <div className="flex min-h-[200px] flex-col items-center justify-center gap-4 p-8 text-center">
            <div className="text-gray-400">
              {searchTerm ? (
                <>
                  <Search className="mx-auto mb-2 size-8" />
                  <p>검색 결과가 없습니다.</p>
                </>
              ) : (
                <>
                  <Heart className="mx-auto mb-2 size-8" />
                  <p>관심종목이 없습니다.</p>
                  <p className="mt-2 text-sm">관심있는 종목을 추가해보세요!</p>
                </>
              )}
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default WatchList;
