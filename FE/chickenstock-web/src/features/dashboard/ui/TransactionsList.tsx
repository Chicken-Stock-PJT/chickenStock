import { Card, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { TradeHistory, TransactionResponse } from "@/features/mypage/model/types";
import { useEffect, useRef } from "react";
import { InfiniteData } from "@tanstack/react-query";

interface TransactionsListProps {
  data: InfiniteData<TransactionResponse>;
  isLoading: boolean;
  error: Error | null;
  fetchNextPage: () => void;
  hasNextPage: boolean;
  isFetchingNextPage: boolean;
}

const TransactionsList = ({
  data,
  isLoading,
  error,
  fetchNextPage,
  hasNextPage,
  isFetchingNextPage,
}: TransactionsListProps) => {
  // 무한 스크롤 state
  const observerRef = useRef<IntersectionObserver | null>(null);
  const loadMoreRef = useRef<HTMLDivElement>(null);

  // 무한 스크롤 처리
  useEffect(() => {
    if (!loadMoreRef.current || !hasNextPage) return;

    observerRef.current = new IntersectionObserver(
      (entries) => {
        if (entries[0].isIntersecting && !isFetchingNextPage) {
          void fetchNextPage();
        }
      },
      { threshold: 0.5 },
    );

    observerRef.current.observe(loadMoreRef.current);

    return () => {
      if (observerRef.current) {
        observerRef.current.disconnect();
      }
    };
  }, [fetchNextPage, hasNextPage, isFetchingNextPage]);

  if (isLoading) {
    return <div>Loading...</div>;
  }

  if (error) {
    return <div>Error: {error.message}</div>;
  }

  const transactions = data?.pages.flatMap((page) => page.tradeHistories) ?? [];

  return (
    <Card>
      <CardHeader>
        <CardTitle>거래 내역</CardTitle>
        <CardDescription>최근 거래 내역을 확인합니다.</CardDescription>
      </CardHeader>
      <div className="border-t">
        <div className="hidden grid-cols-6 gap-4 border-b p-4 font-medium lg:grid">
          <div>날짜</div>
          <div>거래유형</div>
          <div>종목명</div>
          <div className="text-right">수량</div>
          <div className="text-right">거래단가</div>
          <div className="text-right">총액</div>
        </div>
        <div className="divide-y">
          {transactions.length > 0 ? (
            transactions.map((transaction: TradeHistory) => (
              <div
                key={transaction.createdAt}
                className="grid grid-cols-2 items-center gap-0 p-4 lg:grid-cols-6 lg:gap-4"
              >
                <div>
                  {new Date(transaction.tradedAt).toLocaleDateString("ko-KR", {
                    year: "numeric",
                    month: "2-digit",
                    day: "2-digit",
                  })}
                </div>
                <div className="ml-auto lg:ml-0">
                  <div
                    className={`inline-block rounded-full border-transparent px-2 py-1 text-xs text-gray-50 shadow ${
                      transaction.tradeType === "BUY" ? "bg-chart-red" : "bg-chart-blue"
                    }`}
                  >
                    {transaction.tradeType === "BUY" ? "매수" : "매도"}
                  </div>
                </div>
                <div>
                  <div className="mt-4 font-medium lg:mt-0">{transaction.stockName}</div>
                </div>
                <div className="mt-4 text-right lg:mt-0">
                  {transaction.quantity.toLocaleString()}주
                </div>
                <div className="col-span-2 text-right lg:col-span-1">
                  {transaction.unitPrice.toLocaleString()}원
                </div>
                <div className="hidden text-right lg:block">
                  {(transaction.quantity * transaction.unitPrice).toLocaleString()}원
                </div>
              </div>
            ))
          ) : (
            <div className="my-20 p-4 text-center text-muted-foreground">거래 내역이 없습니다.</div>
          )}
        </div>
      </div>
      <div ref={loadMoreRef}>
        {isFetchingNextPage && <div className="text-center">Loading more...</div>}
      </div>
    </Card>
  );
};

export default TransactionsList;
