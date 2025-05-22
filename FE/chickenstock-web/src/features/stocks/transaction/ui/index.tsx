import { TradeExecutionData } from "@/features/stocks/chart/model/types";
import { useEffect, useState, useCallback } from "react";
import TransactionRow from "@/features/stocks/transaction/ui/TransactionRow";
import React from "react";

const MemoizedTransactionRow = React.memo(TransactionRow);

interface TransactionProps {
  stockCode: string;
  tradeExecutionData: TradeExecutionData | null;
}

const Transaction = ({ stockCode, tradeExecutionData }: TransactionProps) => {
  const [transactionData, setTransactionData] = useState<TradeExecutionData[]>([]);

  // WebSocket 데이터 업데이트 처리
  useEffect(() => {
    if (!tradeExecutionData || tradeExecutionData.stockCode !== stockCode) return;

    setTransactionData((prevData) => {
      // 최대 100개의 거래 데이터만 유지
      const newData = [tradeExecutionData, ...prevData].slice(0, 100);
      return newData;
    });
  }, [tradeExecutionData, stockCode]);

  // 거래 데이터 렌더링 메모이제이션
  const renderTransactionRows = useCallback(() => {
    return transactionData.map((data, index) => (
      <MemoizedTransactionRow
        key={`${data.timestamp}-${index}`}
        stockCode={data.stockCode}
        tradeType={data.tradeType}
        timestamp={data.timestamp}
        price={data.price}
        quantity={data.quantity}
        totalAmount={data.totalAmount}
      />
    ));
  }, [transactionData]);

  // 거래 데이터가 없는 경우 처리
  if (transactionData.length === 0) {
    return (
      <div className="flex h-full max-h-[300px] min-h-[300px] flex-col overflow-hidden rounded-lg border border-gray-200 bg-white shadow-sm sm:max-h-[632px]">
        <header className="flex items-center justify-between border-b border-gray-200 px-3 py-2 sm:px-6 sm:py-3">
          <span className="text-sm font-semibold text-gray-800 sm:text-base">실시간 거래</span>
        </header>
        <div className="flex flex-1 items-center justify-center text-xs text-gray-500 sm:text-sm">
          거래 내역이 없습니다.
        </div>
      </div>
    );
  }

  return (
    <div className="flex h-full max-h-[300px] min-h-[300px] flex-col overflow-hidden rounded-lg border border-gray-200 bg-white shadow-sm sm:max-h-[632px]">
      <header className="flex items-center justify-between border-b border-gray-200 px-3 py-2 sm:px-6 sm:py-3">
        <span className="text-sm font-semibold text-gray-800 sm:text-base">실시간 거래</span>
      </header>

      <div className="min-h-0 flex-1 overflow-y-auto">
        <div className="divide-y divide-gray-100">{renderTransactionRows()}</div>
      </div>
    </div>
  );
};

export default React.memo(Transaction);
