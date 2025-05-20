import { TradeExecutionData } from "@/features/stocks/chart/model/types";
import { useEffect } from "react";
import { useState } from "react";
import { useWebSocketStore } from "@/shared/store/websocket";
import TransactionRow from "@/features/stocks/transaction/ui/TransactionRow";

const Transaction = ({ stockCode }: { stockCode: string }) => {
  const [transactionData, setTransactionData] = useState<TradeExecutionData[]>([]);

  const { tradeExecutionData } = useWebSocketStore();

  useEffect(() => {
    if (tradeExecutionData && tradeExecutionData.stockCode === stockCode) {
      const newData = [tradeExecutionData, ...transactionData];
      setTransactionData(newData);
    }
  }, [tradeExecutionData]);

  return (
    <div className="flex h-full flex-col overflow-hidden rounded-lg border border-gray-200 bg-white shadow-sm">
      <header className="flex items-center justify-between border-b border-gray-200 px-6 py-3">
        <span className="font-semibold text-gray-800">실시간 거래</span>
      </header>

      <div className="min-h-0 flex-1 overflow-y-auto">
        <div className="divide-y divide-gray-100">
          {transactionData.map((data, index) => (
            <TransactionRow
              key={index}
              stockCode={data.stockCode}
              tradeType={data.tradeType}
              timestamp={data.timestamp}
              price={data.price}
              quantity={data.quantity}
              totalAmount={data.totalAmount}
            />
          ))}
        </div>
      </div>
    </div>
  );
};

export default Transaction;
