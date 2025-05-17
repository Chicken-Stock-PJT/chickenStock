interface TransactionRowProps {
  stockCode: string;
  timestamp: string;
  price: number;
  quantity: number;
  totalAmount: number;
  tradeType: "BUY" | "SELL";
}

const TransactionRow = ({
  timestamp,
  price,
  quantity,
  totalAmount,
  tradeType,
}: TransactionRowProps) => {
  return (
    <div className="grid grid-cols-5 items-center overflow-hidden text-nowrap p-2">
      {/* 매수 영역 */}
      {/* 수량 */}
      <div className={`flex items-center justify-start font-semibold`}>
        {price.toLocaleString()}원
      </div>

      {/* 가격 */}
      <div
        className={`flex items-center justify-end text-nowrap ${tradeType === "BUY" ? "text-chart-red" : "text-chart-blue"}`}
      >
        {quantity}주
      </div>

      <div
        className={`col-span-2 flex items-center justify-end ${tradeType === "BUY" ? "text-chart-red" : "text-chart-blue"}`}
      >
        {totalAmount.toLocaleString()}원
      </div>

      {/* 시간 */}

      <div className="flex items-center justify-end text-gray-500">
        {timestamp.slice(0, 2)}:{timestamp.slice(2, 4)}:{timestamp.slice(4, 6)}
      </div>
    </div>
  );
};

export default TransactionRow;
