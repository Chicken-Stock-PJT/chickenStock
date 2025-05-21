import { useMemo } from "react";
import React from "react";

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
  // 포맷팅된 값들 메모이제이션
  const formattedValues = useMemo(() => {
    return {
      price: price.toLocaleString(),
      quantity: quantity.toLocaleString(),
      totalAmount: totalAmount.toLocaleString(),
      time: `${timestamp.slice(0, 2)}:${timestamp.slice(2, 4)}:${timestamp.slice(4, 6)}`,
    };
  }, [price, quantity, totalAmount, timestamp]);

  // 스타일 클래스 메모이제이션
  const styles = useMemo(() => {
    const tradeTypeColor = tradeType === "BUY" ? "text-chart-red" : "text-chart-blue";
    return {
      quantityClass: `flex items-center justify-end text-nowrap text-xs sm:text-sm ${tradeTypeColor}`,
      totalAmountClass: `col-span-2 flex items-center justify-end text-xs sm:text-sm ${tradeTypeColor}`,
    };
  }, [tradeType]);

  return (
    <div className="grid grid-cols-5 items-center overflow-hidden text-nowrap p-1.5 sm:p-2">
      {/* 가격 */}
      <div className="flex items-center justify-start text-xs font-semibold sm:text-sm">
        {formattedValues.price}원
      </div>

      {/* 수량 */}
      <div className={styles.quantityClass}>{formattedValues.quantity}주</div>

      {/* 총액 */}
      <div className={styles.totalAmountClass}>{formattedValues.totalAmount}원</div>

      {/* 시간 */}
      <div className="flex items-center justify-end text-[10px] text-gray-500 sm:text-xs">
        {formattedValues.time}
      </div>
    </div>
  );
};

export default React.memo(TransactionRow);
