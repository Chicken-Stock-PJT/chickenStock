import { OrderRowProps } from "@/features/stocks/orderBook/model/types";
import { useMemo } from "react";

const OrderRow = ({ price, askVolume, bidVolume, max, isCurrentPrice = false }: OrderRowProps) => {
  // console.log(price, askVolume, bidVolume, max, isCurrentPrice);
  // 가격 포맷팅 메모이제이션
  const formattedPrice = useMemo(() => {
    return price.toLocaleString();
  }, [price]);

  // 호가 종류 판단 메모이제이션
  const isPurchase = useMemo(() => {
    return !askVolume && bidVolume > 0;
  }, [askVolume, bidVolume]);

  // 스타일 계산 메모이제이션
  const styles = useMemo(() => {
    return {
      bidWidth: `${(bidVolume / max) * 100}%`,
      askWidth: `${(askVolume / max) * 100}%`,
      containerClass: `
        relative grid h-6 sm:h-8 grid-cols-3
        ${isCurrentPrice ? "bg-blue-50" : "hover:bg-gray-50"}
        ${isPurchase ? "text-chart-red" : "text-chart-blue"}
      `,
      priceClass: `flex items-center justify-center text-[10px] xs:text-xs sm:text-sm font-semibold ${
        isCurrentPrice ? "text-blue-700" : "text-gray-800"
      }`,
    };
  }, [isCurrentPrice, isPurchase, bidVolume, askVolume, max]);

  return (
    <div className={styles.containerClass}>
      {/* 매수 영역 */}
      <div className="relative flex items-center justify-end pr-3 sm:pr-6">
        {bidVolume > 0 && (
          <div
            className="absolute right-0 top-0 z-0 h-full rounded-l-sm bg-chart-red bg-opacity-20"
            style={{ width: styles.bidWidth }}
          />
        )}
        <span className="xs:text-xs relative z-10 text-[8px] font-medium sm:text-sm">
          {bidVolume ? bidVolume.toLocaleString() : ""}
        </span>
      </div>

      {/* 가격 영역 */}
      <div className={styles.priceClass}>
        {isCurrentPrice && <span className="mr-1 text-[8px] text-blue-600 sm:text-xs">●</span>}
        {formattedPrice}
        {isCurrentPrice && (
          <span className="mr-1 text-[8px] text-blue-600 opacity-0 sm:text-xs">●</span>
        )}
      </div>

      {/* 매도 영역 */}
      <div className="relative flex items-center pl-3 sm:pl-6">
        {askVolume > 0 && (
          <div
            className="absolute left-0 top-0 z-0 h-full rounded-r-sm bg-chart-blue bg-opacity-20"
            style={{ width: styles.askWidth }}
          />
        )}
        <span className="xs:text-xs relative z-10 text-[8px] font-medium sm:text-sm">
          {askVolume ? askVolume.toLocaleString() : ""}
        </span>
      </div>
    </div>
  );
};

export default OrderRow;
