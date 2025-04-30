import { OrderRowProps } from "@/features/stocks/orderBook/model/types";

const OrderRow = ({ price, askVolume, bidVolume, max, isCurrentPrice = false }: OrderRowProps) => {
  console.log(price, askVolume, bidVolume, max, isCurrentPrice);
  // 가격 포맷팅 함수
  const formatPrice = (price: number) => {
    return price.toLocaleString();
  };

  // 호가 종류 판단 (매수/매도)
  const isPurchase = !askVolume && bidVolume > 0;

  return (
    <div
      className={`
        relative grid h-8 grid-cols-3
        ${isCurrentPrice ? "bg-blue-50" : "hover:bg-gray-50"}
        ${isPurchase ? "text-chart-red" : "text-chart-blue"}
      `}
    >
      {/* 매수 영역 */}
      <div className="relative flex items-center justify-end pr-6">
        {bidVolume > 0 && (
          <div
            className="absolute right-0 top-0 z-0 h-full rounded-l-sm bg-chart-red bg-opacity-20"
            style={{ width: `${(bidVolume / max) * 100}%` }}
          ></div>
        )}
        <span className="relative z-10 font-medium">
          {bidVolume ? bidVolume.toLocaleString() : ""}
        </span>
      </div>

      {/* 가격 영역 */}
      <div
        className={`flex items-center justify-center font-semibold ${isCurrentPrice ? "text-blue-700" : "text-gray-800"}`}
      >
        {isCurrentPrice && <span className="mr-1 text-xs text-blue-600">●</span>}
        {formatPrice(price)}
        {isCurrentPrice && <span className="mr-1 text-xs text-blue-600 opacity-0">●</span>}
      </div>

      {/* 매도 영역 */}
      <div className="relative flex items-center justify-start pl-6">
        {askVolume > 0 && (
          <div
            className="absolute left-0 top-0 z-0 h-full rounded-r-sm bg-chart-blue bg-opacity-20"
            style={{ width: `${(askVolume / max) * 100}%` }}
          ></div>
        )}
        <span className="relative z-10 font-medium">
          {askVolume ? askVolume.toLocaleString() : ""}
        </span>
      </div>
    </div>
  );
};

export default OrderRow;
