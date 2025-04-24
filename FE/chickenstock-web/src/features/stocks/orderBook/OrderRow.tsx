interface OrderRowProps {
  price: number;
  askVolume: number;
  bidVolume: number;
  max: number;
  isCurrentPrice?: boolean;
}

const OrderRow = ({ price, askVolume, bidVolume, max, isCurrentPrice = false }: OrderRowProps) => {
  // 가격 포맷팅 함수
  const formatPrice = (price: number) => {
    return price.toLocaleString();
  };

  // 호가 종류 판단 (매수/매도)
  const isPurchase = !askVolume && bidVolume > 0;

  return (
    <div
      className={`
        grid grid-cols-3 h-8 relative
        ${isCurrentPrice ? "bg-blue-50" : "hover:bg-gray-50"}
        ${isPurchase ? "text-chart-red" : "text-chart-blue"}
      `}
    >
      {/* 매수 영역 */}
      <div className="flex items-center justify-end relative pr-6">
        {bidVolume > 0 && (
          <div
            className="bg-chart-red bg-opacity-20 h-full absolute top-0 right-0 z-0 rounded-l-sm"
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
      <div className="flex items-center justify-start relative pl-6">
        {askVolume > 0 && (
          <div
            className="bg-chart-blue bg-opacity-20 h-full absolute top-0 left-0 z-0 rounded-r-sm"
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
