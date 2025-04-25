import OrderRow from "./OrderRow";
import OrderIndex from "./OrderIndex";
import { mockOrderBookData } from "@/shared/libs/mocks/data";

const OrderBook = () => {
  // 최대값 계산 (매수/매도 모두 고려)
  const max = mockOrderBookData.reduce((acc, order) => {
    const maxVolume = Math.max(Number(order.askVolume), Number(order.bidVolume));
    return Math.max(acc, maxVolume);
  }, 0);

  // 현재가 계산 (중간 인덱스 활용)
  const currentPriceIndex = Math.floor(mockOrderBookData.length / 2);

  return (
    <div className="flex h-full flex-col overflow-hidden rounded-lg border border-gray-200 bg-white shadow-sm">
      <header className="flex items-center justify-between border-b border-gray-200 px-6 py-3">
        <span className="font-semibold text-gray-800">호가잔량</span>
      </header>

      <div className="relative flex-1 overflow-auto">
        <OrderIndex />
        <div className="divide-y divide-gray-100">
          {mockOrderBookData.map((order, index) => (
            <OrderRow
              key={order.price}
              price={order.price}
              askVolume={order.askVolume}
              bidVolume={order.bidVolume}
              max={max}
              isCurrentPrice={index === currentPriceIndex}
            />
          ))}
        </div>
      </div>

      <footer className="flex justify-between border-t border-gray-200 px-6 py-2 text-xs text-gray-500">
        <span>
          총 매수잔량:{" "}
          {mockOrderBookData.reduce((sum, item) => sum + item.bidVolume, 0).toLocaleString()}
        </span>
        <span>
          총 매도잔량:{" "}
          {mockOrderBookData.reduce((sum, item) => sum + item.askVolume, 0).toLocaleString()}
        </span>
      </footer>
    </div>
  );
};

export default OrderBook;
