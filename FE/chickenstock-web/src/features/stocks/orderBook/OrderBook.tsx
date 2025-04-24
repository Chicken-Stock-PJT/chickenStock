import OrderRow from "./OrderRow";
import { mockOrderBookData } from "@/shared/libs/mocks/data";
const OrderBook = () => {
  return (
    <div className="flex w-full flex-col gap-4 rounded-lg border bg-white p-4">
      <div className="mb-4 flex items-center justify-between">
        <span className="font-bold text-gray-800">호가잔량</span>
      </div>
      <div className="flex flex-col items-center justify-center gap-4">
        {mockOrderBookData.map((order) => (
          <OrderRow
            key={order.price}
            price={order.price}
            askVolume={order.askVolume}
            bidVolume={order.bidVolume}
          />
        ))}
      </div>
    </div>
  );
};

export default OrderBook;
