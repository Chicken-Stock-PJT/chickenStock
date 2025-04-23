import OrderRow from "./OrderRow";
import { mockOrderBookData } from "@/shared/libs/mocks/data";
const OrderBook = () => {
  return (
    <div className="w-full border rounded-lg bg-white p-4 flex flex-col gap-4">
      <div className="flex items-center justify-between mb-4">
        <span className="font-bold text-gray-800">호가잔량</span>
      </div>
      <div className="flex flex-col gap-4 items-center justify-center">
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
