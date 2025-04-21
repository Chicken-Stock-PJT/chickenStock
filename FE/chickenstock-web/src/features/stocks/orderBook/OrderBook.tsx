import OrderRow from "./OrderRow";

const OrderBook = () => {
  return (
    <div className="w-full border rounded-lg bg-gray-100 p-4 flex flex-col gap-4">
      <h1 className="text-3xl font-bold text-gray-800">Order</h1>
      
      <p>Order book content goes here.</p>
      <div className="flex flex-col gap-4 items-center justify-center">
        <OrderRow />
      </div>
    </div>
  );
};

export default OrderBook;
