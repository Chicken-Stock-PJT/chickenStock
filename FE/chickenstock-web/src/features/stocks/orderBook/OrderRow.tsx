interface OrderRowProps {
  price: number;
  askVolume: number;
  bidVolume: number;
}

const OrderRow = ({ price, askVolume, bidVolume }: OrderRowProps) => {
  return (
    <div className="flex w-full text-center text-sm font-bold text-gray-500 mx-auto justify-between items-center">
      <span className="px-4">{askVolume}</span>
      <span className="px-4">{price}</span>
      <span className="px-4">{bidVolume}</span>
    </div>
  );
};
export default OrderRow;
