interface OrderRowProps {
  price: number;
  askVolume: number;
  bidVolume: number;
}

const OrderRow = ({ price, askVolume, bidVolume }: OrderRowProps) => {
  return (
    <div
      className={
        askVolume
          ? "mx-auto flex w-full items-center justify-between text-center text-chart-blue text-gray-500"
          : "mx-auto flex w-full items-center justify-between text-center text-chart-red text-gray-500"
      }
    >
      <span className="flex-1 bg-chart-blue bg-opacity-[50%] px-4 text-chart-blue">
        {askVolume ? askVolume : ""}
      </span>
      <span className="px-4">{price}</span>
      <span className="flex-1 bg-chart-red bg-opacity-[50%] px-4 text-chart-red">
        {bidVolume ? bidVolume : ""}
      </span>
    </div>
  );
};
export default OrderRow;
