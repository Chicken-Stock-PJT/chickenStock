const OrderIndex = () => {
  return (
    <div className="grid grid-cols-3 border-b border-gray-200 bg-white py-2 text-sm font-bold text-gray-500">
      <span className="pr-6 text-right">매수호가잔량</span>
      <span className="text-center">호가</span>
      <span className="pl-6 text-left">매도호가잔량</span>
    </div>
  );
};

export default OrderIndex;
