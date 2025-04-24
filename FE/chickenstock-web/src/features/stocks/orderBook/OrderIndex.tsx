const OrderIndex = () => {
  return (
    <div className="sticky top-0 bg-white z-20 py-2 border-b border-gray-200 grid grid-cols-3 text-sm font-bold text-gray-500">
      <span className="text-right pr-6">매수호가잔량</span>
      <span className="text-center">호가</span>
      <span className="text-left pl-6">매도호가잔량</span>
    </div>
  );
};

export default OrderIndex;