const StockListSkeleton = () => {
  return (
    <div className="animate-pulse">
      {[...Array(10)].map((_, index) => (
        <div
          key={index}
          className="flex items-center justify-between p-4 border-b"
        >
          <div className="flex items-center gap-6 w-1/4">
            <div className="w-6 h-6 bg-gray-200 rounded-full"></div>
            <div className="space-y-2">
              <div className="h-4 w-24 bg-gray-200 rounded"></div>
              <div className="h-3 w-16 bg-gray-200 rounded"></div>
            </div>
          </div>
          <div className="w-1/6">
            <div className="h-4 w-20 bg-gray-200 rounded ml-auto"></div>
          </div>
          <div className="w-1/6">
            <div className="h-4 w-16 bg-gray-200 rounded ml-auto"></div>
          </div>
          <div className="w-1/6">
            <div className="h-4 w-14 bg-gray-200 rounded ml-auto"></div>
          </div>
          <div className="w-1/6">
            <div className="h-4 w-20 bg-gray-200 rounded ml-auto"></div>
          </div>
          <div className="w-1/6">
            <div className="h-4 w-24 bg-gray-200 rounded ml-auto"></div>
          </div>
        </div>
      ))}
    </div>
  );
};

export default StockListSkeleton;
