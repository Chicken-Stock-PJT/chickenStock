const StockListSkeleton = () => {
  return (
    <div className="animate-pulse">
      {Array.from({ length: 10 }).map((_, index) => (
        <div key={index} className="flex items-center justify-between border-b p-4">
          <div className="flex w-1/4 items-center gap-6">
            <div className="size-6 rounded-full bg-gray-200"></div>
            <div className="space-y-2">
              <div className="h-4 w-24 rounded bg-gray-200"></div>
              <div className="h-3 w-16 rounded bg-gray-200"></div>
            </div>
          </div>
          <div className="w-1/6">
            <div className="ml-auto h-4 w-20 rounded bg-gray-200"></div>
          </div>
          <div className="w-1/6">
            <div className="ml-auto h-4 w-16 rounded bg-gray-200"></div>
          </div>
          <div className="w-1/6">
            <div className="ml-auto h-4 w-14 rounded bg-gray-200"></div>
          </div>
          <div className="w-1/6">
            <div className="ml-auto h-4 w-20 rounded bg-gray-200"></div>
          </div>
          <div className="w-1/6">
            <div className="ml-auto h-4 w-24 rounded bg-gray-200"></div>
          </div>
        </div>
      ))}
    </div>
  );
};

export default StockListSkeleton;
