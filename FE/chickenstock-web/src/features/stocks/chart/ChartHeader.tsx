const ChartHeader = () => {
  return (
    <div className="w-full flex items-center justify-start gap-4">
      <div className="text-left">
        <h1 className="text-xl font-bold text-gray-800">셀트리온</h1>
        <p className="text-gray-600">stockCode | 068270</p>
      </div>
      <div className="flex items-end h-full gap-2">
        <div>가격</div>
        <div>전일대비</div>
        <div>변동률</div>
      </div>
    </div>
  );
};

export default ChartHeader;
