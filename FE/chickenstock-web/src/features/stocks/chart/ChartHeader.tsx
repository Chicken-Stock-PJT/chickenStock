interface ChartHeaderProps {
  stockCode: string;
  chartData?: {
    currentPrice: string;
    openPrice: string;
    highPrice: string;
    lowPrice: string;
  };
}

const ChartHeader = ({ stockCode, chartData }: ChartHeaderProps) => {
  if (!chartData) return null;

  const currentPrice = Number(chartData.currentPrice).toLocaleString();
  const openPrice = Number(chartData.openPrice).toLocaleString();
  const highPrice = Number(chartData.highPrice).toLocaleString();
  const lowPrice = Number(chartData.lowPrice).toLocaleString();

  return (
    <div className="flex flex-col gap-2">
      <div className="text-left flex items-end gap-2">
        <h1 className="text-xl font-bold text-gray-800">셀트리온</h1>
        <p className="text-gray-600">{stockCode}</p>
      </div>
      <div className="text-left flex gap-2">
        <p className="text-3xl font-semibold leading-none">{currentPrice}</p>
        <div className="flex items-end gap-2 items-center">
          <span className="text-lg leading-none">▲ 5,230</span>
          <span className="text-gray-500 leading-none">(+0.00%)</span>
        </div>
        {/* <div className="flex flex-col">
          <span className="text-sm text-gray-500">시가</span>
          <span>{openPrice}</span>
        </div>
        <div className="flex flex-col">
          <span className="text-sm text-gray-500">고가</span>
          <span>{highPrice}</span>
        </div>
        <div className="flex flex-col">
          <span className="text-sm text-gray-500">저가</span>
          <span>{lowPrice}</span>
        </div> */}
      </div>
    </div>
  );
};

export default ChartHeader;
