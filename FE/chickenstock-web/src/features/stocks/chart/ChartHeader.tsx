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
  // const openPrice = Number(chartData.openPrice).toLocaleString();
  // const highPrice = Number(chartData.highPrice).toLocaleString();
  // const lowPrice = Number(chartData.lowPrice).toLocaleString();

  return (
    <div className="flex items-center gap-4">
      <img
        className="w-[78px] rounded-2xl"
        src={`https://thumb.tossinvest.com/image/resized/96x0/https%3A%2F%2Fstatic.toss.im%2Fpng-icons%2Fsecurities%2Ficn-sec-fill-${stockCode}.png`}
        alt=""
      />
      <div className="flex flex-col gap-2">
        <div className="flex items-end gap-2 text-left">
          <h1 className="text-xl font-bold text-gray-800">셀트리온</h1>
          <p className="text-gray-600">{stockCode}</p>
        </div>
        <div className="flex gap-2 text-left text-chart-blue">
          <p className="text-3xl font-semibold leading-none">{currentPrice}</p>
          <div className="flex items-end items-center gap-2">
            {/* <span className="text-lg leading-none">▲ 5,230</span> */}
            <span className="text-lg leading-none">▼ 1400</span>
            <span className="leading-none text-opacity-25">(-0.88%)</span>
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
    </div>
  );
};

export default ChartHeader;
