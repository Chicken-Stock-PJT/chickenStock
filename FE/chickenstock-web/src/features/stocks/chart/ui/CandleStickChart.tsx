import ReactApexChart from "react-apexcharts";
import { createCandlestickSeries, useCandlestickChartOptions } from "../model/hooks";
import { FormattedChartData, ChartVisibleRange, ChartDataStats, ChartType } from "../model/types";

interface CandleStickChartProps {
  chartData: FormattedChartData[];
  visibleRange: ChartVisibleRange;
  visibleDataStats: ChartDataStats;
  chartType: ChartType;
  onRangeChange: (range: ChartVisibleRange) => void;
}

const CandleStickChart = ({
  chartData,
  visibleRange,
  visibleDataStats,
  chartType,
  onRangeChange,
}: CandleStickChartProps) => {
  const options = useCandlestickChartOptions(
    chartData,
    visibleRange,
    visibleDataStats,
    chartType,
    onRangeChange,
  );
  const series = createCandlestickSeries(chartData);

  return (
    <div className="mb-4 h-[200px]">
      <ReactApexChart options={options} series={series} type="candlestick" height="100%" />
    </div>
  );
};

export default CandleStickChart;
