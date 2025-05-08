import ReactApexChart from "react-apexcharts";
import { createVolumeSeries, useVolumeChartOptions } from "../model/hooks";
import { FormattedChartData, ChartVisibleRange, ChartDataStats, ChartType } from "../model/types";

/**
 * 거래량 차트 컴포넌트
 */

interface VolumeChartProps {
  data: FormattedChartData[];
  visibleRange: ChartVisibleRange;
  visibleDataStats: ChartDataStats;
  chartType: ChartType;
  onRangeChange: (range: ChartVisibleRange) => void;
}

const VolumeChart = ({
  data,
  visibleRange,
  visibleDataStats,
  chartType,
  onRangeChange,
}: VolumeChartProps) => {
  // 차트 옵션 생성
  const options = useVolumeChartOptions(
    data,
    visibleRange,
    visibleDataStats,
    chartType,
    onRangeChange,
  );

  // 시리즈 데이터 생성
  const series = createVolumeSeries(data);

  return (
    <div className="h-[150px]">
      <ReactApexChart options={options} series={series} type="bar" height="100%" />
    </div>
  );
};

export default VolumeChart;
