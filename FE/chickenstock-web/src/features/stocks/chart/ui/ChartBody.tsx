import { useMemo } from "react";
import { ChartData } from "../model/types";
import { calculateVisibleDataStats, formatChartData, useChartRange } from "../model/hooks";
import CandleStickChart from "./CandleStickChart";
import VolumeChart from "./VolumeChart";

const StockChart = ({
  sampleData,
  chartType,
}: {
  sampleData: ChartData[];
  chartType: "DAILY" | "YEARLY" | "MINUTE";
}) => {
  const chartData = useMemo(() => formatChartData(sampleData, chartType), [sampleData, chartType]);

  // 초기 표시 데이터 범위 설정
  const { visibleRange, handleRangeChange } = useChartRange(chartData, chartType);

  // 현재 보이는 범위의 가격 데이터 계산
  const visibleDataStats = useMemo(
    () => calculateVisibleDataStats(chartData, visibleRange),
    [chartData, visibleRange],
  );

  return (
    <div className="w-full rounded-lg bg-gray-50 p-4">
      <div>
        <CandleStickChart
          chartData={chartData}
          visibleRange={visibleRange}
          visibleDataStats={visibleDataStats}
          chartType={chartType}
          onRangeChange={handleRangeChange}
        />
        <VolumeChart
          data={chartData}
          visibleRange={visibleRange}
          visibleDataStats={visibleDataStats}
          chartType={chartType}
          onRangeChange={handleRangeChange}
        />
      </div>
    </div>
  );
};

export default StockChart;
