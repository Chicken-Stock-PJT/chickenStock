import { useMemo, useCallback } from "react";
import { ChartData, ChartVisibleRange } from "../model/types";
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
  // 차트 데이터 메모이제이션 - 원본 데이터나 타입이 변경될 때만 재계산
  const chartData = useMemo(() => formatChartData(sampleData, chartType), [sampleData, chartType]);

  // 초기 표시 데이터 범위 설정
  const { visibleRange, handleRangeChange } = useChartRange(chartData, chartType);

  // 현재 보이는 범위의 가격 데이터 계산 메모이제이션
  const visibleDataStats = useMemo(
    () => calculateVisibleDataStats(chartData, visibleRange),
    [chartData, visibleRange],
  );

  // 범위 변경 핸들러 메모이제이션
  const memoizedRangeChangeHandler = useCallback(
    (range: ChartVisibleRange) => handleRangeChange(range),
    [handleRangeChange],
  );

  return (
    <div className="w-full rounded-lg bg-gray-50 p-4">
      <div>
        <CandleStickChart
          chartData={chartData}
          visibleRange={visibleRange}
          visibleDataStats={visibleDataStats}
          chartType={chartType}
          onRangeChange={memoizedRangeChangeHandler}
        />
        <VolumeChart
          data={chartData}
          visibleRange={visibleRange}
          visibleDataStats={visibleDataStats}
          chartType={chartType}
          onRangeChange={memoizedRangeChangeHandler}
        />
      </div>
    </div>
  );
};

export default StockChart;
