// hooks.ts

import { useEffect, useMemo } from "react";
import { useState } from "react";
import { CHART_COLORS, CHART_MARGIN } from "./constants";
import {
  ChartData,
  ChartDataStats,
  ChartType,
  ChartVisibleRange,
  FormattedChartData,
} from "./types";
import { formatVolume } from "@/shared/libs/hooks/numberFormatters";
import { renderCandlestickTooltip, renderVolumeTooltip } from "../ui/ChartTooltips";

// 데이터 포맷팅
export const formatChartData = (data: ChartData[], chartType: "DAILY" | "YEARLY" | "MINUTE") => {
  // 시간 간격 일정하게 하기 위해 인덱스 기반 접근
  const formattedData = data
    .map((item) => {
      // 날짜/시간 표시용 텍스트 포맷팅
      let timeLabel;
      if (chartType === "DAILY" || chartType === "YEARLY") {
        const year = item.date.substring(0, 4);
        const month = item.date.substring(4, 6);
        const day = item.date.substring(6, 8);
        timeLabel = `${year}-${month}-${day}`;
      } else if (chartType === "MINUTE") {
        const year = item.date.substring(0, 4);
        const month = item.date.substring(4, 6);
        const day = item.date.substring(6, 8);
        const hour = item.date.substring(8, 10);
        const minute = item.date.substring(10, 12);
        timeLabel = `${year}-${month}-${day} ${hour}:${minute}`;
      } else {
        timeLabel = item.date;
      }

      return {
        // 원본 날짜 정보 저장
        originalDate: item.date,
        // 카테고리형 x축을 위한 표시 라벨
        timeLabel: timeLabel,
        // 차트 데이터
        y: [
          parseFloat(Math.abs(Number(item.openPrice)).toString()),
          parseFloat(Math.abs(Number(item.highPrice)).toString()),
          parseFloat(Math.abs(Number(item.lowPrice)).toString()),
          parseFloat(Math.abs(Number(item.currentPrice)).toString()),
        ],
        // 거래량
        volume: parseFloat(item.volume),
      };
    })
    .reverse(); // 시간순으로 정렬

  return formattedData;
};

export function getInitialRange(chartData: FormattedChartData[], chartType: ChartType) {
  let initialDataCount;

  if (chartType === "MINUTE") {
    initialDataCount = Math.min(chartData.length, 60); // 60개 분봉 (약 1시간)
  } else if (chartType === "DAILY") {
    initialDataCount = Math.min(chartData.length, 30); // 30일
  } else {
    initialDataCount = chartData.length; // 12개월
  }

  const startIndex = Math.max(0, chartData.length - initialDataCount);

  return {
    min: startIndex,
    max: chartData.length - 1,
  };
}

// 차트 타입에 따라 날짜 포매팅
export function formatDateByChartType(originalDate: string, chartType: ChartType): string {
  if (chartType === "MINUTE") {
    // 분봉 차트는 시:분만 표시
    const hour = originalDate.substring(8, 10);
    const minute = originalDate.substring(10, 12);
    return `${hour}:${minute}`;
  } else {
    // 일봉/연봉 차트는 월-일 표시
    const month = originalDate.substring(4, 6);
    const day = originalDate.substring(6, 8);
    return `${month}-${day}`;
  }
}

// 차트 날짜를 상세 라벨로 포맷팅
export function formatTimeLabel(originalDate: string, chartType: ChartType): string {
  if (chartType === "MINUTE") {
    const year = originalDate.substring(0, 4);
    const month = originalDate.substring(4, 6);
    const day = originalDate.substring(6, 8);
    const hour = originalDate.substring(8, 10);
    const minute = originalDate.substring(10, 12);
    return `${year}-${month}-${day} ${hour}:${minute}`;
  } else if (chartType === "YEARLY") {
    const year = originalDate.substring(0, 4);
    const month = originalDate.substring(4, 6);
    return `${year}-${month}`;
  } else {
    const year = originalDate.substring(0, 4);
    const month = originalDate.substring(4, 6);
    const day = originalDate.substring(6, 8);
    return `${year}-${month}-${day}`;
  }
}

/**
 * 현재 보이는 범위의 데이터 통계 계산
 */
export function calculateVisibleDataStats(
  chartData: FormattedChartData[],
  visibleRange: ChartVisibleRange,
): ChartDataStats {
  const visibleData = chartData.slice(
    Math.max(0, Math.floor(visibleRange.min)),
    Math.min(chartData.length, Math.ceil(visibleRange.max) + 1),
  );

  if (visibleData.length === 0) {
    return { minPrice: 0, maxPrice: 0, minVolume: 0, maxVolume: 0 };
  }

  // 가격 최소/최대값 계산
  let minPrice = Infinity;
  let maxPrice = -Infinity;
  let minVolume = Infinity;
  let maxVolume = -Infinity;

  visibleData.forEach((item) => {
    // 최저가보다 작은 값이 있으면 업데이트
    if (item.y[2] < minPrice) minPrice = item.y[2];
    // 최고가보다 큰 값이 있으면 업데이트
    if (item.y[1] > maxPrice) maxPrice = item.y[1];
    // 거래량 최소/최대값 업데이트
    if (item.volume < minVolume) minVolume = item.volume;
    if (item.volume > maxVolume) maxVolume = item.volume;
  });

  // 마진 추가 (위아래로 5%)
  const priceRange = maxPrice - minPrice;
  const volumeRange = maxVolume - minVolume;

  return {
    minPrice: Math.max(0, minPrice - priceRange * CHART_MARGIN),
    maxPrice: maxPrice + priceRange * CHART_MARGIN,
    minVolume: Math.max(0, minVolume - volumeRange * CHART_MARGIN),
    maxVolume: maxVolume + volumeRange * CHART_MARGIN,
  };
}

/**
 * 캔들스틱 차트용 시리즈 데이터 생성
 */
export function createCandlestickSeries(chartData: FormattedChartData[]) {
  return [
    {
      name: "주가",
      data: chartData.map((item, index) => ({
        x: index, // 인덱스를 x값으로 사용
        y: item.y,
      })),
    },
  ];
}

/**
 * 거래량 차트용 시리즈 데이터 생성
 */
export function createVolumeSeries(chartData: FormattedChartData[]) {
  return [
    {
      name: "거래량",
      data: chartData.map((item, index) => ({
        x: index, // 인덱스를 x값으로 사용
        y: item.volume,
      })),
    },
  ];
}

export function useChartRange(chartData: FormattedChartData[], chartType: ChartType) {
  // 초기 표시 데이터 범위 설정
  const [visibleRange, setVisibleRange] = useState<ChartVisibleRange>(() =>
    getInitialRange(chartData, chartType),
  );

  // 차트 타입이 변경될 때 범위 재설정
  useEffect(() => {
    setVisibleRange(getInitialRange(chartData, chartType));
  }, [chartType, chartData]);

  // 리셋 버튼 이벤트 설정
  useEffect(() => {
    const resetButtonElement = document.querySelector(".apexcharts-reset-icon");

    const handleReset = () => {
      setVisibleRange(getInitialRange(chartData, chartType));
    };

    if (resetButtonElement) {
      resetButtonElement.addEventListener("click", handleReset);
    }

    return () => {
      if (resetButtonElement) {
        resetButtonElement.removeEventListener("click", handleReset);
      }
    };
  }, [chartType, chartData]);

  // 줌 및 스크롤 이벤트 핸들러
  const handleRangeChange = (range: ChartVisibleRange) => {
    setVisibleRange(range);
  };

  return { visibleRange, setVisibleRange, handleRangeChange };
}

export function useCandlestickChartOptions(
  chartData: FormattedChartData[],
  visibleRange: ChartVisibleRange,
  visibleDataStats: ChartDataStats,
  chartType: ChartType,
  onRangeChange: (range: ChartVisibleRange) => void,
) {
  return useMemo(
    () => ({
      chart: {
        type: "candlestick" as const,
        height: "100%",
        id: "candles",
        toolbar: {
          tools: {
            download: false,
            selection: true,
            zoom: true,
            zoomin: true,
            zoomout: true,
            pan: true,
            reset: true,
          },
          autoSelected: "pan" as const,
          show: true,
        },
        zoom: {
          enabled: true,
        },
        animations: {
          enabled: true,
          animateGradually: {
            enabled: true,
          },
          dynamicAnimation: {
            enabled: false,
          },
        },
        events: {
          zoomed: function (
            _chartContext: ApexChart,
            { xaxis }: { xaxis: { min: number; max: number } },
          ) {
            if (xaxis) {
              onRangeChange({
                min: Math.floor(xaxis.min),
                max: Math.ceil(xaxis.max),
              });
            }
          },
          scrolled: function (
            _chartContext: ApexChart,
            { xaxis }: { xaxis: { min: number; max: number } },
          ) {
            if (xaxis) {
              onRangeChange({
                min: Math.floor(xaxis.min),
                max: Math.ceil(xaxis.max),
              });
            }
          },
        },
      },
      xaxis: {
        type: "category" as const,
        labels: {
          formatter: function (val: string) {
            const item = chartData[Math.floor(Number(val))];
            if (!item) return "";
            return formatDateByChartType(item.originalDate, chartType);
          },
        },
        axisBorder: {
          show: true,
        },
        axisTicks: {
          show: true,
        },
        min: visibleRange.min,
        max: visibleRange.max,
        tickAmount: Math.min(10, visibleRange.max - visibleRange.min + 1),
      },
      yaxis: {
        tooltip: {
          enabled: true,
        },
        forceNiceScale: true,
        min: visibleDataStats.minPrice,
        max: visibleDataStats.maxPrice,
        labels: {
          formatter: (value: number) => {
            return value.toLocaleString();
          },
        },
      },
      tooltip: {
        custom: function ({ dataPointIndex }: { dataPointIndex: number }) {
          return renderCandlestickTooltip({ dataPointIndex, chartData });
        },
      },
      grid: {
        borderColor: CHART_COLORS.GRID,
        xaxis: {
          lines: {
            show: true,
          },
        },
      },
      plotOptions: {
        candlestick: {
          colors: {
            upward: CHART_COLORS.UPWARD,
            downward: CHART_COLORS.DOWNWARD,
          },
        },
      },
    }),
    [chartData, visibleRange, visibleDataStats, chartType, onRangeChange],
  );
}

/**
 * 거래량 차트 옵션 생성 훅
 */
export function useVolumeChartOptions(
  chartData: FormattedChartData[],
  visibleRange: ChartVisibleRange,
  visibleDataStats: ChartDataStats,
  chartType: ChartType,
  onRangeChange: (range: ChartVisibleRange) => void,
) {
  return useMemo(
    () => ({
      chart: {
        height: "100%",
        type: "bar" as const,
        id: "volume-chart",
        toolbar: {
          tools: {
            download: false,
            selection: true,
            zoom: true,
            zoomin: true,
            zoomout: true,
            pan: true,
            reset: true,
          },
          autoSelected: "pan" as const,
          show: false,
        },
        zoom: {
          enabled: true,
        },
        animations: {
          enabled: true,
          animateGradually: {
            enabled: true,
          },
          dynamicAnimation: {
            enabled: false,
          },
        },
        events: {
          zoomed: function (
            _chartContext: ApexChart,
            { xaxis }: { xaxis: { min: number; max: number } },
          ) {
            if (xaxis) {
              onRangeChange({
                min: Math.floor(xaxis.min),
                max: Math.ceil(xaxis.max),
              });
            }
          },
          scrolled: function (
            _chartContext: ApexChart,
            { xaxis }: { xaxis: { min: number; max: number } },
          ) {
            if (xaxis) {
              onRangeChange({
                min: Math.floor(xaxis.min),
                max: Math.ceil(xaxis.max),
              });
            }
          },
        },
      },
      dataLabels: {
        enabled: false,
      },
      plotOptions: {
        bar: {
          columnWidth: "80%",
          colors: {
            ranges: [
              {
                from: -1000,
                to: 0,
                color: "#4170FD",
              },
              {
                from: 0,
                to: 1000,
                color: "#FD4141",
              },
            ],
          },
        },
      },
      stroke: {
        width: 0,
      },
      xaxis: {
        type: "category" as const,
        labels: {
          formatter: function (val: string) {
            const item = chartData[Math.floor(Number(val))];
            if (!item) return "";
            return formatDateByChartType(item.originalDate, chartType);
          },
        },
        axisBorder: {
          offsetX: 13,
        },
        min: visibleRange.min,
        max: visibleRange.max,
        tickAmount: Math.min(10, visibleRange.max - visibleRange.min + 1),
      },
      yaxis: {
        min: visibleDataStats.minVolume,
        max: visibleDataStats.maxVolume,
        forceNiceScale: true,
        labels: {
          show: true,
          formatter: formatVolume,
        },
      },
      tooltip: {
        enabled: true,
        custom: function ({ dataPointIndex }: { dataPointIndex: number }) {
          return renderVolumeTooltip({ dataPointIndex, chartData });
        },
      },
    }),
    [chartData, visibleRange, visibleDataStats, chartType, onRangeChange],
  );
}
