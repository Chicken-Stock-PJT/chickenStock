import { useEffect, useState, useCallback, useMemo, useRef } from "react";
import ChartHeader from "./ChartHeader";
import { ChartData, ChartType } from "../model/types";
import { getStockChartData } from "../api";
import { useWebSocketStore } from "@/shared/store/websocket";
import { createChart, CandlestickSeries, HistogramSeries } from "lightweight-charts";
import { formatChartTime } from "@/features/stocks/chart/model/hooks";

interface ChartProps {
  stockName?: string;
  stockCode?: string;
  priceData?: {
    currentPrice: string;
    priceChange: string;
    changeRate: string;
  };
}

const Chart = ({ stockName = "삼성전자", stockCode = "005930", priceData }: ChartProps) => {
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [chartData, setChartData] = useState<ChartData[]>([]);
  const [chartType, setChartType] = useState<"MINUTE" | "DAILY" | "YEARLY">("DAILY");
  const { stockPriceData } = useWebSocketStore();

  // 차트 관련 refs - 항상 선언 (조건부로 선언하지 않음)
  const chartContainerRef = useRef(null);
  const chartInstanceRef = useRef(null);

  // 최근 받은 웹소켓 데이터를 참조로 저장
  const latestPriceRef = useRef<{
    currentPrice: string;
    timestamp: number;
  } | null>(null);

  // 업데이트 디바운싱을 위한 타이머 참조
  const updateTimerRef = useRef<number | null>(null);

  const fetchChartData = useCallback(async () => {
    try {
      setLoading(true);
      const data = await getStockChartData({
        stockCode,
        chartType,
        hasNext: false,
        nextKey: "",
      });
      setChartData(
        data.chartData
          .map((item) => ({
            ...item,
            date: formatChartTime(item.date, chartType),
          }))
          .reverse(),
      );
    } catch (err) {
      setError(err instanceof Error ? err.message : "An error occurred");
    } finally {
      setLoading(false);
    }
  }, [stockCode, chartType]);

  // 초기 차트 데이터 로드
  useEffect(() => {
    void fetchChartData();

    // 컴포넌트 언마운트 시 타이머 정리
    return () => {
      if (updateTimerRef.current !== null) {
        window.clearTimeout(updateTimerRef.current);
      }
    };
  }, [fetchChartData]);

  // 효율적인 차트 데이터 업데이트 함수
  const updateChartData = useCallback(() => {
    if (!latestPriceRef.current || chartData.length === 0) return;

    const newPrice = Number(latestPriceRef.current?.currentPrice);
    const lastData = chartData[0];

    // 실제로 가격이 변경되었고 유효한 가격일 때만 업데이트
    if (Number(lastData.currentPrice) !== newPrice && !isNaN(newPrice)) {
      setChartData((prevData) => {
        const updatedData = [...prevData];
        updatedData[0] = {
          ...lastData,
          currentPrice: latestPriceRef.current!.currentPrice,
          highPrice: Math.max(Number(lastData.highPrice), newPrice).toString(),
          lowPrice: Math.min(Number(lastData.lowPrice), newPrice).toString(),
        };
        return updatedData;
      });
    }

    // 타이머 참조 초기화
    updateTimerRef.current = null;
  }, [chartData]);

  // 웹소켓 데이터 업데이트 처리 - 디바운싱 적용
  useEffect(() => {
    if (!stockPriceData || chartData.length === 0) return;

    // 최신 가격 정보 참조 업데이트
    latestPriceRef.current = {
      currentPrice: stockPriceData.currentPrice,
      timestamp: Date.now(),
    };

    // 이미 예약된 업데이트가 있다면 새로 예약하지 않음
    if (updateTimerRef.current !== null) return;

    // 100ms 디바운스로 차트 업데이트 예약
    updateTimerRef.current = window.setTimeout(updateChartData, 100);
  }, [stockPriceData, updateChartData, chartData.length]);

  const handleChartTypeChange = useCallback((type: ChartType) => {
    setChartType(type);
    setChartData([]); // 차트 타입 변경 시 데이터 초기화
  }, []);

  // 메모이제이션된 차트 데이터
  const memoizedChartData = useMemo(() => chartData, [chartData]);

  // 차트 생성 및 설정 - 의존성 배열에 memoizedChartData 추가
  useEffect(() => {
    // 데이터가 없거나 로딩 중이면 차트를 생성하지 않음
    if (!memoizedChartData.length || loading || !chartContainerRef.current) return;

    // 기존 차트 인스턴스가 있다면 제거
    if (chartInstanceRef.current) {
      chartInstanceRef.current.remove();
      chartInstanceRef.current = null;
    }

    // 차트 인스턴스 생성
    const chart = createChart(chartContainerRef.current, {
      localization: {
        locale: "ko",
      },
      width: chartContainerRef.current.clientWidth,
      height: 500,
      layout: {
        background: { color: "#ffffff" },
        textColor: "#333333",
      },
      grid: {
        vertLines: { color: "rgba(197, 203, 206, 0.5)" },
        horzLines: { color: "rgba(197, 203, 206, 0.5)" },
      },
      crosshair: {
        mode: 0, // 0 = normal crosshair
      },
      timeScale: {
        borderColor: "rgba(197, 203, 206, 0.8)",
        timeVisible: true,
        secondsVisible: false,
        barSpacing: 12,
      },
    });

    // 캔들스틱 시리즈 생성
    const candlestickSeries = chart.addSeries(CandlestickSeries, {
      upColor: "#FD4141",
      downColor: "#4170FD",
      borderVisible: false,
      wickUpColor: "#FD4141",
      wickDownColor: "#4170FD",
      priceScaleId: "right", // 오른쪽 가격 스케일 사용
    });

    // 캔들스틱 시리즈의 오른쪽 가격 스케일 설정
    chart.priceScale("right").applyOptions({
      borderColor: "rgba(197, 203, 206, 0.8)",
      visible: true,
      autoScale: true, // 자동 스케일링 활성화
      scaleMargins: {
        top: 0.1, // 상단 여백 10%
        bottom: 0.3, // 하단 여백 30% (거래량 영역을 위한 공간)
      },
    });

    // 거래량 시리즈 생성 (별도의 왼쪽 가격 스케일 사용)
    const volumeSeries = chart.addSeries(HistogramSeries, {
      priceFormat: {
        type: "volume",
      },
      priceScaleId: "volume", // 별도의 가격 스케일 ID
    });

    // 거래량 시리즈의 가격 스케일 설정
    chart.priceScale("volume").applyOptions({
      visible: true,
      autoScale: true,
      scaleMargins: {
        top: 0.7, // 상단 70% 이후에 표시 (하단 30%에 표시)
        bottom: 0,
      },
    });

    const candleData = memoizedChartData.map((item: ChartData) => ({
      time: item.date,
      open: Number(item.openPrice),
      high: Number(item.highPrice),
      low: Number(item.lowPrice),
      close: Number(item.currentPrice),
    }));

    const volumeData = memoizedChartData.map((item: ChartData) => ({
      time: item.date,
      value: Number(item.volume),
    }));

    // 캔들스틱 데이터에 색상 정보 추가하여 볼륨 데이터 생성
    const coloredVolumeData = volumeData.map((item) => {
      // 해당 날짜의 캔들스틱 데이터 찾기
      const candle = candleData.find((c) => c.time === item.time);

      // 캔들스틱 상승/하락 여부에 따라 색상 지정
      let color = "rgba(0, 0, 0, 0.5)"; // 기본 색상

      if (candle) {
        color =
          candle.close >= candle.open
            ? "#FD4141" // 상승 (빨강)
            : "#4170FD"; // 하락 (파랑)
      }

      return {
        ...item,
        color,
      };
    });

    // 데이터 설정
    candlestickSeries.setData(candleData);
    volumeSeries.setData(coloredVolumeData);

    // 차트의 가시 영역을 데이터에 맞게 조정
    chart.timeScale().fitContent();

    // 창 크기 변경에 대응
    const handleResize = () => {
      if (chartContainerRef.current && chartInstanceRef.current) {
        chart.applyOptions({
          width: chartContainerRef.current.clientWidth,
        });
      }
    };

    window.addEventListener("resize", handleResize);
    chartInstanceRef.current = chart;

    // 컴포넌트 언마운트 시 정리
    return () => {
      window.removeEventListener("resize", handleResize);
      if (chartInstanceRef.current) {
        chartInstanceRef.current.remove();
        chartInstanceRef.current = null;
      }
    };
  }, [memoizedChartData, loading]);

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error}</div>;
  if (!memoizedChartData.length) return null;

  return (
    <div className="flex w-full flex-col gap-4 rounded-lg border border-gray-200 bg-white p-4">
      <ChartHeader
        stockName={stockName}
        stockCode={stockCode}
        currentPrice={priceData?.currentPrice ?? "0"}
        priceChange={priceData?.priceChange ?? "0"}
        changeRate={priceData?.changeRate ?? "0"}
        onChartTypeChange={handleChartTypeChange}
        selectedChartType={chartType}
      />
      <div ref={chartContainerRef} style={{ width: "100%", height: "500px" }} />
    </div>
  );
};

export default Chart;
