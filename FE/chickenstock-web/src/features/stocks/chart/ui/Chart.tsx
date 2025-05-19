import { useEffect, useState, useRef, useCallback } from "react";
import ChartHeader from "./ChartHeader";
import { ChartData, ChartType, TimeInterval } from "../model/types";
import { getStockChartData } from "../api";
import { useWebSocketStore } from "@/shared/store/websocket";
import {
  createChart,
  CandlestickSeries,
  HistogramSeries,
  ISeriesApi,
  IChartApi,
  Time,
  CandlestickData,
  HistogramData,
  MouseEventParams,
} from "lightweight-charts";
import { formatChartTime, updateTimestamp } from "@/features/stocks/chart/model/hooks";

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
  const [chartType, setChartType] = useState<ChartType>("DAILY");
  const [timeInterval, setTimeInterval] = useState<TimeInterval>("1");
  const [nextKey, setNextKey] = useState<string>("");
  const [isLoadingMore, setIsLoadingMore] = useState(false);
  const { stockPriceData, tradeExecutionData } = useWebSocketStore();
  const candlestickSeriesRef = useRef<ISeriesApi<"Candlestick"> | null>(null);
  const volumeSeriesRef = useRef<ISeriesApi<"Histogram"> | null>(null);
  const [prevVolume, setPrevVolume] = useState({ time: 0 as Time, value: 0, color: "" });

  // 차트 관련 refs - 항상 선언 (조건부로 선언하지 않음)
  const chartContainerRef = useRef<HTMLDivElement>(null);
  const chartInstanceRef = useRef<IChartApi | null>(null);
  const handleChartTypeChange = (type: ChartType) => {
    if (type === chartType) return;
    setChartType(type);
    setChartData([]); // 차트 타입 변경 시 데이터 초기화
  };

  const handleTimeIntervalChange = (interval: TimeInterval) => {
    setChartType("MINUTE");
    if (interval === timeInterval) return;
    setTimeInterval(interval);
    setChartData([]); // 시간 간격 변경 시 데이터 초기화
  };

  // 초기 차트 데이터 로드
  useEffect(() => {
    const fetchChartData = async () => {
      try {
        setLoading(true);
        const data = await getStockChartData({
          stockCode,
          chartType,
          hasNext: false,
          nextKey: "",
          timeInterval,
        });
        console.log(data.chartData);
        const processedData = data.chartData
          .map((item) => ({
            currentPrice: Math.abs(Number(item.currentPrice)).toString(),
            openPrice: Math.abs(Number(item.openPrice)).toString(),
            highPrice: Math.abs(Number(item.highPrice)).toString(),
            lowPrice: Math.abs(Number(item.lowPrice)).toString(),
            volume: Math.abs(Number(item.volume)).toString(),
            date: formatChartTime(item.date.toString()),
          }))
          .reverse(); // API 데이터가 내림차순이므로 오름차순으로 변환

        setChartData(processedData);
        setNextKey(data.nextKey);
        setPrevVolume({
          time: formatChartTime(data.chartData[0].date.toString()) as Time,
          value: Number(data.chartData[0].volume),
          color: "",
        });
      } catch (err) {
        setError(err instanceof Error ? err.message : "An error occurred");
      } finally {
        setLoading(false);
      }
    };
    void fetchChartData();
  }, [stockCode, chartType, timeInterval]);

  // 이전 데이터 로드 함수
  const loadMoreData = useCallback(async () => {
    if (isLoadingMore || !nextKey) return;

    try {
      setIsLoadingMore(true);

      // 현재 보이는 영역 저장
      const visibleRange = chartInstanceRef.current?.timeScale().getVisibleLogicalRange();
      const currentFrom = visibleRange?.from ?? 0;
      const currentTo = visibleRange?.to ?? 0;

      const data = await getStockChartData({
        stockCode,
        chartType,
        hasNext: true,
        nextKey,
        timeInterval,
      });

      const lastDate = chartData[chartData.length - 1].date;
      const firstDate = formatChartTime(data.chartData[0].date.toString());

      if (lastDate === firstDate) {
        setIsLoadingMore(false);
        return;
      }

      const newData = data.chartData
        .map((item) => ({
          currentPrice: Math.abs(Number(item.currentPrice)).toString(),
          openPrice: Math.abs(Number(item.openPrice)).toString(),
          highPrice: Math.abs(Number(item.highPrice)).toString(),
          lowPrice: Math.abs(Number(item.lowPrice)).toString(),
          volume: Math.abs(Number(item.volume)).toString(),
          date: formatChartTime(item.date.toString()),
        }))
        .reverse();

      setChartData((prev) => {
        const updatedData = [...newData, ...prev];

        // 데이터 업데이트 후 차트의 가시 영역 복원
        setTimeout(() => {
          if (chartInstanceRef.current) {
            const newFrom = currentFrom + newData.length;
            const newTo = currentTo + newData.length;
            chartInstanceRef.current.timeScale().setVisibleLogicalRange({
              from: newFrom,
              to: newTo,
            });
          }
        }, 0);

        return updatedData;
      });

      setNextKey(data.nextKey);
    } catch (err) {
      setError(err instanceof Error ? err.message : "An error occurred");
    } finally {
      setIsLoadingMore(false);
    }
  }, [stockCode, chartType, nextKey, chartData, timeInterval]);

  // stockPriceData 업데이트 시 차트 업데이트
  useEffect(() => {
    if (loading) return;
    if (stockPriceData && candlestickSeriesRef.current) {
      const lastCandle =
        candlestickSeriesRef.current.data()[candlestickSeriesRef.current.data().length - 1];
      const time = updateTimestamp(
        formatChartTime(stockPriceData.timestamp),
        Number(lastCandle.time),
        chartType,
        timeInterval,
      ) as Time;

      const newData = {
        time,
        open: Number(
          time === Number(lastCandle.time)
            ? (lastCandle as CandlestickData<Time>).open
            : (lastCandle as CandlestickData<Time>).close,
        ),
        high:
          time === Number(lastCandle.time)
            ? Math.max(
                Number((lastCandle as CandlestickData<Time>).high),
                Math.abs(Number(stockPriceData.currentPrice)),
              )
            : Math.abs(Number(stockPriceData.currentPrice)),
        low:
          time === Number(lastCandle.time)
            ? Math.min(
                Number((lastCandle as CandlestickData<Time>).low),
                Math.abs(Number(stockPriceData.currentPrice)),
              )
            : Math.abs(Number(stockPriceData.currentPrice)),
        close: Math.abs(Number(stockPriceData.currentPrice)),
      };
      // console.log(newData);
      candlestickSeriesRef.current.update(newData as CandlestickData<Time>);
    }
  }, [stockPriceData]);

  useEffect(() => {
    if (loading) return;
    if (tradeExecutionData && volumeSeriesRef.current && chartData.length > 0) {
      const lastCandle =
        candlestickSeriesRef.current?.data()[candlestickSeriesRef.current?.data().length - 1];
      const lastOpenPrice = Number((lastCandle as CandlestickData<Time>).open);
      const time = updateTimestamp(
        formatChartTime(tradeExecutionData.timestamp),
        Number(volumeSeriesRef.current.data()[0].time),
        chartType,
        timeInterval,
      ) as Time;

      const quantity = Number(tradeExecutionData.quantity);
      // console.log(time, prevVolume.time);
      const newData = {
        time: time,
        value: prevVolume.time === time ? prevVolume.value + quantity : quantity,
        color:
          Number(tradeExecutionData.price) >= lastOpenPrice
            ? "rgba(253, 65, 65, 0.5)"
            : "rgba(65, 112, 253, 0.5)",
      };
      setPrevVolume({ time: newData.time, value: newData.value, color: newData.color });
      volumeSeriesRef.current.update(newData as HistogramData<Time>);
    }
  }, [tradeExecutionData]);

  // 차트 생성 및 설정 - 의존성 배열에 memoizedChartData 추가
  useEffect(() => {
    // 데이터가 없거나 로딩 중이면 차트를 생성하지 않음
    if (!chartData.length || loading || !chartContainerRef.current) return;

    // 기존 차트 인스턴스가 있다면 제거
    if (chartInstanceRef.current) {
      chartInstanceRef.current.remove();
      chartInstanceRef.current = null;
    }

    // 차트 인스턴스 생성
    const chart = createChart(chartContainerRef.current, {
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
      priceFormat: {
        type: "price",
        minMove: 1,
      },
    });
    candlestickSeriesRef.current = candlestickSeries;

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
    volumeSeriesRef.current = volumeSeries;

    // 거래량 시리즈의 가격 스케일 설정
    chart.priceScale("volume").applyOptions({
      visible: true,
      autoScale: true,
      scaleMargins: {
        top: 0.7, // 상단 70% 이후에 표시 (하단 30%에 표시)
        bottom: 0,
      },
    });

    // console.log("chartData", chartData);

    // 주가 차트 데이터 설정
    const candleData = chartData.map((item: ChartData) => ({
      time: item.date,
      open: Number(item.openPrice),
      high: Number(item.highPrice),
      low: Number(item.lowPrice),
      close: Number(item.currentPrice),
    }));

    // 거래량 차트 데이터 설정
    const volumeData = chartData.map((item: ChartData) => ({
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
            ? "rgba(253, 65, 65, 0.5)" // 상승 (빨강)
            : "rgba(65, 112, 253, 0.5)"; // 하락 (파랑)
      }

      return {
        ...item,
        color,
      };
    });

    // 데이터 설정
    candlestickSeries.setData(candleData as CandlestickData<Time>[]);
    volumeSeries.setData(coloredVolumeData as HistogramData<Time>[]);
    chart.timeScale().setVisibleLogicalRange({
      from: chartData.length - 100,
      to: chartData.length,
    });

    // 무한스크롤
    let timeoutId: NodeJS.Timeout;
    chart.timeScale().subscribeVisibleLogicalRangeChange((logicalRange) => {
      if (logicalRange?.from && logicalRange.from < 10) {
        clearTimeout(timeoutId);
        timeoutId = setTimeout(() => {
          void loadMoreData();
        }, 250); // 300ms 디바운스
      }
    });

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

    // 툴팁 설정
    const tooltip = document.createElement("div");
    tooltip.style.position = "absolute";
    tooltip.style.display = "none";
    tooltip.style.padding = "8px";
    tooltip.style.background = "rgba(255, 255, 255, 0.9)";
    tooltip.style.border = "1px solid #ccc";
    tooltip.style.borderRadius = "4px";
    tooltip.style.fontSize = "12px";
    tooltip.style.pointerEvents = "none";
    tooltip.style.zIndex = "1000";
    chartContainerRef.current?.appendChild(tooltip);

    // 마우스 이벤트 핸들러
    chart.subscribeCrosshairMove((param: MouseEventParams) => {
      if (
        param.point === undefined ||
        !param.time ||
        param.point.x < 0 ||
        param.point.x > chartContainerRef.current!.clientWidth ||
        param.point.y < 0 ||
        param.point.y > chartContainerRef.current!.clientHeight
      ) {
        tooltip.style.display = "none";
        return;
      }

      const data = param.seriesData.get(candlestickSeries);
      if (!data) {
        tooltip.style.display = "none";
        return;
      }

      const candleData = data as CandlestickData<Time>;
      const yearStr = new Date(Number(param.time) * 1000).getFullYear();
      const dateStr = new Date(Number(param.time) * 1000).toLocaleDateString();
      const timeStr = new Date(Number(param.time) * 1000).toLocaleTimeString();
      const price = candleData.close;
      const volumeData = param.seriesData.get(volumeSeries);
      const volume = volumeData && "value" in volumeData ? volumeData.value : 0;

      let left = param.point.x + 15;
      if (left > chartContainerRef.current!.clientWidth - tooltip.clientWidth) {
        left = param.point.x - tooltip.clientWidth - 15;
      }

      let top = param.point.y + 15;
      if (top > chartContainerRef.current!.clientHeight - tooltip.clientHeight) {
        top = param.point.y - tooltip.clientHeight - 15;
      }

      tooltip.style.display = "block";
      tooltip.style.left = left + "px";
      tooltip.style.top = top + "px";
      tooltip.innerHTML = `
        <div style="font-weight: bold">${chartType === "YEARLY" ? yearStr + "년" : dateStr} ${chartType === "MINUTE" ? timeStr.slice(0, 8) : ""}</div>
        <div>시가: ${candleData.open}</div>
        <div>고가: ${candleData.high}</div>
        <div>저가: ${candleData.low}</div>
        <div style="color: ${
          Number(price) >= Number(candleData.open)
            ? "rgba(253, 65, 65, 1)"
            : "rgba(65, 112, 253, 1)"
        }">종가: ${price}</div>
        <div style="color: ${
          Number(price) >= Number(candleData.open)
            ? "rgba(253, 65, 65, 1)"
            : "rgba(65, 112, 253, 1)"
        }">거래량: ${volume?.toLocaleString() ?? "0"}</div>
      `;
    });

    // 컴포넌트 언마운트 시 정리
    return () => {
      window.removeEventListener("resize", handleResize);
      if (chartInstanceRef.current) {
        chartInstanceRef.current.remove();
        chartInstanceRef.current = null;
      }
      tooltip.remove();
      clearTimeout(timeoutId);
    };
  }, [chartData, loading, loadMoreData]);

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error}</div>;
  if (!chartData.length) return null;

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
        onTimeIntervalChange={handleTimeIntervalChange}
        timeInterval={timeInterval}
      />
      <div
        ref={chartContainerRef}
        style={{ width: "100%", height: "500px", position: "relative" }}
      />
    </div>
  );
};

export default Chart;
