import { useEffect, useState, useCallback, useMemo, useRef } from "react";
import ChartHeader from "./ChartHeader";
import { ChartData, ChartType } from "../model/types";
import { getStockChartData } from "../api";
import StockChart from "./ChartBody";
import { useWebSocketStore } from "@/shared/store/websocket";

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
      setChartData(data.chartData);
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

    const newPrice = Number(latestPriceRef.current.currentPrice);
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
      <StockChart sampleData={memoizedChartData} chartType={chartType} />
    </div>
  );
};

export default Chart;
