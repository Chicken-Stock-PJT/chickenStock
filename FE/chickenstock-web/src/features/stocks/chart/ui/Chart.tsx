import { useEffect, useState } from "react";
// import ChartBody from "./ChartBody";
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

  useEffect(() => {
    const fetchData = async () => {
      try {
        const data = await getStockChartData({
          stockCode,
          chartType,
          hasNext: false,
          nextKey: "",
        });
        setChartData(data.chartData);
        console.log(data);
      } catch (err) {
        setError(err instanceof Error ? err.message : "An error occurred");
      } finally {
        setLoading(false);
      }
    };
    void fetchData();
  }, [stockCode, chartType]);

  // 웹소켓으로 받은 실시간 데이터로 차트 업데이트
  useEffect(() => {
    if (stockPriceData && chartData.length > 0) {
      const lastData = chartData[chartData.length - 1];
      const updatedData = [...chartData];

      // 마지막 데이터 업데이트
      updatedData[updatedData.length - 1] = {
        ...lastData,
        currentPrice: stockPriceData.currentPrice,
        highPrice: Math.max(
          Number(lastData.highPrice),
          Number(stockPriceData.currentPrice),
        ).toString(),
        lowPrice: Math.min(
          Number(lastData.lowPrice),
          Number(stockPriceData.currentPrice),
        ).toString(),
      };

      setChartData(updatedData);
    }
  }, [stockPriceData]);

  const handleChartTypeChange = (type: ChartType) => {
    setChartType(type);
    setChartData([]);
  };

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
      />
      <StockChart sampleData={chartData} chartType={chartType} />
    </div>
  );
};

export default Chart;
