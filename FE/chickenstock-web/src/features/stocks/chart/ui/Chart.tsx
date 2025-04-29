import { useEffect, useState } from "react";
import ChartBody from "./ChartBody";
import ChartHeader from "./ChartHeader";
import { ChartData, ChartResponse } from "../model/types";
import apiClient from "@/shared/api/axios";
import { getStockChartData } from "../api";

interface ChartProps {
  stockName?: string;
  stockCode?: string;
  priceData?: {
    currentPrice: string;
    priceChange: string;
    changeRate: string;
  };
}

interface ChartRequest {
  chartType: string;
  stockCode: string;
  hasNext: boolean;
  nextKey: string;
}

const Chart = ({ stockName = "삼성전자", stockCode = "005930", priceData }: ChartProps) => {
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [chartData, setChartData] = useState<ChartData[]>([]);
  const [chartType, setChartType] = useState<string>("DAILY");
  const [hasNext, setHasNext] = useState(false);
  const [nextKey, setNextKey] = useState("");
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
        setHasNext(data.hasNext);
        setNextKey(data.nextKey);
        console.log(data);
      } catch (err) {
        setError(err instanceof Error ? err.message : "An error occurred");
      } finally {
        setLoading(false);
      }
    };
    void fetchData();
  }, [stockCode, chartType]);

  const handleLoadMore = async (startDate: string, endDate: string) => {
    try {
      // API 호출로 추가 데이터 가져오기
      const response = await getStockChartData({
        stockCode,
        chartType,
        hasNext: true,
        nextKey: nextKey,
      });
      // 새로운 데이터를 기존 데이터 앞에 추가 (과거 데이터이므로)
      setChartData((prevData) => [...response.chartData, ...prevData]);
      setHasNext(response.hasNext);
      setNextKey(response.nextKey);
    } catch (error) {
      console.error("Failed to load more data:", error);
    }
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
      />
      <ChartBody chartData={chartData} onLoadMore={handleLoadMore} />
    </div>
  );
};

export default Chart;
