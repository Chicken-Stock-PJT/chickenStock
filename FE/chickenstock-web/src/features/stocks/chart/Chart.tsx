import { useEffect, useState } from "react";
import ChartBody from "./ChartBody";
import ChartHeader from "./ChartHeader";
import { ChartData, ChartResponse } from "./types";

interface ChartProps {
  stockCode?: string;
}

const Chart = ({ stockCode = "005930" }: ChartProps) => {
  const [chartData, setChartData] = useState<ChartData[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchChartData = async () => {
      setLoading(true);
      try {
        const response = await fetch(`/api/stock/chart/all/${stockCode}?chartType=DAILY`);
        if (!response.ok) throw new Error("Failed to fetch chart data");
        console.log(response);

        const data = (await response.json()) as ChartResponse;
        setChartData(data.chartData);
        return;
      } catch (err) {
        setError(err instanceof Error ? err.message : "An error occurred");
      } finally {
        setLoading(false);
      }
    };

    void fetchChartData();
  }, [stockCode]);

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error}</div>;

  return (
    <div className="flex w-full flex-col gap-4 rounded-lg border border-gray-200 bg-white p-4">
      <ChartHeader stockCode={stockCode} chartData={chartData[0]} />
      <ChartBody chartData={chartData} />
    </div>
  );
};

export default Chart;
