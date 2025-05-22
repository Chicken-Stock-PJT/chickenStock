import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import ReactApexChart from "react-apexcharts";
import { Holdings } from "@/features/dashboard/model/types";

interface PortfolioChartProps {
  holdings: Holdings[];
  stockValuation: number;
  cash: number;
}

const PortfolioChart = ({ holdings, stockValuation, cash }: PortfolioChartProps) => {
  const options = {
    series: [...holdings.map((holding) => holding.valuationAmount)],
    options: {
      chart: {
        type: "donut" as const,
      },
      labels: [...holdings.map((holding) => holding.stockName)],
      responsive: [
        {
          breakpoint: 480,
          options: {
            chart: {
              width: 200,
            },
            legend: {
              position: "bottom",
            },
          },
        },
      ],
    },
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle>종목별 투자 비중</CardTitle>
        <CardDescription>투자 종목별 비중을 확인합니다.</CardDescription>
        <div className="flex gap-8 pt-4">
          <div className="py-2">
            <div className="text-sm font-semibold text-gray-500">현금</div>
            <div className="font-bold">{cash.toLocaleString()}원</div>
          </div>
          <div className="border-l pl-4"></div>
          <div className="py-2">
            <div className="text-sm font-semibold text-gray-500">총 평가금액</div>
            <div className="font-bold">{stockValuation.toLocaleString()}원</div>
          </div>
        </div>
      </CardHeader>

      <CardContent>
        <div id="chart">
          <ReactApexChart options={options.options} series={options.series} type="donut" />
        </div>
      </CardContent>
    </Card>
  );
};

export default PortfolioChart;
