import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import ReactApexChart from "react-apexcharts";
import { Position } from "../model/types";

interface PortfolioChartProps {
  positions: Position[];
  totalValuation: number;
  memberMoney: number;
}

const PortfolioChart = ({ positions, totalValuation, memberMoney }: PortfolioChartProps) => {
  const options = {
    // series: [memberMoney, ...positions.map((position) => position.valuationAmount)],

    series: positions.map((position) => position.valuationAmount),
    options: {
      chart: {
        type: "donut" as const,
      },
      // labels: ["현금", ...positions.map((position) => position.stockName)],
      labels: positions.map((position) => position.stockName),
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
        <CardDescription>포트폴리오 내 종목별 비중을 확인합니다.</CardDescription>
        <div className="ml-4 flex gap-8 pt-4">
          <div className="py-2">
            <div className="text-sm font-semibold text-gray-500">현금</div>
            <div className="font-bold">{memberMoney.toLocaleString()}원</div>
          </div>
          <div className="border-l pl-4"></div>
          <div className="py-2">
            <div className="text-sm font-semibold text-gray-500">총 평가금액</div>
            <div className="font-bold">{totalValuation.toLocaleString()}원</div>
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
