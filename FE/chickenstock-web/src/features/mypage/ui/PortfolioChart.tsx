import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import ReactApexChart from "react-apexcharts";

const PortfolioChart = () => {
  const options = {
    series: [44, 55, 41, 17, 15],
    options: {
      chart: {
        type: "donut" as const,
      },
      labels: ["삼성전자", "SK하이닉스", "카카오", "네이버", "LG화학"],
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
