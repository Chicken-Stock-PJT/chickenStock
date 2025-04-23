import ReactApexChart from "react-apexcharts";

interface ChartBodyProps {
  chartData: Array<{
    date: string;
    currentPrice: string;
    openPrice: string;
    highPrice: string;
    lowPrice: string;
  }>;
}

const ChartBody = ({ chartData }: ChartBodyProps) => {
  const formattedData = chartData.map((item) => ({
    x: new Date(item.date.replace(/(\d{4})(\d{2})(\d{2})/, "$1-$2-$3")),
    y: [
      Number(item.openPrice), // 시가
      Number(item.highPrice), // 고가
      Number(item.lowPrice), // 저가
      Number(item.currentPrice), // 종가
    ],
  }));

  const options = {
    chart: {
      type: "candlestick",
      height: 400,
      toolbar: {
        show: true,
        tools: {
          download: true,
          selection: true,
          zoom: true,
          zoomin: true,
          zoomout: true,
          pan: true,
          reset: true,
        },
      },
    },
    plotOptions: {
      candlestick: {
        colors: {
          upward: "#FD4141", // 상승 캔들 색상 (빨간색)
          downward: "#4170FD", // 하락 캔들 색상 (파란색)
        },
      },
    },
    xaxis: {
      type: "datetime",
      labels: {
        formatter: function (value: string) {
          return new Date(value).toLocaleDateString("ko-KR", {
            month: "short",
            day: "numeric",
          });
        },
      },
    },
    yaxis: {
      tooltip: {
        enabled: true,
      },
      labels: {
        formatter: function (value: number) {
          return value.toLocaleString("ko-KR") + "원";
        },
      },
    },
    tooltip: {
      custom: function ({ seriesIndex, dataPointIndex, w }: any) {
        const o = w.globals.seriesCandleO[seriesIndex][dataPointIndex];
        const h = w.globals.seriesCandleH[seriesIndex][dataPointIndex];
        const l = w.globals.seriesCandleL[seriesIndex][dataPointIndex];
        const c = w.globals.seriesCandleC[seriesIndex][dataPointIndex];
        const date = new Date(w.globals.seriesX[seriesIndex][dataPointIndex]);

        return (
          '<div class="apexcharts-tooltip-box">' +
          `<div>${date.toLocaleDateString("ko-KR", {
            year: "numeric",
            month: "long",
            day: "numeric",
          })}</div>` +
          `<div>시가: ${o.toLocaleString()}원</div>` +
          `<div>고가: ${h.toLocaleString()}원</div>` +
          `<div>저가: ${l.toLocaleString()}원</div>` +
          `<div>종가: ${c.toLocaleString()}원</div>` +
          "</div>"
        );
      },
    },
  };

  const series = [
    {
      name: "주가",
      data: formattedData,
    },
  ];

  return (
    <div className="w-full h-[400px]">
      <ReactApexChart
        options={options}
        series={series}
        type="candlestick"
        height={400}
      />
    </div>
  );
};

export default ChartBody;
