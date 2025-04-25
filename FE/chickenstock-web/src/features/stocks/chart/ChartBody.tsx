import ReactApexChart from "react-apexcharts";

interface ChartBodyProps {
  chartData: {
    date: string;
    currentPrice: string;
    openPrice: string;
    highPrice: string;
    lowPrice: string;
  }[];
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
      type: "candlestick" as const,
      height: 298,
      animations: {
        enabled: true,
        animateGradually: {
          enabled: true,
        },
        dynamicAnimation: {
          enabled: false,
        },
      },
      toolbar: {
        show: true,
        tools: {
          download: false,
          selection: true,
          zoom: true,
          zoomin: true,
          zoomout: true,
          pan: true,
          reset: true,
        },
        autoSelected: "pan" as const, // 기본으로 pan 도구 선택
      },
      zoom: {
        enabled: true,
        type: "xy" as const, // x축과 y축 모두 줌 가능
        autoScaleYaxis: true, // y축 자동 스케일링 활성화
      },
      pan: {
        enabled: true,
        type: "xy" as const, // x축과 y축 모두 pan 가능
      },
    },
    plotOptions: {
      candlestick: {
        colors: {
          upward: "#FD4141", // 상승 캔들 색상 (빨간색)
          downward: "#4170FD", // 하락 캔들 색상 (파란색)
        },
        wick: {
          useFillColor: true, // 심지 색상을 캔들 색상과 동일하게 설정
        },
      },
    },
    xaxis: {
      type: "datetime" as const,
      labels: {
        formatter: function (value: string) {
          return new Date(value).toLocaleDateString("ko-KR", {
            month: "short",
            day: "numeric",
          });
        },
        datetimeUTC: false, // 로컬 시간 사용
      },
      tooltip: {
        enabled: false,
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
      floating: false,
      forceNiceScale: true, // 좋은 스케일 강제로 사용
      tickAmount: 8, // y축 눈금 수
    },
    tooltip: {
      enabled: true,
      shared: false,
      custom: function ({ seriesIndex, dataPointIndex, w }: any) {
        const o = w.globals.seriesCandleO[seriesIndex][dataPointIndex];
        const h = w.globals.seriesCandleH[seriesIndex][dataPointIndex];
        const l = w.globals.seriesCandleL[seriesIndex][dataPointIndex];
        const c = w.globals.seriesCandleC[seriesIndex][dataPointIndex];
        const date = new Date(w.globals.seriesX[seriesIndex][dataPointIndex]);

        return (
          '<div class="apexcharts-tooltip-box" style="padding: 8px; background: rgba(255, 255, 255, 0.95); border: 1px solid #ccc; box-shadow: 0 2px 8px rgba(0,0,0,0.1); border-radius: 4px;">' +
          `<div style="font-weight: bold; margin-bottom: 5px;">${date.toLocaleDateString("ko-KR", {
            year: "numeric",
            month: "long",
            day: "numeric",
          })}</div>` +
          `<div>시가: ${o.toLocaleString()}원</div>` +
          `<div>고가: ${h.toLocaleString()}원</div>` +
          `<div>저가: ${l.toLocaleString()}원</div>` +
          `<div${c >= o ? ' style="color: #FD4141; font-weight: bold;"' : ' style="color: #4170FD; font-weight: bold;"'}>종가: ${c.toLocaleString()}원</div>` +
          "</div>"
        );
      },
    },
    grid: {
      borderColor: "#f1f1f1",
      xaxis: {
        lines: {
          show: true,
        },
      },
    },
    responsive: [
      {
        breakpoint: 1000,
        options: {
          chart: {
            height: 298,
          },
        },
      },
    ],
  };

  const series = [
    {
      name: "주가",
      data: formattedData,
    },
  ];

  return (
    <div className="h-[298px] w-100">
      <ReactApexChart options={options} series={series} type="candlestick" height={298} />
    </div>
  );
};

export default ChartBody;
