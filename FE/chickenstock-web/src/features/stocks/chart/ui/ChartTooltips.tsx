import { CHART_COLORS } from "../model/constants";
import { FormattedChartData } from "../model/types";

interface CandlestickTooltipProps {
  dataPointIndex: number;
  chartData: FormattedChartData[];
}

export const renderCandlestickTooltip = ({
  dataPointIndex,
  chartData,
}: CandlestickTooltipProps): string => {
  const o = chartData[dataPointIndex].y[0];
  const h = chartData[dataPointIndex].y[1];
  const l = chartData[dataPointIndex].y[2];
  const c = chartData[dataPointIndex].y[3];
  const timeLabel = chartData[dataPointIndex].timeLabel;

  return `<div class="apexcharts-tooltip-box" style="padding: 8px; background: ${CHART_COLORS.TOOLTIP_BG}; border: 1px solid ${CHART_COLORS.TOOLTIP_BORDER}; box-shadow: 0 2px 8px rgba(0,0,0,0.1); border-radius: 4px;">
    <div style="font-weight: bold; margin-bottom: 5px;">${timeLabel}</div>
    <div>시가: ${o.toLocaleString()}</div>
    <div>고가: ${h.toLocaleString()}</div>
    <div>저가: ${l.toLocaleString()}</div>
    <div${c >= o ? ` style="color: ${CHART_COLORS.UPWARD}; font-weight: bold;"` : ` style="color: ${CHART_COLORS.DOWNWARD}; font-weight: bold;"`}>종가: ${c.toLocaleString()}</div>
  </div>`;
};

interface VolumeTooltipProps {
  dataPointIndex: number;
  chartData: FormattedChartData[];
}

export const renderVolumeTooltip = ({ dataPointIndex, chartData }: VolumeTooltipProps): string => {
  const volume = chartData[dataPointIndex].volume;
  const timeLabel = chartData[dataPointIndex].timeLabel;
  const o = chartData[dataPointIndex].y[0];
  const c = chartData[dataPointIndex].y[3];
  const color = c >= o ? CHART_COLORS.UPWARD : CHART_COLORS.DOWNWARD;

  return `<div class="apexcharts-tooltip-box" style="padding: 8px; background: ${CHART_COLORS.TOOLTIP_BG}; border: 1px solid ${CHART_COLORS.TOOLTIP_BORDER}; box-shadow: 0 2px 8px rgba(0,0,0,0.1); border-radius: 4px;">
    <div style="font-weight: bold; margin-bottom: 5px;">${timeLabel}</div>
    <div style="color: ${color};">거래량: ${volume.toLocaleString()}</div>
  </div>`;
};
