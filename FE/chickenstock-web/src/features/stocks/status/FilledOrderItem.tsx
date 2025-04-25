import { FilledOrder } from "./types";

interface FilledOrderItemProps {
  order: FilledOrder;
  idx: number;
}

const FilledOrderItem = ({ ...props }: FilledOrderItemProps) => {
  return (
    <div
      key={props.idx}
      className={`flex items-center p-2 rounded-lg ${
        props.idx % 2 === 0 ? "bg-white" : "bg-gray-50"
      }`}
    >
      <div className="flex flex-col text-left flex-1">
        <span
          className={`font-semibold ${props.order.side === "BUY" ? "text-chart-red" : "text-chart-blue"}`}
        >
          {props.order.side === "BUY" ? "매수" : "매도"}
        </span>
        <span className="text-sm">{props.order.executedVolume}주</span>
      </div>
      <span className="flex-1 text-sm">{props.order.executionPrice.toLocaleString()}원</span>
      <div className="flex-1 flex flex-col text-gray-500 text-xs text-right">
        <span>
          신청:{" "}
          {props.order.submittedAt.toLocaleTimeString("ko-KR", {
            hour: "2-digit",
            minute: "2-digit",
          })}
        </span>
        <span>
          체결:{" "}
          {props.order.executedAt.toLocaleTimeString("ko-KR", {
            hour: "2-digit",
            minute: "2-digit",
          })}
        </span>
      </div>
    </div>
  );
};

export default FilledOrderItem;
