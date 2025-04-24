import { OpenOrder } from "./types";

interface OpenOrderItemProps {
  order: OpenOrder;
  idx: number;
}

const OpenOrderItem = ({ ...props }: OpenOrderItemProps) => {
  return (
    <div
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
        <span className="text-sm">
          {props.order.remainingVolume}주 / {props.order.totalVolume}주
        </span>
      </div>
      <span className="flex-1 text-sm">{props.order.orderPrice.toLocaleString()}원</span>
      <div className="flex-1 text-gray-500 text-xs text-right">
        {props.order.orderTime.toLocaleTimeString("ko-KR", {
          hour: "2-digit",
          minute: "2-digit",
        })}
      </div>
    </div>
  );
};

export default OpenOrderItem;
