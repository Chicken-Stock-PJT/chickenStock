import { OpenOrder } from "./types";

interface OpenOrderItemProps {
  order: OpenOrder;
  idx: number;
}

const OpenOrderItem = ({ ...props }: OpenOrderItemProps) => {
  return (
    <div
      className={`flex items-center rounded-lg p-2 ${
        props.idx % 2 === 0 ? "bg-white" : "bg-gray-50"
      }`}
    >
      <div className="flex flex-1 flex-col text-left">
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
      <div className="flex-1 text-right text-xs text-gray-500">
        {props.order.orderTime.toLocaleTimeString("ko-KR", {
          hour: "2-digit",
          minute: "2-digit",
        })}
      </div>
    </div>
  );
};

export default OpenOrderItem;
