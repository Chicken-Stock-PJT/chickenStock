import { TradeHistory } from "../model/types";

interface FilledOrderItemProps {
  order: TradeHistory;
  idx: number;
}

const FilledOrderItem = ({ ...props }: FilledOrderItemProps) => {
  return (
    <div
      key={props.idx}
      className={`flex items-center rounded-lg p-1.5 sm:p-2 ${
        props.idx % 2 === 0 ? "bg-white" : "bg-gray-50"
      }`}
    >
      <div className="flex flex-1 flex-col text-left">
        <span
          className={`text-xs font-semibold sm:text-sm ${props.order.tradeType === "BUY" ? "text-chart-red" : "text-chart-blue"}`}
        >
          {props.order.tradeType === "BUY" ? "매수" : "매도"}
        </span>
        <span className="text-[10px] sm:text-sm">{props.order.quantity}주</span>
      </div>
      <span className="flex-1 text-[10px] sm:text-sm">
        {props.order.unitPrice.toLocaleString()}원
      </span>
      <div className="flex flex-1 flex-col text-right text-[8px] text-gray-500 sm:text-xs">
        {/* <span>
          신청:{" "}
          {new Date(props.order.tradedAt).toLocaleTimeString("ko-KR", {
            hour: "2-digit",
            minute: "2-digit",
          })}
        </span> */}
        {/* 체결시간 */}
        <div className="flex flex-col items-end">
          <span>
            {new Date(props.order.tradedAt).toLocaleDateString("ko-KR", {
              year: "numeric",
              month: "2-digit",
              day: "2-digit",
            })}
          </span>
          <span>
            {new Date(props.order.tradedAt).toLocaleTimeString("ko-KR", {
              hour: "2-digit",
              minute: "2-digit",
            })}
          </span>
        </div>
      </div>
    </div>
  );
};

export default FilledOrderItem;
