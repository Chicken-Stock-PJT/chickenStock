import { Button } from "@/shared/libs/ui/button";
import { PendingOrder } from "../model/types";
import { useCancelOrder } from "../model/mutations";

interface OpenOrderItemProps {
  order: PendingOrder;
  idx: number;
}

const OpenOrderItem = ({ order, idx }: OpenOrderItemProps) => {
  const { mutate: cancelOrder } = useCancelOrder(order.orderId.toString(), order.stockCode);

  return (
    <div
      className={`flex items-center rounded-lg p-2 ${idx % 2 === 0 ? "bg-white" : "bg-gray-50"}`}
    >
      <div className="flex flex-1 flex-col text-left">
        <span
          className={`font-semibold ${order.orderType === "BUY" ? "text-chart-red" : "text-chart-blue"}`}
        >
          {order.orderType === "BUY" ? "매수" : "매도"}
        </span>
        <span className="text-sm">{order.quantity}주</span>
      </div>
      <span className="flex-1 text-sm">{order.targetPrice.toLocaleString()}원</span>
      <div className="flex-1 text-right text-xs text-gray-500">
        {new Date(order.createdAt).toLocaleTimeString("ko-KR", {
          hour: "2-digit",
          minute: "2-digit",
        })}
      </div>
      <Button className="ml-2" variant="outline" size="sm" onClick={() => cancelOrder()}>
        취소
      </Button>
    </div>
  );
};

export default OpenOrderItem;
