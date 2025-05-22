import { TrendingUp, TrendingDown, Clock, X } from "lucide-react";
import { usePendingOrdersQuery } from "../model/queries";
import { useCancelOrderMutation } from "../model/mutations";
import { ConfirmModal } from "./ConfirmModal";
import { useState } from "react";

const PendingOrdersList = () => {
  const { data: orders, isLoading, error } = usePendingOrdersQuery();
  const cancelOrderMutation = useCancelOrderMutation();
  const [confirmModal, setConfirmModal] = useState<{
    show: boolean;
    orderId: number;
    stockName: string;
  }>({
    show: false,
    orderId: 0,
    stockName: "",
  });

  const handleCancelClick = (orderId: number, stockName: string) => {
    setConfirmModal({
      show: true,
      orderId,
      stockName,
    });
  };

  const handleConfirmCancel = () => {
    cancelOrderMutation.mutate({ orderId: confirmModal.orderId });
    setConfirmModal({ show: false, orderId: 0, stockName: "" });
  };

  const handleModalClose = () => {
    setConfirmModal({ show: false, orderId: 0, stockName: "" });
  };

  if (isLoading) {
    return <div className="flex justify-center py-8">로딩 중...</div>;
  }

  if (error) {
    return <div className="py-8 text-center text-red-500">오류가 발생했습니다.</div>;
  }

  if (!orders || orders.length === 0) {
    return (
      <div className="py-12 text-center">
        <Clock className="mx-auto mb-3 size-12 text-gray-400" />
        <p className="text-gray-600">대기 중인 지정가 주문이 없습니다.</p>
      </div>
    );
  }

  return (
    <div className="space-y-4">
      <div className="mb-6 flex flex-col gap-2 sm:flex-row sm:items-center sm:justify-between">
        <h2 className="text-xl font-bold sm:text-2xl">지정가 주문목록</h2>
        <span className="text-sm text-gray-500">총 {orders.length}건</span>
      </div>

      <div className="grid gap-4">
        {orders.map((order) => (
          <div
            key={order.orderId}
            className="rounded-lg border border-gray-200 bg-white p-4 shadow-sm transition-all hover:shadow-md"
          >
            <div className="flex gap-4 sm:items-start sm:justify-between">
              <div className="flex-1">
                {/* 종목 정보 */}
                <div className="mb-3 flex flex-wrap items-start gap-2">
                  <div className="items-center gap-2 sm:flex">
                    <h3 className="text-base font-semibold sm:text-lg">{order.stockName}</h3>
                    <span className="text-xs text-gray-500 sm:text-sm">({order.stockCode})</span>
                  </div>
                  <span
                    className={`mt-1 inline-flex items-center gap-1 rounded-full px-2 py-0.5 text-xs font-medium ${
                      order.orderType === "BUY"
                        ? "bg-red-100 text-red-700"
                        : "bg-blue-100 text-blue-700"
                    }`}
                  >
                    {order.orderType === "BUY" ? (
                      <TrendingUp className="size-3" />
                    ) : (
                      <TrendingDown className="size-3" />
                    )}
                    {order.orderType === "BUY" ? "매수" : "매도"}
                  </span>
                </div>

                {/* 주문 상세 정보 */}
                <div className="grid grid-cols-2 gap-3 text-sm sm:grid-cols-3">
                  <div>
                    <span className="block text-xs text-gray-500 sm:text-sm">주문수량</span>
                    <p className="font-medium">{order.quantity.toLocaleString()}주</p>
                  </div>
                  <div>
                    <span className="block text-xs text-gray-500 sm:text-sm">목표가</span>
                    <p className="font-medium">₩{order.targetPrice.toLocaleString()}</p>
                  </div>
                  <div className="col-span-2 sm:col-span-1">
                    <span className="block text-xs text-gray-500 sm:text-sm">주문시간</span>
                    <p className="font-medium">
                      {new Date(order.createdAt).toLocaleDateString("ko-KR", {
                        month: "2-digit",
                        day: "2-digit",
                        hour: "2-digit",
                        minute: "2-digit",
                      })}
                    </p>
                  </div>
                </div>
              </div>

              {/* 취소 버튼 */}
              <button
                onClick={() => handleCancelClick(order.orderId, order.stockName)}
                className="ml-4 h-full rounded-full p-2 text-gray-500 transition-colors hover:bg-gray-100 hover:text-red-600"
                disabled={cancelOrderMutation.isPending}
              >
                <X className="size-5" />
              </button>
            </div>
          </div>
        ))}
      </div>

      {/* Confirm Modal */}
      <ConfirmModal
        show={confirmModal.show}
        title="지정가 주문 취소"
        message={`"${confirmModal.stockName}" 지정가 주문을 취소하시겠습니까?`}
        onConfirm={handleConfirmCancel}
        onCancel={handleModalClose}
        confirmText="주문 취소"
        cancelText="닫기"
      />
    </div>
  );
};

export default PendingOrdersList;
