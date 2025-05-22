import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/shared/libs/ui/tabs";
import OpenOrderItem from "./OpenOrderItem";
import FilledOrderItem from "./FilledOrderItem";
import { useGetPendingOrders, useGetStatus } from "@/features/stocks/status/model/queries";
import { useAuthStore } from "@/shared/store/auth";
import { Button } from "@/shared/libs/ui/button";
import { useNavigate } from "react-router-dom";

const Status = ({ stockCode }: { stockCode: string }) => {
  const navigate = useNavigate();
  const { isLoggedIn } = useAuthStore();

  const { data: filledTrades } = useGetStatus(stockCode);
  const { data: pendingOrders } = useGetPendingOrders(stockCode);

  const toLogin = () => {
    localStorage.setItem("redirectUrl", window.location.pathname);
    void navigate("/login");
  };
  return (
    <div className="flex h-full max-h-[518px] flex-col gap-2 rounded-lg border border-gray-200 bg-white p-4 sm:max-h-[907px]">
      <Tabs defaultValue="filled" className="flex h-full flex-col">
        <TabsList className="mb-2 flex gap-1.5">
          <TabsTrigger value="filled" className="w-full">
            체결내역
          </TabsTrigger>
          <TabsTrigger value="unfilled" className="w-full">
            미체결내역
          </TabsTrigger>
        </TabsList>

        {isLoggedIn ? (
          <>
            {/* 체결내역 컨텐츠 */}
            <TabsContent value="filled" className="flex-1 overflow-auto">
              {filledTrades?.tradeHistories.length ? (
                filledTrades.tradeHistories
                  .map((order, idx) => <FilledOrderItem key={idx} order={order} idx={idx} />)
                  .reverse()
              ) : (
                <div className="flex h-full items-center justify-center text-sm text-gray-500">
                  체결 내역이 없습니다.
                </div>
              )}
            </TabsContent>

            {/* 미체결내역 컨텐츠 */}
            <TabsContent value="unfilled" className="flex-1 overflow-auto">
              {pendingOrders?.length ? (
                pendingOrders.map((order, idx) => (
                  <OpenOrderItem key={order.orderId} order={order} idx={idx} />
                ))
              ) : (
                <div className="flex h-full items-center justify-center text-sm text-gray-500">
                  보류중인 주문이 없습니다.
                </div>
              )}
            </TabsContent>
          </>
        ) : (
          <div className="flex h-full flex-col items-center justify-center gap-2">
            <div className="mb-2 text-sm text-gray-500">
              로그인 후 체결내역 및 미체결내역을 확인할 수 있습니다.
            </div>
            <Button onClick={toLogin}>로그인</Button>
          </div>
        )}
      </Tabs>
    </div>
  );
};

export default Status;
