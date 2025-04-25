import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/widgets/ui/tabs";
import OpenOrderItem from "./OpenOrderItem";
import { FilledOrder, OpenOrder } from "./types";
import FilledOrderItem from "./FilledOrderItem";

const Status = () => {
  // mock 체결내역 데이터
  const filledTrades: FilledOrder[] = [];
  // const filledTrades: FilledOrder[] = [
  //   {
  //     side: "BUY",
  //     executedVolume: 12,
  //     executionPrice: 15300,
  //     submittedAt: new Date("2025-04-24T14:35:12"),
  //     executedAt: new Date("2025-04-24T14:35:20"),
  //   },
  //   {
  //     side: "SELL",
  //     executedVolume: 8,
  //     executionPrice: 15320,
  //     submittedAt: new Date("2025-04-24T14:36:01"),
  //     executedAt: new Date("2025-04-24T14:36:07"),
  //   },
  //   {
  //     side: "BUY",
  //     executedVolume: 25,
  //     executionPrice: 15280,
  //     submittedAt: new Date("2025-04-24T14:37:45"),
  //     executedAt: new Date("2025-04-24T14:37:46"),
  //   },
  //   {
  //     side: "SELL",
  //     executedVolume: 5,
  //     executionPrice: 15310,
  //     submittedAt: new Date("2025-04-24T14:38:10"),
  //     executedAt: new Date("2025-04-24T14:38:11"),
  //   },
  //   {
  //     side: "BUY",
  //     executedVolume: 40,
  //     executionPrice: 15290,
  //     submittedAt: new Date("2025-04-24T14:38:55"),
  //     executedAt: new Date("2025-04-24T14:39:02"),
  //   },
  // ];
  // mock 미체결내역 데이터
  const openOrders: OpenOrder[] = [
    {
      side: "BUY",
      totalVolume: 10,
      remainingVolume: 4,
      orderPrice: 15350,
      orderTime: new Date("2025-04-24T14:40:00"),
    },
    {
      side: "SELL",
      totalVolume: 20,
      remainingVolume: 20,
      orderPrice: 15400,
      orderTime: new Date("2025-04-24T14:41:15"),
    },
    {
      side: "BUY",
      totalVolume: 15,
      remainingVolume: 7,
      orderPrice: 15380,
      orderTime: new Date("2025-04-24T14:42:30"),
    },
    {
      side: "SELL",
      totalVolume: 12,
      remainingVolume: 12,
      orderPrice: 15420,
      orderTime: new Date("2025-04-24T14:43:05"),
    },
    {
      side: "BUY",
      totalVolume: 8,
      remainingVolume: 2,
      orderPrice: 15360,
      orderTime: new Date("2025-04-24T14:44:20"),
    },
  ];

  return (
    <div className="flex h-full flex-col gap-2 rounded-lg border border-gray-200 bg-white p-4">
      <Tabs defaultValue="filled" className="flex h-full flex-col">
        <TabsList className="mb-2 flex gap-1.5">
          <TabsTrigger value="filled" className="w-full">
            체결내역
          </TabsTrigger>
          <TabsTrigger value="unfilled" className="w-full">
            미체결내역
          </TabsTrigger>
        </TabsList>

        {/* 체결내역 컨텐츠 */}
        <TabsContent value="filled" className="flex-1 overflow-auto">
          {filledTrades.length ? (
            filledTrades.map((order, idx) => <FilledOrderItem key={idx} order={order} idx={idx} />)
          ) : (
            <div className="flex h-full items-center justify-center text-sm text-gray-500">
              체결 내역이 없습니다.
            </div>
          )}
        </TabsContent>

        {/* 미체결내역 컨텐츠 */}
        <TabsContent value="unfilled" className="flex-1 overflow-auto">
          {openOrders.length ? (
            openOrders.map((order, idx) => <OpenOrderItem key={idx} order={order} idx={idx} />)
          ) : (
            <div className="flex h-full items-center justify-center text-sm text-gray-500">
              체결 내역이 없습니다.
            </div>
          )}
        </TabsContent>
      </Tabs>
    </div>
  );
};

export default Status;
