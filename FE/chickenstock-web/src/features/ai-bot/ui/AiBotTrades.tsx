import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/shared/libs/ui/tabs";
import { useState } from "react";
import AiBotHoldings from "./AiBotHoldings";
import AiBotTransactions from "./AiBotTransactions";
import { MemberDashboardResponse } from "@/features/dashboard/model/types";

const AiBotTrades = ({
  portfolio,
  refetch,
}: {
  portfolio: MemberDashboardResponse;
  refetch: () => void;
}) => {
  const [selectedTab, setSelectedTab] = useState<"holding" | "transactions">("holding");

  return (
    <div className="text-left">
      <Tabs
        value={selectedTab}
        onValueChange={(value) => setSelectedTab(value as "holding" | "transactions")}
      >
        <TabsList>
          <TabsTrigger value="holding">보유 종목</TabsTrigger>
          <TabsTrigger value="transactions">거래내역</TabsTrigger>
        </TabsList>
        <TabsContent value="holding">
          <AiBotHoldings holdings={portfolio.holdings} onClick={() => void refetch()} />
        </TabsContent>
        <TabsContent value="transactions">
          <AiBotTransactions />
        </TabsContent>
      </Tabs>
    </div>
  );
};

export default AiBotTrades;
