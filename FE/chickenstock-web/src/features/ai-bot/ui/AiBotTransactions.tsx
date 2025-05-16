import TransactionsList from "@/features/dashboard/ui/TransactionsList";
import { useAiTradeHistoryQuery } from "@/features/ai-bot/model/queries";
const AiBotTransactions = ({ botId }: { botId: number }) => {
  const { data, isLoading, error, fetchNextPage, hasNextPage, isFetchingNextPage } =
    useAiTradeHistoryQuery(botId);

  return (
    <div className="text-left">
      <TransactionsList
        data={data!}
        isLoading={isLoading}
        error={error}
        fetchNextPage={() => void fetchNextPage()}
        hasNextPage={hasNextPage}
        isFetchingNextPage={isFetchingNextPage}
      />
    </div>
  );
};

export default AiBotTransactions;
