import TransactionsList from "@/features/dashboard/ui/TransactionsList";
import { useGetTransactions } from "@/features/mypage/model/queries";
const AiBotTransactions = () => {
  const { data, isLoading, error, fetchNextPage, hasNextPage, isFetchingNextPage } =
    useGetTransactions();

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
