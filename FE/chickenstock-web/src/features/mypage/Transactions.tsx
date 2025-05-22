import TransactionsList from "../dashboard/ui/TransactionsList";
import { useGetTransactions } from "./model/queries";

const Transactions = () => {
  const { data, isLoading, error, fetchNextPage, hasNextPage, isFetchingNextPage } =
    useGetTransactions();
  if (isLoading) {
    return <div>Loading...</div>;
  }
  if (error) {
    return <div>Error: {error.message}</div>;
  }
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

export default Transactions;
