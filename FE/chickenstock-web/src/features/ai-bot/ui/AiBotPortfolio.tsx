import AiBotDashboard from "./AiBotDashboard";
import AiBotTrades from "./AiBotTrades";
import { useMemberDashboardQuery } from "@/features/dashboard/model/queries";

const AiBotPortfolio = ({ selectedBot }: { selectedBot: number }) => {
  const { data, isLoading, error, refetch } = useMemberDashboardQuery(selectedBot);

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  const portfolio = data!;
  return (
    <div className="flex flex-col gap-4">
      <AiBotDashboard portfolio={portfolio} aiBotId={selectedBot} />
      <AiBotTrades portfolio={portfolio} refetch={() => void refetch()} selectedBot={selectedBot} />
    </div>
  );
};

export default AiBotPortfolio;
