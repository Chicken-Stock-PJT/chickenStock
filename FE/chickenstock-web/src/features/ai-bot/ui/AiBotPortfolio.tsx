import AiBotDashboard from "./AiBotDashboard";
import AiBotTrades from "./AiBotTrades";
import { useAiBotDashboardQuery } from "@/features/ai-bot/model/queries";

const AiBotPortfolio = ({ selectedBot }: { selectedBot: number }) => {
  const { data, isLoading, error, refetch } = useAiBotDashboardQuery(selectedBot);

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;
  console.log(data);

  const portfolio = data!;
  return (
    <div>
      <AiBotDashboard portfolio={portfolio} aiBotId={selectedBot} />
      <AiBotTrades portfolio={portfolio} refetch={() => void refetch()} />
    </div>
  );
};

export default AiBotPortfolio;
