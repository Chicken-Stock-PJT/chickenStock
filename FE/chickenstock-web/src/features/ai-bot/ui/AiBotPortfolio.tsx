import { useMemberDashboardQuery } from "@/features/mypage/model/queries";
import AiBotDashboard from "./AiBotDashboard";
import AiBotTrades from "./AiBotTrades";

const AiBotPortfolio = () => {
  const { data, isLoading, error, refetch } = useMemberDashboardQuery();

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  const portfolio = data!;
  return (
    <div>
      <AiBotDashboard portfolio={portfolio} />
      <AiBotTrades portfolio={portfolio} refetch={() => void refetch()} />
    </div>
  );
};

export default AiBotPortfolio;
