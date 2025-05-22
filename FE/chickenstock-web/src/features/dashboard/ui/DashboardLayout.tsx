import AiBotTrades from "@/features/ai-bot/ui/AiBotTrades";
import { useMemberDashboardQuery } from "@/features/dashboard/model/queries";
import MemberDashboard from "@/features/dashboard/ui/MemberDashboard";
import { useParams } from "react-router-dom";

const DashboardLayout = ({ name }: { name: string }) => {
  const memberId = useParams().memberId;
  const { data, isLoading, error, refetch } = useMemberDashboardQuery(Number(memberId));

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  const portfolio = data!;

  return (
    <div className="mx-4 mb-10 flex flex-col gap-4">
      <MemberDashboard name={name} portfolio={portfolio} />
      <AiBotTrades
        portfolio={portfolio}
        refetch={() => void refetch()}
        selectedBot={Number(memberId)}
      />
    </div>
  );
};

export default DashboardLayout;
