import HoldingsList from "@/features/dashboard/ui/HoldingsList";
import { Holdings } from "@/features/dashboard/model/types";

const AiBotHoldings = ({ holdings, onClick }: { holdings: Holdings[]; onClick: () => void }) => {
  return <HoldingsList holdings={holdings} onClick={onClick} isLoading={false} />;
};

export default AiBotHoldings;
