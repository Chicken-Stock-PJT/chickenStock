import PendingOrdersList from "./ui/PendingOrdersList";
import PendingOrdersGuide from "./ui/PendingORdersGuide";

const PendingOrders = () => {
  return (
    <div className="space-y-6 text-left">
      <PendingOrdersList />
      <PendingOrdersGuide />
    </div>
  );
};

export default PendingOrders;
