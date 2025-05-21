import DashboardLayout from "@/features/dashboard/ui/DashboardLayout";
import { useLocation } from "react-router-dom";

const DashboardPage = () => {
  const { name } = useLocation().state as { name: string };
  return <DashboardLayout name={name} />;
};

export default DashboardPage;
