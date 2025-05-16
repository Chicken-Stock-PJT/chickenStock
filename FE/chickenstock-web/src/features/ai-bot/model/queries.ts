import { useQuery } from "@tanstack/react-query";
import { getAiBotDashboard } from "../api";

export const useAiBotDashboardQuery = (botId: number) => {
  return useQuery({
    queryKey: ["aiBotDashboard", botId],
    queryFn: () => getAiBotDashboard(botId),
    staleTime: 0,
    gcTime: 0,
  });
};
