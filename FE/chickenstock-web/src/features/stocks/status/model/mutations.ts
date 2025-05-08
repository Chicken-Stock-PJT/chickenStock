import { useMutation, useQueryClient } from "@tanstack/react-query";
import { cancelOrder } from "../api";

export const useCancelOrder = (orderId: string, stockCode: string) => {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => cancelOrder(orderId),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ["pendingOrders", stockCode] });
    },
    onError: (error) => {
      alert(error.message);
    },
  });
};
