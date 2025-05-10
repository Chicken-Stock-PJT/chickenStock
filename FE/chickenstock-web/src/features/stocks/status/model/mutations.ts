import { useMutation, useQueryClient } from "@tanstack/react-query";
import { cancelOrder } from "../api";
import { toast } from "@/shared/libs/hooks/use-toast";
export const useCancelOrder = (orderId: string, stockCode: string) => {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: () => cancelOrder(orderId),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ["pendingOrders", stockCode] });
      toast({
        description: "주문이 취소되었습니다.",
      });
    },
    onError: (error) => {
      toast({
        variant: "destructive",
        description: error.message,
      });
    },
  });
};
