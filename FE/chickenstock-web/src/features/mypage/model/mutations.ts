import { ErrorResponse } from "./types";
import { AxiosError } from "axios";
import { useMutation } from "@tanstack/react-query";
import { updateNickname } from "@/features/mypage/api";
import { queryClient } from "@/shared/api/queryClient";
import { toast } from "@/shared/libs/hooks/use-toast";

export const useUpdateNickname = () => {
  //   const queryClient = useQueryClient();
  const { mutateAsync } = useMutation({
    mutationFn: (nickname: string) => updateNickname(nickname),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ["simpleProfile"] });
      toast({
        description: "닉네임 변경이 완료되었습니다.",
      });
    },
    onError: (error: AxiosError<ErrorResponse>) => {
      const errorMessage = error.response?.data.message ?? "닉네임 변경에 실패했습니다.";
      toast({
        variant: "destructive",
        description: errorMessage,
      });
    },
  });
  return { mutateAsync };
};
