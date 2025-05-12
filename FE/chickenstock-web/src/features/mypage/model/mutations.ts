import { ErrorResponse } from "./types";
import { AxiosError } from "axios";
import { useMutation } from "@tanstack/react-query";
import { updateNickname } from "@/features/mypage/api";
import { queryClient } from "@/shared/api/queryClient";

export const useUpdateNickname = () => {
  //   const queryClient = useQueryClient();
  const { mutateAsync } = useMutation({
    mutationFn: (nickname: string) => updateNickname(nickname),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ["simpleProfile"] });
    },
    onError: (error: AxiosError<ErrorResponse>) => {
      const errorMessage = error.response?.data.message ?? "닉네임 수정에 실패했습니다.";
      alert(errorMessage);
    },
  });
  return { mutateAsync };
};
