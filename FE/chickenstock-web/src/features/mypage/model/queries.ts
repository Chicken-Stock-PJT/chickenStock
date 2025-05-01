import { useMutation } from "@tanstack/react-query";
import { useAuthStore } from "@/shared/store/auth";
import { updateNickname } from "../api";
import { AxiosError } from "axios";
interface ErrorResponse {
  status: number;
  code: string;
  error: string;
  message: string;
  path: string;
  timestamp: string;
}

export const useUpdateNickname = () => {
  //   const queryClient = useQueryClient();
  const { mutateAsync } = useMutation({
    mutationFn: (nickname: string) => updateNickname(nickname),
    onSuccess: (_, nickname) => {
      // 쿼리 무효화
      //   void queryClient.invalidateQueries({ queryKey: ["profile"] });
      useAuthStore.getState().setSimpleProfile({ nickname });
    },
    onError: (error: AxiosError<ErrorResponse>) => {
      const errorMessage = error.response?.data.message ?? "닉네임 수정에 실패했습니다.";
      alert(errorMessage);
    },
  });
  return { mutateAsync };
};
