import { getUserInfo } from "@/features/mypage/api";
import { useAuthStore } from "@/shared/store/auth";
import { useQuery } from "@tanstack/react-query";

export const useSimpleProfile = () => {
  const { isLoggedIn } = useAuthStore();

  return useQuery({
    queryKey: ["simpleProfile"],
    queryFn: getUserInfo,
    enabled: isLoggedIn,
    staleTime: 0, // 항상 최신 데이터를 사용
    refetchOnWindowFocus: true, // 윈도우 포커스 시 갱신
    refetchOnMount: true,
    refetchOnReconnect: true, // 네트워크 재연결 시 갱신
  });
};
