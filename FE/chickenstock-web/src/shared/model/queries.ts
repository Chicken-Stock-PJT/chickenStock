import { useAuthStore } from "@/shared/store/auth";
import { useQuery } from "@tanstack/react-query";

export const useSimpleProfile = () => {
  const { isLoggedIn, getSimpleProfile } = useAuthStore();

  return useQuery({
    queryKey: ["simpleProfile"],
    queryFn: getSimpleProfile,
    enabled: isLoggedIn,
    staleTime: 1000 * 60 * 2, // 2분 동안은 캐시된 데이터 사용
    refetchOnWindowFocus: true, // 윈도우 포커스 시 갱신
    refetchOnMount: true, // 컴포넌트 마운트 시 갱신
    refetchOnReconnect: true, // 네트워크 재연결 시 갱신
  });
};
