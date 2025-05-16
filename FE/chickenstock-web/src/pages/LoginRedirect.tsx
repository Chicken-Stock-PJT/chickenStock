import { useEffect } from "react";
import { useSearchParams, useNavigate } from "react-router-dom";
import { useAuthStore } from "@/shared/store/auth";
import { useGetWatchlist } from "@/features/watchlist/model/queries";
import { useSimpleProfile } from "@/shared/model/queries";

const LoginRedirect = () => {
  const [searchParams] = useSearchParams();
  const code = searchParams.get("oneTimeCode");
  console.log(code);
  const navigate = useNavigate();
  const { refetch: refetchWatchlist } = useGetWatchlist();
  const { refetch: refetchProfile } = useSimpleProfile();

  useEffect(() => {
    if (code) {
      const fetchData = async (code: string) => {
        try {
          const response = await useAuthStore.getState().socialLogin(code);
          useAuthStore.getState().setAccessToken(response.accessToken);
          await refetchProfile();
          await refetchWatchlist();

          void navigate(localStorage.getItem("redirectUrl") ?? "/");
          localStorage.removeItem("redirectUrl");
        } catch (error) {
          console.error(error);
        }
      };
      void fetchData(code);
    }
  }, [code, navigate, refetchWatchlist, refetchProfile]);
  return (
    <div>
      <h3>로그인 중...</h3>
    </div>
  );
};

export default LoginRedirect;
