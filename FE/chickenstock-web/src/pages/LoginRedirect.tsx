import { useEffect } from "react";
import { useSearchParams, useNavigate } from "react-router-dom";
import axios from "axios";
import { useAuthStore } from "@/shared/store/auth";
import { LoginResponse } from "@/shared/store/types";
import { getWatchlist } from "@/features/watchlist/api";

const LoginRedirect = () => {
  const [searchParams] = useSearchParams();
  const code = searchParams.get("oneTimeCode");
  const navigate = useNavigate();

  useEffect(() => {
    if (code) {
      const fetchData = async (code: string) => {
        try {
          const response = await axios.post<LoginResponse>(
            `${import.meta.env.VITE_BASE_URL}/auth/exchange`,
            {
              oneTimeCode: code,
              platform: "web",
            },
          );
          const accessToken = response.data.accessToken;
          useAuthStore.getState().setAccessToken(accessToken);
          await useAuthStore.getState().getSimpleProfile();
          await getWatchlist();
          void navigate("/");
        } catch (error) {
          console.error(error);
        }
      };
      void fetchData(code);
    }
  }, [code, navigate]);
  return (
    <div>
      <h1>로그인 중...</h1>
    </div>
  );
};

export default LoginRedirect;
