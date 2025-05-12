import { Button } from "@/shared/libs/ui/button";
import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Input } from "@/shared/libs/ui/input";
import googleLogin from "@/assets/google.svg";
import kakaoLogin from "@/assets/kakao.svg";
import naverLogin from "@/assets/naver.svg";
import { Link, useNavigate } from "react-router-dom";
import { useState } from "react";
import { useAuthStore } from "@/shared/store/auth";
import { useGetWatchlist } from "@/features/watchlist/model/queries";
import { AxiosError } from "axios";
import { Alert, AlertDescription } from "@/shared/libs/ui/alert";
import { AlertCircle } from "lucide-react";

const LoginCard = () => {
  const navigate = useNavigate();
  const [formData, setFormData] = useState({
    email: "",
    password: "",
  });
  const [showAlert, setShowAlert] = useState(false);
  const [alertMessage, setAlertMessage] = useState("");

  const { refetch: refetchWatchlist } = useGetWatchlist();

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { id, value } = e.target;
    setFormData((prev) => ({
      ...prev,
      [id]: value,
    }));
    // 입력 시작하면 알림 숨기기
    if (showAlert) setShowAlert(false);
  };

  const handleSubmit = async (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    try {
      const response = await useAuthStore.getState().login(formData.email, formData.password);
      useAuthStore.getState().setAccessToken(response.accessToken);
      await useAuthStore.getState().getSimpleProfile();
      await refetchWatchlist();
      void navigate(localStorage.getItem("redirectUrl") ?? "/");
      localStorage.removeItem("redirectUrl");
    } catch (err) {
      console.error(err);
      if (err instanceof AxiosError && err.response?.data?.code === "AUTH-E004") {
        setAlertMessage("이메일 또는 비밀번호가 일치하지 않습니다.");
        setShowAlert(true);
      } else {
        // 기타 오류 처리
        setAlertMessage("로그인 중 오류가 발생했습니다. 다시 시도해주세요.");
        setShowAlert(true);
      }
    }
  };

  return (
    <Card className="mx-auto w-[400px]">
      <CardHeader>
        <CardTitle className="text-2xl">로그인</CardTitle>
      </CardHeader>
      <CardContent>
        {/* 로그인 실패 시 Alert 표시 */}
        {showAlert && (
          <Alert variant="destructive">
            <div className="flex gap-2">
              <AlertCircle className="h-4 w-4 my-auto" />
              <AlertDescription className="text-left">{alertMessage}</AlertDescription>
            </div>
          </Alert>
        )}
        <form onSubmit={handleSubmit}>
          <div className="mt-6 flex flex-col gap-2">
            <Input
              id="email"
              type="email"
              placeholder="이메일"
              value={formData.email}
              onChange={handleInputChange}
              required
              onFocus={() => setShowAlert(false)}
            />
            <Input
              id="password"
              type="password"
              placeholder="비밀번호"
              value={formData.password}
              onChange={handleInputChange}
              required
              onFocus={() => setShowAlert(false)}
            />
          </div>
          <div className="mt-4 flex justify-end underline">
            <Link to={"/find/password"} className="text-sm text-gray-500">
              비밀번호 찾기
            </Link>
          </div>

          <div className="mb-6 mt-8">
            <Button type="submit" className="w-full">
              이메일로 로그인
            </Button>
          </div>
        </form>
        <div className="flex items-center">
          <div className="flex-grow border-t border-gray-200 dark:border-gray-700"></div>
          <span className="flex-shrink mx-4 text-gray-400 dark:text-gray-200 text-sm">
            소셜 로그인
          </span>
          <div className="flex-grow border-t border-gray-200 dark:border-gray-700"></div>
        </div>
      </CardContent>
      <CardFooter>
        <div className="w-full space-y-8">
          <div className="mx-12 flex justify-between gap-4">
            <img
              src={googleLogin}
              alt="구글 로그인"
              className="w-[56px] cursor-pointer fill-current text-primary-400"
              onClick={() => {
                window.location.href = `${import.meta.env.VITE_BASE_URL}/auth/oauth2/redirect/google?redirectUri=${encodeURIComponent(
                  import.meta.env.VITE_LOGIN_REDIRECT_URI,
                )}`;
              }}
            />
            <img
              src={kakaoLogin}
              alt="카카오 로그인"
              className="w-[56px] cursor-pointer fill-current text-primary-400"
              onClick={() => {
                window.location.href = `${import.meta.env.VITE_BASE_URL}/auth/oauth2/redirect/kakao?redirectUri=${encodeURIComponent(
                  import.meta.env.VITE_LOGIN_REDIRECT_URI,
                )}`;
              }}
            />
            <img
              src={naverLogin}
              alt="네이버 로그인"
              className="w-[56px] cursor-pointer fill-current text-primary-400"
              onClick={() => {
                window.location.href = `${import.meta.env.VITE_BASE_URL}/auth/oauth2/redirect/naver?redirectUri=${encodeURIComponent(
                  import.meta.env.VITE_LOGIN_REDIRECT_URI,
                )}`;
              }}
            />
          </div>
          <div className="flex justify-center gap-2 text-center text-sm">
            <span className="text-gray-500">아직 계정이 없으신가요?</span>
            <Link to={"/signup"} className="underline">
              회원가입
            </Link>
          </div>
        </div>
      </CardFooter>
    </Card>
  );
};

export default LoginCard;
