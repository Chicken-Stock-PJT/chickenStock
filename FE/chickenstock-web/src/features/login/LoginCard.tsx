import { Button } from "@/shared/libs/ui/button";
import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Input } from "@/shared/libs/ui/input";
import googleLogin from "@/assets/google.svg";
import kakaoLogin from "@/assets/kakao.svg";
import naverLogin from "@/assets/naver.svg";
import { Link, useNavigate } from "react-router-dom";
import { useState, useEffect } from "react";
import { useAuthStore } from "@/shared/store/auth";

const LoginCard = () => {
  const navigate = useNavigate();
  const [formData, setFormData] = useState({
    email: "",
    password: "",
  });

  useEffect(() => {
    const { accessToken } = useAuthStore.getState();
    if (accessToken) {
      void useAuthStore.getState().getSimpleProfile();
    }
  }, []);

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { id, value } = e.target;
    setFormData((prev) => ({
      ...prev,
      [id]: value,
    }));
  };

  const handleSubmit = async (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    try {
      const response = await useAuthStore.getState().login(formData.email, formData.password);
      useAuthStore.getState().setAccessToken(response.accessToken);
      await useAuthStore.getState().getSimpleProfile();
      void navigate("/");
    } catch (err) {
      console.error(err);
      alert("로그인에 실패했습니다.");
    }
  };

  return (
    <Card className="mx-auto w-[480px]">
      <CardHeader>
        <CardTitle className="text-2xl">로그인</CardTitle>
      </CardHeader>
      <CardContent>
        <form onSubmit={(e) => void handleSubmit(e)}>
          <div className="mt-6 flex flex-col gap-2">
            <Input
              id="email"
              type="email"
              placeholder="이메일"
              required
              value={formData.email}
              onChange={handleInputChange}
            />
            <Input
              id="password"
              type="password"
              placeholder="비밀번호"
              required
              value={formData.password}
              onChange={handleInputChange}
            />
          </div>
          <div className="my-6">
            <Button className="w-full" type="submit">
              이메일로 로그인
            </Button>
          </div>
        </form>
        <hr />
      </CardContent>
      <CardFooter>
        <div className="mx-20  w-full space-y-8">
          <div className="flex justify-between gap-4">
            <img
              src={googleLogin}
              alt="구글 로그인"
              className="w-[56px] cursor-pointer fill-current text-primary-400"
              onClick={() => {
                window.location.href = `${import.meta.env.VITE_BASE_URL}/oauth2/authorization/google`;
              }}
            />
            <img
              src={kakaoLogin}
              alt="카카오 로그인"
              className="w-[56px] cursor-pointer fill-current text-primary-400"
              onClick={() => {
                window.location.href = `${import.meta.env.VITE_BASE_URL}/oauth2/authorization/kakao`;
              }}
            />
            <img
              src={naverLogin}
              alt="네이버 로그인"
              className="w-[56px] cursor-pointer fill-current text-primary-400"
              onClick={() => {
                window.location.href = `${import.meta.env.VITE_BASE_URL}/oauth2/authorization/naver`;
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
