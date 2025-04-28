import { Button } from "@/shared/libs/ui/button";
import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Input } from "@/shared/libs/ui/input";
import googleLogin from "@/assets/google.svg";
import kakaoLogin from "@/assets/kakao.svg";
import naverLogin from "@/assets/naver.svg";
import { Link } from "react-router-dom";
import { useState } from "react";

const LoginCard = () => {
  const [formData, setFormData] = useState({
    email: "",
    password: "",
  });

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { id, value } = e.target;
    setFormData((prev) => ({
      ...prev,
      [id]: value,
    }));
  };

  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    // try {
    //   const response = await axios.post("/api/login", formData);
    //   // 로그인 성공 시 처리
    //   console.log("로그인 성공:", response.data);
    //   // 여기에 토큰 저장이나 리다이렉트 로직을 추가할 수 있습니다
    // } catch (error) {
    //   // 로그인 실패 시 처리
    //   console.error("로그인 실패:", error);
    // }
    console.log(`이메일: ${formData.email} / 비밀번호: ${formData.password}`);
  };

  return (
    <Card className="mx-auto w-[480px]">
      <CardHeader>
        <CardTitle className="text-2xl">로그인</CardTitle>
      </CardHeader>
      <CardContent>
        <form onSubmit={handleSubmit}>
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
            />
            <img
              src={kakaoLogin}
              alt="카카오 로그인"
              className="w-[56px] cursor-pointer fill-current text-primary-400"
            />
            <img
              src={naverLogin}
              alt="네이버 로그인"
              className="w-[56px] cursor-pointer fill-current text-primary-400"
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
