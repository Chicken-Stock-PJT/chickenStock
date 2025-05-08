import {
  Card,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@/shared/libs/ui/card";
import { Input } from "@/shared/libs/ui/input";
import { Button } from "@/shared/libs/ui/button";
import { useState } from "react";
import { findPassword } from "@/features/auth/find/api";
import { Link, useNavigate } from "react-router-dom";

const FindPasswordPage = () => {
  const [email, setEmail] = useState("");
  const navigate = useNavigate();

  const handleSubmit = async (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    try {
      const res = await findPassword(email);
      alert(res.message);
      void navigate("/login");
    } catch (error) {
      alert(error);
    }
  };

  const handleEmailChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setEmail(e.target.value);
  };

  return (
    <div>
      <Card className="mx-auto mb-40 mt-20 w-[400px]">
        <CardHeader>
          <CardTitle className="text-2xl">비밀번호 찾기</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex flex-col gap-10">
            <div>
              <CardDescription>회원가입에 사용한 이메일을 입력해주세요.</CardDescription>
              <CardDescription>이메일로 임시 비밀번호가 발송됩니다.</CardDescription>
            </div>
            <form onSubmit={(e) => void handleSubmit(e)} className="flex flex-col gap-4">
              <Input type="email" placeholder="이메일" value={email} onChange={handleEmailChange} />
              <Button type="submit">임시 비밀번호 받기</Button>
            </form>
          </div>
        </CardContent>
        <CardFooter>
          <div className="mt-4 flex w-full justify-center gap-2 text-center text-sm">
            <span className="text-gray-500">아직 계정이 없으신가요?</span>
            <Link to={"/signup"} className="underline">
              회원가입
            </Link>
          </div>
        </CardFooter>
      </Card>
    </div>
  );
};

export default FindPasswordPage;
