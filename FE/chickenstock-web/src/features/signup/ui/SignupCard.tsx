import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Link, useNavigate } from "react-router-dom";
import EmailVerification from "./EmailVerification";
import { useSignupStore } from "../model/store";
import SignupForm from "./SignupForm";

const SignupCard = () => {
  const navigate = useNavigate();
  const { step, submitSignup, resetForm } = useSignupStore();

  const handleSubmit = async () => {
    const success = await submitSignup();
    if (success) {
      // 회원가입 성공 시 로그인 페이지로 이동
      resetForm();
      navigate("/login");
    }
  };
  return (
    <Card className="mx-auto w-[400px]">
      <CardHeader>
        <CardTitle>회원가입</CardTitle>
      </CardHeader>
      <CardContent className="items-center gap-4 space-y-4">
        {step === 1 && <SignupForm />}
        {step === 2 && <EmailVerification onSubmit={handleSubmit} />}
      </CardContent>
      <CardFooter className="flex justify-center gap-2 text-center text-sm">
        <span className="text-gray-500">이미 계정이 있으신가요?</span>
        <Link to={"/login"}>로그인하기</Link>
      </CardFooter>
    </Card>
  );
};

export default SignupCard;
