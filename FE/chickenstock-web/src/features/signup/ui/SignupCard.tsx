import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Link, useNavigate } from "react-router-dom";
import EmailVerification from "./EmailVerification";
import SignupForm from "./SignupForm";
import { useState } from "react";
import { authApi } from "../api";

interface SignupForm {
  email: string;
  name: string;
  nickname: string;
  password: string;
}

const SignupCard = () => {
  const [step, setStep] = useState(1);
  const [formData, setFormData] = useState({
    email: "",
    name: "",
    nickname: "",
    password: "",
  });
  const navigate = useNavigate();

  const handleFormSubmit = ({ ...data }: SignupForm) => {
    setFormData(data);
    setStep(2);
  };

  const handleVerificationSubmit = async () => {
    // // 회원가입 api 요청
    try {
      const response = await authApi.signup(formData);

      console.log(response);
    } catch (err) {
      alert(err);
    }

    void navigate("/login");
    console.log(formData);
  };

  return (
    <Card className="mx-auto w-[400px]">
      <CardHeader>
        <CardTitle>회원가입</CardTitle>
      </CardHeader>
      <CardContent className="items-center gap-4 space-y-4">
        {step === 1 && <SignupForm onSubmit={handleFormSubmit} />}
        {step === 2 && (
          <EmailVerification email={formData.email} onSubmit={handleVerificationSubmit} />
        )}
      </CardContent>
      <CardFooter className="flex justify-center gap-2 text-center text-sm">
        <span className="text-gray-500">이미 계정이 있으신가요?</span>
        <Link to={"/login"}>로그인하기</Link>
      </CardFooter>
    </Card>
  );
};

export default SignupCard;
