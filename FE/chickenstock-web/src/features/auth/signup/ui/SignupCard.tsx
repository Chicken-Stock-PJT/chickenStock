import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/shared/libs/ui/card";
import { Link, useNavigate } from "react-router-dom";
import EmailVerification from "./EmailVerification";
import SignupForm from "./SignupForm";
import { useState } from "react";
import { authApi } from "../api";
import MyAlertDialog from "@/widgets/alert/MyAlertDialog";

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
  const [isAlertOpen, setIsAlertOpen] = useState(false);
  const [alertTitle, setAlertTitle] = useState("");
  const [alertMessage, setAlertMessage] = useState("");
  const [alertAction, setAlertAction] = useState<() => void>(() => void 0);
  const navigate = useNavigate();

  const handleFormSubmit = ({ ...data }: SignupForm) => {
    setFormData(data);
    setStep(2);
  };

  const handleVerificationSubmit = async (): Promise<void> => {
    try {
      const response = await authApi.signup(formData);
      if (response) {
        setIsAlertOpen(true);
        setAlertTitle("회원가입 완료");
        setAlertMessage("회원가입이 완료되었습니다. 로그인 페이지로 이동합니다.");
        setAlertAction(() => () => void navigate("/login"));
      }
    } catch (err) {
      console.error(err);
      setIsAlertOpen(true);
      setAlertTitle("회원가입 실패");
      setAlertMessage("회원가입에 실패했습니다. 다시 시도해주세요.");
      setAlertAction(() => () => void navigate("/signup"));
    }
  };

  return (
    <>
      <Card className="mx-auto mb-40 mt-20 w-[400px]">
        <CardHeader>
          <CardTitle>회원가입</CardTitle>
        </CardHeader>
        <CardContent className="items-center gap-4 space-y-4">
          {step === 1 && <SignupForm onSubmit={handleFormSubmit} />}
          {step === 2 && (
            <EmailVerification
              email={formData.email}
              onSubmit={() => void handleVerificationSubmit()}
            />
          )}
        </CardContent>
        <CardFooter className="flex justify-center gap-2 text-center text-sm">
          <span className="text-gray-500">이미 계정이 있으신가요?</span>
          <Link to={"/login"}>로그인하기</Link>
        </CardFooter>
      </Card>
      <MyAlertDialog
        isOpen={isAlertOpen}
        setIsOpen={setIsAlertOpen}
        title={alertTitle}
        description={alertMessage}
        action={alertAction}
        actionText="확인"
      />
    </>
  );
};

export default SignupCard;
