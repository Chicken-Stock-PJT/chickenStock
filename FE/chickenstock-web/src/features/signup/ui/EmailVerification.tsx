import React, { useEffect, useState } from "react";
import { Button } from "@/shared/libs/ui/button";
import { Input } from "@/shared/libs/ui/input";
import { Label } from "@/shared/libs/ui/label";
import { Loader2 } from "lucide-react";
import { authApi } from "../api";

interface VerificationStepProps {
  email: string;
  onSubmit: () => void;
}

const initialSendCode = async (email: string) => {
  try {
    await authApi.sendCode(email);
  } catch (err) {
    // 에러는 sendCode 내부에서 처리됨
  }
};

export default function EmailVerification({ email, onSubmit }: VerificationStepProps) {
  const [verificationCode, setVerificationCode] = useState("");
  const [error, setError] = useState("");
  const [isVerifying, setIsVerifying] = useState(false);
  const [isResending, setIsResending] = useState(false);

  useEffect(() => {
    initialSendCode(email);
  });

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!verificationCode.trim()) {
      setError("인증코드를 입력해주세요.");
      return;
    }

    try {
      setIsVerifying(true);

      const result = await authApi.verifyCode(email, verificationCode);
      if (result.success) {
        onSubmit();
      }
    } catch (err) {
      setError("인증에 실패했습니다. 코드를 다시 확인해주세요.");
      console.log(err);
    } finally {
      setIsVerifying(false);
    }
  };

  const handleResendCode = async () => {
    try {
      setIsResending(true);
      await initialSendCode(email);
      alert("인증코드가 재발송되었습니다.");
    } catch (err) {
      setError("인증코드 재발송에 실패했습니다.");
    } finally {
      setIsResending(false);
    }
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-6 text-left">
      <div className="mb-4 text-center">
        <h3 className="text-lg font-medium">이메일 인증</h3>
        <p className="text-sm text-gray-500">{email}로 전송된 인증코드를 입력해주세요.</p>
      </div>

      <div>
        <Label htmlFor="verificationCode">인증코드</Label>
        <div className="mt-1">
          <Input
            id="verificationCode"
            type="text"
            value={verificationCode}
            onChange={(e) => setVerificationCode(e.target.value)}
            placeholder="인증코드 6자리"
            required
            className={`block w-full ${error ? "border-red-500" : ""}`}
          />
        </div>
        {error && <p className="mt-1 text-xs text-red-600">{error}</p>}
      </div>

      <div className="flex flex-col gap-2">
        <Button type="submit" className="w-full" disabled={isVerifying || isResending}>
          {isVerifying ? (
            <>
              <Loader2 className="mr-2 size-4 animate-spin" /> 인증 중...
            </>
          ) : (
            "인증 완료"
          )}
        </Button>

        <Button
          type="button"
          variant="outline"
          className="w-full"
          onClick={handleResendCode}
          disabled={isVerifying || isResending}
        >
          {isResending ? (
            <>
              <Loader2 className="mr-2 size-4 animate-spin" /> 재발송 중...
            </>
          ) : (
            "인증코드 재발송"
          )}
        </Button>
      </div>
    </form>
  );
}
