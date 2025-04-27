import React from "react";

import { useSignupStore } from "../model/store";
import { Button } from "@/shared/libs/ui/button";
import { Input } from "@/shared/libs/ui/input";
import { Label } from "@/shared/libs/ui/label";
import { Alert, AlertDescription } from "@/shared/libs/ui/alert";
import { AlertCircle, Loader2, ArrowLeft, Check } from "lucide-react";

interface VerificationStepProps {
  onSubmit: () => Promise<void>;
}

export default function VerificationStep({ onSubmit }: VerificationStepProps) {
  const {
    email,
    verificationCode,
    countdown,
    isLoading,
    isEmailSent,
    isEmailVerified,
    error,
    setVerificationCode,
    setStep,
    verifyCode,
    sendVerificationCode,
  } = useSignupStore();

  // 컴포넌트 마운트 시 자동으로 인증코드 발송
  React.useEffect(() => {
    if (!isEmailSent) {
      sendVerificationCode();
    }
  }, [isEmailSent, sendVerificationCode]);

  const handleVerify = async (e: React.FormEvent) => {
    e.preventDefault();
    const success = await verifyCode();
    if (success) {
      // 인증 성공 시 회원가입 완료 처리
      await onSubmit();
    }
  };

  const handleResend = async () => {
    await sendVerificationCode();
  };

  // 남은 시간 포맷팅 (mm:ss)
  const formatTime = (seconds: number) => {
    const mins = Math.floor(seconds / 60);
    const secs = seconds % 60;
    return `${mins.toString().padStart(2, "0")}:${secs.toString().padStart(2, "0")}`;
  };

  return (
    <form onSubmit={handleVerify} className="space-y-6 text-left">
      <div>
        <div className="flex items-center justify-between">
          <Label htmlFor="email">이메일</Label>
          <Button
            type="button"
            variant="ghost"
            size="sm"
            onClick={() => setStep(1)}
            className="h-8 px-2 text-xs"
          >
            <ArrowLeft className="mr-1 size-3" /> 정보 수정
          </Button>
        </div>
        <div className="relative mt-1">
          <Input
            id="email"
            type="email"
            value={email}
            disabled
            className="block w-full bg-gray-50 pr-10"
          />
          {isEmailVerified && (
            <div className="absolute inset-y-0 right-0 flex items-center pr-3">
              <Check className="size-5 text-green-500" />
            </div>
          )}
        </div>
        <p className="mt-1 text-xs text-gray-500">
          {email}로 인증코드가 발송되었습니다. 이메일을 확인해주세요.
        </p>
      </div>

      <div>
        <div className="flex items-center justify-between">
          <Label htmlFor="verification-code">인증코드</Label>
          {countdown > 0 && <span className="text-sm text-primary">{formatTime(countdown)}</span>}
        </div>
        <div className="mt-1">
          <Input
            id="verification-code"
            type="text"
            value={verificationCode}
            onChange={(e) => setVerificationCode(e.target.value)}
            placeholder="6자리 인증코드 입력"
            required
            className="block w-full"
            disabled={isEmailVerified}
          />
        </div>
        {isEmailVerified ? (
          <p className="mt-1 text-xs text-green-600">이메일이 성공적으로 인증되었습니다.</p>
        ) : (
          <p className="mt-1 text-xs text-gray-500">
            이메일로 발송된 6자리 인증코드를 입력해주세요.
            {countdown === 0 && " 인증코드가 만료되었습니다. 재발송해주세요."}
          </p>
        )}
      </div>

      {error && (
        <Alert variant="destructive">
          <AlertCircle className="size-4" />
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )}

      <div className="flex gap-3">
        {isEmailVerified ? (
          <Button type="submit" className="w-full" disabled={isLoading}>
            {isLoading ? (
              <>
                <Loader2 className="mr-2 size-4 animate-spin" /> 가입 중...
              </>
            ) : (
              "회원가입 완료"
            )}
          </Button>
        ) : (
          <>
            <Button
              type="button"
              variant="outline"
              className="flex-1"
              onClick={handleResend}
              disabled={isLoading || (countdown > 0 && countdown < 170)}
            >
              {isLoading && !isEmailVerified ? (
                <Loader2 className="size-4 animate-spin" />
              ) : countdown > 0 && countdown < 170 ? (
                `${formatTime(countdown)} 후 재발송`
              ) : (
                "인증코드 재발송"
              )}
            </Button>
            <Button type="submit" className="flex-1" disabled={isLoading || countdown === 0}>
              {isLoading && isEmailVerified ? (
                <Loader2 className="mr-2 size-4 animate-spin" />
              ) : (
                "인증코드 확인"
              )}
            </Button>
          </>
        )}
      </div>
    </form>
  );
}
