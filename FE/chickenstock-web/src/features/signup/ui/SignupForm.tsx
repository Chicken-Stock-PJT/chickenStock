"use client";

import React from "react";

import { useSignupStore } from "../model/store";
import { Button } from "@/shared/libs/ui/button";
import { Input } from "@/shared/libs/ui/input";
import { Label } from "@/shared/libs/ui/label";
import { Alert, AlertDescription } from "@/shared/libs/ui/alert";
import { AlertCircle, Loader2, Eye, EyeOff } from "lucide-react";

export default function SignupForm() {
  const {
    email,
    name,
    nickname,
    password,
    confirmPassword,
    isLoading,
    isCheckingEmail,
    error,
    emailError,
    nameError,
    nicknameError,
    passwordError,
    setEmail,
    setName,
    setNickname,
    setPassword,
    setConfirmPassword,
    proceedToVerification,
  } = useSignupStore();

  const [showPassword, setShowPassword] = React.useState(false);
  const [showConfirmPassword, setShowConfirmPassword] = React.useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    await proceedToVerification();
  };

  // 입력 핸들러 함수들
  const handleEmailChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setEmail(e.target.value);
  };

  const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setName(e.target.value);
  };

  const handleNicknameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setNickname(e.target.value);
  };

  const handlePasswordChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setPassword(e.target.value);
  };

  const handleConfirmPasswordChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setConfirmPassword(e.target.value);
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-6 text-left">
      <div>
        <Label htmlFor="email">이메일</Label>
        <div className="mt-1">
          <Input
            id="email"
            type="email"
            value={email}
            onChange={handleEmailChange}
            placeholder="example@email.com"
            required
            className={`block w-full ${emailError ? "border-red-500" : ""}`}
          />
        </div>
        {emailError ? (
          <p className="mt-1 text-xs text-red-600">{emailError}</p>
        ) : (
          <p className="mt-1 text-xs text-gray-500">
            입력하신 이메일로 인증코드가 발송됩니다. 실제 사용 중인 이메일을 입력해주세요.
          </p>
        )}
      </div>

      <div>
        <Label htmlFor="name">이름</Label>
        <div className="mt-1">
          <Input
            id="name"
            name="name"
            type="text"
            required
            value={name}
            onChange={handleNameChange}
            placeholder="홍길동"
            className={`block w-full ${nameError ? "border-red-500" : ""}`}
          />
        </div>
        {nameError && <p className="mt-1 text-xs text-red-600">{nameError}</p>}
      </div>

      <div>
        <Label htmlFor="nickname">닉네임</Label>
        <div className="mt-1">
          <Input
            id="nickname"
            name="nickname"
            type="text"
            required
            value={nickname}
            onChange={handleNicknameChange}
            placeholder="닉네임을 입력하세요"
            className={`block w-full ${nicknameError ? "border-red-500" : ""}`}
          />
        </div>
        {nicknameError ? (
          <p className="mt-1 text-xs text-red-600">{nicknameError}</p>
        ) : (
          <p className="mt-1 text-xs text-gray-500">
            다른 사용자에게 표시되는 이름입니다. 나중에 변경할 수 있습니다.
          </p>
        )}
      </div>

      <div>
        <Label htmlFor="password">비밀번호</Label>
        <div className="relative mt-1">
          <Input
            id="password"
            name="password"
            type={showPassword ? "text" : "password"}
            required
            value={password}
            onChange={handlePasswordChange}
            className={`block w-full pr-10 ${passwordError ? "border-red-500" : ""}`}
          />
          <button
            type="button"
            className="absolute inset-y-0 right-0 flex items-center pr-3"
            onClick={() => setShowPassword(!showPassword)}
          >
            {showPassword ? (
              <EyeOff className="size-5 text-gray-400" />
            ) : (
              <Eye className="size-5 text-gray-400" />
            )}
          </button>
        </div>
        {passwordError ? (
          <p className="mt-1 text-xs text-red-600">{passwordError}</p>
        ) : (
          <p className="mt-1 text-xs text-gray-500">
            8자 이상이며, 문자, 숫자, 특수문자를 포함해야 합니다.
          </p>
        )}
      </div>

      <div>
        <Label htmlFor="confirmPassword">비밀번호 확인</Label>
        <div className="relative mt-1">
          <Input
            id="confirmPassword"
            name="confirmPassword"
            type={showConfirmPassword ? "text" : "password"}
            required
            value={confirmPassword}
            onChange={handleConfirmPasswordChange}
            className="block w-full pr-10"
          />
          <button
            type="button"
            className="absolute inset-y-0 right-0 flex items-center pr-3"
            onClick={() => setShowConfirmPassword(!showConfirmPassword)}
          >
            {showConfirmPassword ? (
              <EyeOff className="size-5 text-gray-400" />
            ) : (
              <Eye className="size-5 text-gray-400" />
            )}
          </button>
        </div>
      </div>

      {error && (
        <Alert variant="destructive">
          <AlertCircle className="size-4" />
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )}

      <Button type="submit" className="w-full" disabled={isLoading || isCheckingEmail}>
        {isLoading || isCheckingEmail ? (
          <>
            <Loader2 className="mr-2 size-4 animate-spin" />{" "}
            {isCheckingEmail ? "이메일 확인 중..." : "처리 중..."}
          </>
        ) : (
          "다음 단계로"
        )}
      </Button>
    </form>
  );
}
