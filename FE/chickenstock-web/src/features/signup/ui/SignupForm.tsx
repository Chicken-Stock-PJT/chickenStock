import React, { useState } from "react";

import { useSignupStore } from "../model/store";
import { Button } from "@/shared/libs/ui/button";
import { Input } from "@/shared/libs/ui/input";
import { Label } from "@/shared/libs/ui/label";
import { Alert, AlertDescription } from "@/shared/libs/ui/alert";
import { AlertCircle, Loader2, Eye, EyeOff } from "lucide-react";

export default function SignupForm() {
  // 상태
  const [email, setEmail] = useState("");
  const [name, setName] = useState("");
  const [nickname, setNickname] = useState("");
  const [password1, setPassword1] = useState("");
  const [password2, setPassword2] = useState("");

  const [showPassword1, setShowPassword1] = useState(false);
  const [showpassword2, setShowpassword2] = useState(false);

  const emailError = false;
  const nameError = false;
  const nicknameError = false;
  const passwordError = false;

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    // form 제출
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
    setPassword1(e.target.value);
  };

  const handlepassword2Change = (e: React.ChangeEvent<HTMLInputElement>) => {
    setPassword2(e.target.value);
  };

  // 이메일, 닉네임: 중복검사 + 유효성

  // 유효성검사
  const handleValidateEmail = () => {
    return;
  };

  const handleValidateName = () => {
    return;
  };

  const handleValidateNickname = () => {
    return;
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
            placeholder="이메일"
            required
            className={`block w-full ${emailError ? "border-red-500" : ""}`}
            onBlur={handleValidateEmail}
          />
        </div>
        {emailError ? <p className="mt-1 text-xs text-red-600">{emailError}</p> : <></>}
        <p className="mt-1 text-xs text-gray-500">
          입력하신 이메일로 인증코드가 발송됩니다. 실제 사용 중인 이메일을 입력해주세요.
        </p>
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
            placeholder="이름"
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
            placeholder="닉네임"
            className={`block w-full ${nicknameError ? "border-red-500" : ""}`}
            onBlur={handleValidateNickname}
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
        <Label htmlFor="password1">비밀번호</Label>
        <div className="relative mt-1">
          <Input
            id="password1"
            name="password1"
            type={showPassword1 ? "text" : "password"}
            required
            value={password1}
            onChange={handlePasswordChange}
            className={`block w-full pr-10 ${passwordError ? "border-red-500" : ""}`}
          />
          <div
            className="absolute inset-y-0 right-0 flex cursor-pointer items-center px-3"
            onClick={() => setShowPassword1(!showPassword1)}
          >
            {showPassword1 ? (
              <EyeOff className="size-5 text-gray-400" />
            ) : (
              <Eye className="size-5 text-gray-400" />
            )}
          </div>
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
        <Label htmlFor="password2">비밀번호 확인</Label>
        <div className="relative mt-1">
          <Input
            id="password2"
            name="password2"
            type={showpassword2 ? "text" : "password"}
            required
            value={password2}
            onChange={handlepassword2Change}
            className="block w-full pr-10"
          />
          <div
            className="absolute inset-y-0 right-0 flex cursor-pointer items-center px-3"
            onClick={() => setShowpassword2(!showpassword2)}
          >
            {showpassword2 ? (
              <EyeOff className="size-5 text-gray-400" />
            ) : (
              <Eye className="size-5 text-gray-400" />
            )}
          </div>
        </div>
      </div>

      {/* {error && (
        <Alert variant="destructive">
          <AlertCircle className="size-4" />
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )} */}

      {/* <Button type="submit" className="w-full" disabled={isLoading || isCheckingEmail}> */}
      <Button className="w-full">
        다음
        {/* {isLoading || isCheckingEmail ? (
          <>
            <Loader2 className="mr-2 size-4 animate-spin" />{" "}
            {isCheckingEmail ? "이메일 확인 중..." : "처리 중..."}
          </>
        ) : (
          "다음 단계로"
        )} */}
      </Button>
    </form>
  );
}
