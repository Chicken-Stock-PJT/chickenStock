import React, { useEffect, useState } from "react";
import { Button } from "@/shared/libs/ui/button";
import { Input } from "@/shared/libs/ui/input";
import { Label } from "@/shared/libs/ui/label";
import { Eye, EyeOff } from "lucide-react";
import { SignupData } from "../model/types";
import { authApi } from "../api";

interface SignupFormProps {
  onSubmit: (data: SignupData) => void;
}

export default function SignupForm({ onSubmit }: SignupFormProps) {
  // 상태
  const [email, setEmail] = useState("");
  const [name, setName] = useState("");
  const [nickname, setNickname] = useState("");
  const [password1, setPassword1] = useState("");
  const [password2, setPassword2] = useState("");

  const [showPassword1, setShowPassword1] = useState(false);
  const [showPassword2, setShowPassword2] = useState(false);

  const [emailError, setEmailError] = useState("");
  const [nameError, setNameError] = useState("");
  const [nicknameError, setNicknameError] = useState("");
  const [passwordError, setPasswordError] = useState("");
  const [password2Error, setPassword2Error] = useState("");

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    if (!email.trim()) {
      setEmailError("이메일을 입력해주세요.");
    }

    if (!nickname.trim()) {
      setNicknameError("닉네임을 입력해주세요.");
    }

    if (!name.trim()) {
      setNameError("이름을 입력해주세요.");
    }

    if (!password1.trim()) {
      setPasswordError("비밀번호를 입력해주세요.");
    }

    if (!password2.trim()) {
      setPassword2Error("비밀번호를 재입력해주세요.");
    }

    if (!emailError && !nameError && !nicknameError && !passwordError && !password2Error) {
      onSubmit({
        email,
        name,
        nickname,
        password: password1,
      });
    }
  };

  // 입력 핸들러
  const handleEmailChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    e.preventDefault();
    setEmail(e.target.value);
  };

  const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    e.preventDefault();
    setName(e.target.value);
  };

  const handleNicknameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    e.preventDefault();
    setNickname(e.target.value);
  };

  const handlePasswordChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    e.preventDefault();
    setPassword1(e.target.value);
  };

  const handlepassword2Change = (e: React.ChangeEvent<HTMLInputElement>) => {
    e.preventDefault();
    setPassword2(e.target.value);
  };

  // 이메일 유효성검사
  useEffect(() => {
    if (!email) return;

    const emailRegex = /^[a-z0-9._%+-]{1,}@[a-z0-9-]+(\.[a-z0-9-]+)*\.[a-z]{2,}$/;
    if (!emailRegex.test(email)) {
      setEmailError("올바른 이메일 형식이 아닙니다.");
      return;
    }
    setEmailError("");
  }, [email]);

  // 닉네임 유효성 검사
  useEffect(() => {
    if (!nickname) return;

    if (nickname.includes(" ")) {
      setNicknameError("닉네임에 공백을 포함할 수 없습니다.");
      return;
    }

    if (nickname.length < 2 || nickname.length > 10) {
      setNicknameError("닉네임은 2~10자 사이여야 합니다.");
      return;
    }
    setNicknameError("");
  }, [nickname]);

  // 이메일 중복검사
  const validateEmail = async (): Promise<void> => {
    if (emailError) return;
    // 이메일 중복 검사 (API 호출)
    try {
      // API 호출
      const response = await authApi.checkEmail(email);

      if (!response.success) {
        setEmailError(response.message);
        return;
      }

      setEmailError(""); // 에러 초기화

      return;
    } catch (error) {
      setEmailError("이메일 중복 확인 중 오류가 발생했습니다.");
      console.error("이메일 중복 확인 오류:", error);
      return;
    }
  };

  // 닉네임 중복검사
  const validateNickname = async (): Promise<void> => {
    if (nicknameError) return;

    try {
      const response = await authApi.checkNickname(nickname);
      console.log(response);

      if (response.duplicate) {
        setNicknameError(response.message);
        return;
      }

      setNicknameError(""); // 에러 초기화
      return;
    } catch (error) {
      setNicknameError("닉네임 중복 확인 중 오류가 발생했습니다.");
      console.error("닉네임 중복 확인 오류:", error);
      return;
    }
  };

  // 이름 유효성 검사
  useEffect(() => {
    if (!name) return;

    if (name.length < 2) {
      setNameError("이름은 최소 2자 이상이어야 합니다.");
      return;
    }

    setNameError(""); // 에러 초기화
  }, [name]);

  // 비밀번호 유효성 검사
  useEffect(() => {
    if (!password1) return;

    const passwordRegex = /^((?=.*\d)(?=.*[a-zA-z])(?=.*[\W_]).{8,16})$/;
    if (!passwordRegex.test(password1)) {
      setPasswordError("영문, 숫자, 특수문자를 조합해서 입력해주세요. (8~16자)");
      return;
    }

    setPasswordError(""); // 에러 초기화
  }, [password1]);

  // 비밀번호 확인
  useEffect(() => {
    if (password1 !== password2) {
      setPassword2Error("비밀번호가 일치하지 않습니다.");
      return;
    }

    setPassword2Error(""); // 에러 초기화
  }, [password1, password2]);

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
            onBlur={() => void validateEmail()}
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
            onBlur={() => void validateNickname()}
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
            type={showPassword2 ? "text" : "password"}
            required
            value={password2}
            onChange={handlepassword2Change}
            className="block w-full pr-10"
          />
          <div
            className="absolute inset-y-0 right-0 flex cursor-pointer items-center px-3"
            onClick={() => setShowPassword2(!showPassword2)}
          >
            {showPassword2 ? (
              <EyeOff className="size-5 text-gray-400" />
            ) : (
              <Eye className="size-5 text-gray-400" />
            )}
          </div>
        </div>
        {password2Error && <p className="mt-1 text-xs text-red-600">{password2Error}</p>}
      </div>

      <Button type="submit" className="w-full">
        {/* <Button className="w-full"> */}
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
