import { create } from "zustand";

interface SignupState {
  // 현재 단계
  step: number;

  // 사용자 정보
  email: string;
  name: string;
  nickname: string;
  password: string;
  confirmPassword: string;

  // 인증 관련 상태
  verificationCode: string;
  isEmailSent: boolean;
  isEmailVerified: boolean;
  actualCode: string;
  countdown: number;

  // 로딩 및 에러 상태
  isLoading: boolean;
  isCheckingEmail: boolean;
  error: string;
  emailError: string;
  nameError: string;
  nicknameError: string;
  passwordError: string;

  // 액션
  setStep: (step: number) => void;
  setEmail: (email: string) => void;
  setName: (name: string) => void;
  setNickname: (nickname: string) => void;
  setPassword: (password: string) => void;
  setConfirmPassword: (password: string) => void;
  setVerificationCode: (code: string) => void;
  setIsEmailSent: (sent: boolean) => void;
  setIsEmailVerified: (verified: boolean) => void;
  setActualCode: (code: string) => void;
  setCountdown: (seconds: number) => void;
  setIsLoading: (loading: boolean) => void;
  setIsCheckingEmail: (checking: boolean) => void;
  setError: (error: string) => void;
  setEmailError: (error: string) => void;
  setNameError: (error: string) => void;
  setNicknameError: (error: string) => void;
  setPasswordError: (error: string) => void;

  // 복합 액션
  resetForm: () => void;
  validateEmail: () => Promise<boolean>;
  validateForm: () => boolean;
  proceedToVerification: () => Promise<boolean>;
  sendVerificationCode: () => Promise<void>;
  verifyCode: () => Promise<boolean>;
  submitSignup: () => Promise<boolean>;
}

export const useSignupStore = create<SignupState>((set, get) => ({
  // 초기 상태
  step: 1,
  email: "",
  name: "",
  nickname: "",
  password: "",
  confirmPassword: "",
  verificationCode: "",
  isEmailSent: false,
  isEmailVerified: false,
  actualCode: "",
  countdown: 0,
  isLoading: false,
  isCheckingEmail: false,
  error: "",
  emailError: "",
  nameError: "",
  nicknameError: "",
  passwordError: "",

  // 기본 액션
  setStep: (step) => set({ step }),
  setEmail: (email) => set({ email, emailError: "" }),
  setName: (name) => set({ name, nameError: "" }),
  setNickname: (nickname) => set({ nickname, nicknameError: "" }),
  setPassword: (password) => set({ password, passwordError: "" }),
  // 여기가 문제였습니다 - confirmPassword와 error 리셋 추가
  setConfirmPassword: (confirmPassword) => set({ confirmPassword, error: "" }),
  setVerificationCode: (code) => set({ verificationCode: code }),
  setIsEmailSent: (sent) => set({ isEmailSent: sent }),
  setIsEmailVerified: (verified) => set({ isEmailVerified: verified }),
  setActualCode: (code) => set({ actualCode: code }),
  setCountdown: (seconds) => set({ countdown: seconds }),
  setIsLoading: (loading) => set({ isLoading: loading }),
  setIsCheckingEmail: (checking) => set({ isCheckingEmail: checking }),
  setError: (error) => set({ error }),
  setEmailError: (error) => set({ emailError: error }),
  setNameError: (error) => set({ nameError: error }),
  setNicknameError: (error) => set({ nicknameError: error }),
  setPasswordError: (error) => set({ passwordError: error }),

  // 나머지 코드는 동일하게 유지...
  resetForm: () =>
    set({
      step: 1,
      email: "",
      name: "",
      nickname: "",
      password: "",
      confirmPassword: "",
      verificationCode: "",
      isEmailSent: false,
      isEmailVerified: false,
      actualCode: "",
      countdown: 0,
      isLoading: false,
      isCheckingEmail: false,
      error: "",
      emailError: "",
      nameError: "",
      nicknameError: "",
      passwordError: "",
    }),

  // 이메일 유효성 검사 및 중복 체크
  validateEmail: async () => {
    const { email, setEmailError, setIsCheckingEmail } = get();

    // 이메일 형식 검사
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
      setEmailError("유효한 이메일 주소를 입력해주세요.");
      return false;
    }

    setIsCheckingEmail(true);

    try {
      // 실제 구현에서는 여기에 이메일 중복 체크 API 호출
      console.log("이메일 중복 체크:", email);

      // 이메일 중복 체크 시뮬레이션 (1초 후)
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // 중복 체크 성공 (실제 구현에서는 API 응답에 따라 처리)
      setIsCheckingEmail(false);
      return true;
    } catch (err) {
      setIsCheckingEmail(false);
      setEmailError("이메일 중복 체크에 실패했습니다. 다시 시도해주세요.");
      return false;
    }
  },

  // 전체 폼 유효성 검사
  validateForm: () => {
    const {
      email,
      name,
      nickname,
      password,
      confirmPassword,
      setEmailError,
      setNameError,
      setNicknameError,
      setPasswordError,
      setError,
    } = get();

    let isValid = true;

    // 이메일 검사
    if (!email) {
      setEmailError("이메일을 입력해주세요.");
      isValid = false;
    } else {
      const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
      if (!emailRegex.test(email)) {
        setEmailError("유효한 이메일 주소를 입력해주세요.");
        isValid = false;
      }
    }

    // 이름 검사
    if (!name) {
      setNameError("이름을 입력해주세요.");
      isValid = false;
    }

    // 닉네임 검사
    if (!nickname) {
      setNicknameError("닉네임을 입력해주세요.");
      isValid = false;
    }

    // 비밀번호 검사
    if (!password) {
      setPasswordError("비밀번호를 입력해주세요.");
      isValid = false;
    } else {
      const passwordRegex = /^(?=.*[A-Za-z])(?=.*\d)(?=.*[@$!%*#?&])[A-Za-z\d@$!%*#?&]{8,}$/;
      if (!passwordRegex.test(password)) {
        setPasswordError("비밀번호는 8자 이상이며, 문자, 숫자, 특수문자를 포함해야 합니다.");
        isValid = false;
      }
    }

    // 비밀번호 확인 검사
    if (password !== confirmPassword) {
      setError("비밀번호가 일치하지 않습니다.");
      isValid = false;
    }

    return isValid;
  },

  // 인증 단계로 진행
  proceedToVerification: async () => {
    const { validateForm, validateEmail, setError, setStep } = get();

    // 폼 유효성 검사
    if (!validateForm()) {
      return false;
    }

    // 이메일 중복 체크
    const isEmailValid = await validateEmail();
    if (!isEmailValid) {
      return false;
    }

    // 인증 단계로 이동
    setError("");
    setStep(2);
    return true;
  },

  // 이메일 인증코드 발송
  sendVerificationCode: async () => {
    const { email, setIsLoading, setError, setIsEmailSent, setActualCode, setCountdown } = get();

    setError("");
    setIsLoading(true);

    try {
      // 실제 구현에서는 여기에 인증코드 발송 API 호출
      console.log("인증코드 발송:", email);

      // 인증코드 발송 시뮬레이션 (1초 후)
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // 6자리 랜덤 숫자 생성
      const randomCode = Math.floor(100000 + Math.random() * 900000).toString();
      console.log("인증코드:", randomCode); // 개발 환경에서만 표시

      setActualCode(randomCode);
      setIsEmailSent(true);

      // 카운트다운 시작 (3분 = 180초)
      setCountdown(180);
      const timer = setInterval(() => {
        const currentCountdown = get().countdown;
        if (currentCountdown <= 1) {
          clearInterval(timer);
          set({ countdown: 0 });
        } else {
          set({ countdown: currentCountdown - 1 });
        }
      }, 1000);

      setIsLoading(false);
    } catch (err) {
      setIsLoading(false);
      setError("인증코드 발송에 실패했습니다. 다시 시도해주세요.");
    }
  },

  // 인증코드 확인
  verifyCode: async () => {
    const { verificationCode, actualCode, setIsLoading, setError, setIsEmailVerified } = get();

    if (!verificationCode) {
      setError("인증코드를 입력해주세요.");
      return false;
    }

    setError("");
    setIsLoading(true);

    try {
      // 실제 구현에서는 여기에 인증코드 확인 API 호출
      console.log("인증코드 확인:", verificationCode);

      // 인증코드 확인 시뮬레이션 (1초 후)
      await new Promise((resolve) => setTimeout(resolve, 1000));

      if (verificationCode === actualCode) {
        // 인증 성공
        setIsEmailVerified(true);
        setIsLoading(false);
        return true;
      } else {
        // 인증 실패
        setIsLoading(false);
        setError("인증코드가 일치하지 않습니다. 다시 확인해주세요.");
        return false;
      }
    } catch (err) {
      setIsLoading(false);
      setError("인증에 실패했습니다. 다시 시도해주세요.");
      return false;
    }
  },

  // 회원가입 제출
  submitSignup: async () => {
    const { email, name, nickname, password, isEmailVerified, setIsLoading, setError } = get();

    // 이메일 인증 확인
    if (!isEmailVerified) {
      setError("이메일 인증이 필요합니다.");
      return false;
    }

    setError("");
    setIsLoading(true);

    try {
      // 실제 구현에서는 여기에 회원가입 API 호출
      console.log("회원가입 시도:", { email, name, nickname, password });

      // 회원가입 성공 시뮬레이션 (2초 후)
      await new Promise((resolve) => setTimeout(resolve, 2000));

      setIsLoading(false);
      return true;
    } catch (err) {
      setIsLoading(false);
      setError("회원가입에 실패했습니다. 다시 시도해주세요.");
      return false;
    }
  },
}));
