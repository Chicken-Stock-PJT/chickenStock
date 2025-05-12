import os
from typing import Dict, Optional, List, Any
from pydantic_settings import BaseSettings, SettingsConfigDict

class AccountConfig(BaseSettings):
    """개별 계정 설정을 위한 모델"""
    email: str
    password: str
    strategy: str = "ENVELOPE"  # 기본값은 ENVELOPE

class Settings(BaseSettings):
    # API 설정
    API_AUTH_URL: str = "https://api.kiwoom.com"  # 키움증권 API 기본 URL
    API_BASE_URL: str = "https://api.kiwoom.com"
    BACKEND_API_URL: str = "https://k12a106.p.ssafy.io"  # 백엔드 API URL
    WEBSOCKET_API_URL: str = "wss://api.kiwoom.com:10000"
    
    # 인증 설정
    KIWOOM_APP_KEY: str = os.getenv("KIWOOM_APP_KEY", "")
    KIWOOM_APP_SECRET: str = os.getenv("KIWOOM_APP_SECRET", "")

    # 로깅 설정 추가
    LOG_LEVEL: str = "INFO"  # 기본값은 INFO
    
    # 기본 계정 정보
    DEFAULT_EMAIL: Optional[str] = None
    DEFAULT_PASSWORD: Optional[str] = None
    
    # 기본 전략
    DEFAULT_STRATEGY: str = "ENVELOPE"  # "ENVELOPE" 또는 "BOLLINGER"
    
    # 계정별 전략 매핑
    ACCOUNT_STRATEGIES: Dict[str, str] = {}
    
    # 추가 계정 목록 (자동 시작용)
    ADDITIONAL_ACCOUNTS: List[AccountConfig] = []
    
    # Pydantic 2.x에서는 추가 필드를 허용하도록 설정
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore"  # 추가 필드를 무시하도록 설정
    )

# 설정 인스턴스 생성
settings = Settings()

# 환경 변수에서 추가 계정 정보 로드
def load_additional_accounts():
    # 개별 계정 환경변수 로드 (ACCOUNT_1, ACCOUNT_2, ...)
    for i in range(1, 10):  # 최대 9개 계정 지원
        email_var = f'ACCOUNT_{i}_EMAIL'
        password_var = f'ACCOUNT_{i}_PASSWORD'
        strategy_var = f'ACCOUNT_{i}_STRATEGY'
        
        email = os.getenv(email_var)
        password = os.getenv(password_var)
        
        if email and password:
            strategy = os.getenv(strategy_var, settings.DEFAULT_STRATEGY)
            settings.ADDITIONAL_ACCOUNTS.append(
                AccountConfig(
                    email=email,
                    password=password,
                    strategy=strategy
                )
            )
            
            # ACCOUNT_STRATEGIES에도 추가 (하위 호환성)
            settings.ACCOUNT_STRATEGIES[email] = strategy

# 추가 계정 로드
load_additional_accounts()