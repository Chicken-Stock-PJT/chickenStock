import os
from typing import Dict, Optional, List, Any
from pydantic import Field, field_validator
from pydantic_settings import BaseSettings, SettingsConfigDict
from dotenv import load_dotenv

class AccountConfig(BaseSettings):
    """개별 계정 설정을 위한 모델"""
    email: str
    password: str
    strategy: str = "ENVELOPE"  # 기본값은 ENVELOPE

class Settings(BaseSettings):
    load_dotenv()
    # API 설정
    API_BASE_URL: str = "https://api.kiwoom.com"  # 키움증권 API 기본 URL
    BACKEND_API_URL: str = "https://chickenstock.shop"  # 백엔드 API URL
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
    ADDITIONAL_ACCOUNTS: List[AccountConfig] = Field(default_factory=list)
    
    # Pydantic 2.x에서는 추가 필드를 허용하도록 설정
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore"  # 추가 필드를 무시하도록 설정
    )

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        
        # 초기화 후 환경 변수에서 계정 정보 직접 로드
        accounts = []
        
        # 계정 1 설정 로드
        email1 = os.getenv("ACCOUNT_1_EMAIL")
        password1 = os.getenv("ACCOUNT_1_PASSWORD")
        strategy1 = os.getenv("ACCOUNT_1_STRATEGY")
        
        if email1 and password1 and strategy1:
            accounts.append(AccountConfig(
                email=email1,
                password=password1,
                strategy=strategy1
            ))
            
        # 계정 2 설정 로드
        email2 = os.getenv("ACCOUNT_2_EMAIL")
        password2 = os.getenv("ACCOUNT_2_PASSWORD")
        strategy2 = os.getenv("ACCOUNT_2_STRATEGY")
        
        if email2 and password2 and strategy2:
            accounts.append(AccountConfig(
                email=email2,
                password=password2,
                strategy=strategy2
            ))

        # 계정 3 설정 로드
        email3 = os.getenv("ACCOUNT_3_EMAIL")
        password3 = os.getenv("ACCOUNT_3_PASSWORD")
        strategy3 = os.getenv("ACCOUNT_3_STRATEGY")
        
        if email3 and password3 and strategy3:
            accounts.append(AccountConfig(
                email=email3,
                password=password3,
                strategy=strategy3
            ))

        # 계정 4 설정 로드
        email4 = os.getenv("ACCOUNT_4_EMAIL")
        password4 = os.getenv("ACCOUNT_4_PASSWORD")
        strategy4 = os.getenv("ACCOUNT_4_STRATEGY")
        
        if email4 and password4 and strategy4:
            accounts.append(AccountConfig(
                email=email4,
                password=password4,
                strategy=strategy4
            ))
            
        self.ADDITIONAL_ACCOUNTS = accounts

# 설정 인스턴스 생성
settings = Settings()
