import os
from typing import Dict, Optional, List, Any
from pydantic import model_validator
from pydantic_settings import BaseSettings, SettingsConfigDict

class AccountConfig(BaseSettings):
    """개별 계정 설정을 위한 모델"""
    email: str
    password: str
    strategy: str = "ENVELOPE"  # 기본값은 ENVELOPE

class Settings(BaseSettings):
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
    ADDITIONAL_ACCOUNTS: List[AccountConfig] = [
        {
        'email': os.getenv("ACCOUNT_1_EMAIL", ""),
        'password': os.getenv("ACCOUNT_1_PASSWORD", ""),
        'strategy': os.getenv("ACCOUNT_1_STRATEGY", ""),
        },
        {
        'email': os.getenv("ACCOUNT_2_EMAIL", ""),
        'password': os.getenv("ACCOUNT_2_PASSWORD", ""),
        'strategy': os.getenv("ACCOUNT_2_STRATEGY", ""),
        },
    ]
    
    # Pydantic 2.x에서는 추가 필드를 허용하도록 설정
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore"  # 추가 필드를 무시하도록 설정
    )

# 설정 인스턴스 생성
settings = Settings()
