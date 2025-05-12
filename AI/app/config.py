import os
from typing import Dict, Optional
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    # API 설정
    API_BASE_URL: str = "https://api.kiwoom.com"  # 키움증권 API 기본 URL
    BACKEND_API_URL: str = "https://k12a106.p.ssafy.io"  # 백엔드 API URL
    WEBSOCKET_API_URL: str = "wss://api.kiwoom.com:10000"
    
    # 인증 설정
    KIWOOM_API_KEY: str = os.getenv('KIWOOM_API_KEY', '')  # appkey
    KIWOOM_API_SECRET: str = os.getenv('KIWOOM_API_SECRET', '')  # secretkey
    
    # 기본 계정 정보
    DEFAULT_EMAIL: Optional[str] = None
    DEFAULT_PASSWORD: Optional[str] = None
    
    # 기본 전략
    DEFAULT_STRATEGY: str = "ENVELOPE"  # "ENVELOPE" 또는 "BOLLINGER"
    
    # 계정별 전략 매핑
    ACCOUNT_STRATEGIES: Dict[str, str] = {}
    
    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"

# 설정 인스턴스 생성
settings = Settings()