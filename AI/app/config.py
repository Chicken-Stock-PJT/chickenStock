import os
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    # API 설정
    API_BASE_URL: str = "https://api.kiwoom.com"  # 키움증권 API 기본 URL
    BACKEND_API_URL: str = "https://k12a106.p.ssafy.io"  # 백엔드 API URL
    WEBSOCKET_API_URL: str = "wss://api.kiwoom.com:10000"
    
    # 인증 설정
    KIWOOM_API_KEY: str = os.getenv('KIWOOM_API_KEY', '')  # appkey
    KIWOOM_API_SECRET: str = os.getenv('KIWOOM_API_SECRET', '')  # secretkey
    
    # 토큰 관리 설정
    TOKEN_REFRESH_MARGIN: int = 300  # 토큰 만료 5분(300초) 전에 갱신
    TOKEN_REFRESH_INTERVAL: int = 3600  # 토큰 갱신 검사 간격(최대 1시간)

    # 주식 거래 설정
    MIN_CONFIDENCE: float = 0.6  # 최소 신뢰도 (60% 이상일 때만 거래)
    MAX_POSITIONS: int = 20  # 최대 보유 종목 수
    POSITION_SIZE_PCT: float = 0.05  # 총 자산의 5%를 투자
    
    # 로깅 설정
    LOG_LEVEL: str = "INFO"
    
    # HTTP 요청 설정
    REQUEST_TIMEOUT: int = 30  # HTTP 요청 타임아웃(초)
    MAX_RETRIES: int = 3  # 최대 재시도 횟수
    
    # 디버그 모드 (장 시간 외에도 동작)
    DEBUG_MODE: bool = False
    
    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"
        extra = 'ignore'

settings = Settings()