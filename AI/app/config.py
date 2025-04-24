from pydantic import BaseSettings

class Settings(BaseSettings):
    # API 설정
    BACKEND_API_URL: str = "http://localhost:8080/api"  # 백엔드 API URL
    
    # 주식 거래 설정
    MIN_CONFIDENCE: float = 0.6  # 최소 신뢰도 (60% 이상일 때만 거래)
    MAX_POSITIONS: int = 20  # 최대 보유 종목 수
    POSITION_SIZE_PCT: float = 0.05  # 총 자산의 5%를 투자
    
    # 로깅 설정
    LOG_LEVEL: str = "INFO"
    
    # 키움증권 API 설정
    ACCOUNT_PASSWORD: str = ""  # 계좌 비밀번호 (환경변수로 설정 권장)
    
    # 디버그 모드 (장 시간 외에도 동작)
    DEBUG_MODE: bool = False
    
    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"

settings = Settings()