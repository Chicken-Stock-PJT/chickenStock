from pydantic import BaseSettings
import os
from dotenv import load_dotenv

load_dotenv()

class Settings(BaseSettings):
    BACKEND_WS_URL: str = os.getenv("BACKEND_WS_URL", "ws://localhost:8080/trading")
    MODEL_PATH: str = os.getenv("MODEL_PATH", "./models/trading_model.h5")
    LOG_LEVEL: str = os.getenv("LOG_LEVEL", "INFO")
    SCREENING_INTERVAL: int = int(os.getenv("SCREENING_INTERVAL", "3600"))  # 1시간마다 스크리닝
    MAX_ACTIVE_SYMBOLS: int = int(os.getenv("MAX_ACTIVE_SYMBOLS", "5"))  # 동시에 거래할 최대 종목 수
    MIN_CONFIDENCE: float = float(os.getenv("MIN_CONFIDENCE", "0.7"))  # 최소 신뢰도

settings = Settings()