"""
FastAPI 요청/응답 모델
"""
from typing import Dict, List, Optional, Any
from pydantic import BaseModel
from datetime import datetime
from app.models.trade_models import TradingStrategy

class LoginRequest(BaseModel):
    """로그인 요청 모델"""
    email: str
    password: str
    platform: str = "mobile"
    override_strategy: Optional[TradingStrategy] = None

class BotCreateRequest(BaseModel):
    """봇 생성 요청 모델"""
    email: str
    password: str
    strategy: TradingStrategy

class BotStatusResponse(BaseModel):
    """봇 상태 응답 모델"""
    email: str
    strategy: TradingStrategy
    is_running: bool
    start_time: Optional[str] = None
    last_data_update: Optional[str] = None
    account_info: Optional[Dict] = None