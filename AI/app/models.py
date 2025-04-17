from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime

class MarketData(BaseModel):
    """시장 데이터 모델"""
    symbol: str
    timestamp: datetime
    price: float
    volume: int
    high: Optional[float] = None
    low: Optional[float] = None
    bid: Optional[float] = None
    ask: Optional[float] = None

class TradeDecision(BaseModel):
    """AI 모델의 거래 결정"""
    symbol: str
    action: str  # "BUY", "SELL", "HOLD"
    confidence: float
    price: float
    quantity: int
    timestamp: datetime = datetime.now()