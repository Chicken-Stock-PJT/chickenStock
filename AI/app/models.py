from pydantic import BaseModel
from typing import Optional, List, Dict, Any
from datetime import datetime

class TradeDecision(BaseModel):
    """AI 모델의 거래 결정"""
    symbol: str
    action: str  # "BUY", "SELL", "HOLD"
    confidence: float
    price: float
    quantity: int
    timestamp: datetime = datetime.now()
    meta: Dict[str, Any] = {}  # 추가 메타데이터 (전략 정보, 지표값 등)
    
    def dict(self):
        """API 요청을 위한 딕셔너리 변환"""
        return {
            "symbol": self.symbol,
            "action": self.action,
            "price": self.price,
            "quantity": self.quantity,
            "confidence": self.confidence,
            "timestamp": self.timestamp.isoformat(),
            "meta": self.meta
        }

class MarketData(BaseModel):
    """시장 데이터 모델"""
    symbol: str
    name: Optional[str] = None
    price: float
    volume: Optional[int] = None
    timestamp: str

class AccountInfo(BaseModel):
    """계좌 정보 모델"""
    cash_balance: float  # 예수금
    positions: Dict[str, Dict[str, Any]]  # 보유 종목 정보 (종목코드 -> 상세정보)
    total_asset_value: float  # 총 자산가치 (현금 + 포지션)
    
    class Config:
        arbitrary_types_allowed = True