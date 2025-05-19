"""
거래 관련 데이터 모델
"""
from enum import Enum
from typing import Dict, List, Optional, Any
from datetime import datetime

class TradingStrategy(str, Enum):
    """트레이딩 전략 유형"""
    ENVELOPE = "ENVELOPE"
    BOLLINGER = "BOLLINGER"
    SHORT_TERM = "SHORT_TERM"
    DRL_UTRANS = "DRL_UTRANS"

class TradeDecision:
    """매매 결정 모델"""
    def __init__(
        self,
        symbol: str,
        action: str,
        quantity: int,
        price: float,
        reason: str
    ):
        self.symbol = symbol
        self.action = action  # "buy" 또는 "sell"
        self.quantity = quantity
        self.price = price
        self.reason = reason
        self.timestamp = datetime.now().isoformat()
    
    def to_dict(self) -> Dict[str, Any]:
        """딕셔너리로 변환"""
        return {
            "symbol": self.symbol,
            "action": self.action,
            "quantity": self.quantity,
            "price": self.price,
            "reason": self.reason,
            "timestamp": self.timestamp
        }