"""
전략 기본 인터페이스
"""
import logging
from abc import ABC, abstractmethod
from typing import Dict, List, Any
from app.models.trade_models import TradingStrategy

logger = logging.getLogger(__name__)

class BaseTradingModel(ABC):
    """기본 트레이딩 모델 인터페이스"""
    
    def __init__(self, kiwoom_api):
        """트레이딩 모델 초기화"""
        self.kiwoom_api = kiwoom_api
        self.backend_client = None
        self.is_running = False
    
    def set_backend_client(self, backend_client):
        """백엔드 클라이언트 설정"""
        self.backend_client = backend_client
    
    @abstractmethod
    async def start(self):
        """트레이딩 모델 시작"""
        pass
    
    @abstractmethod
    async def stop(self):
        """트레이딩 모델 중지"""
        pass
    
    @abstractmethod
    async def handle_realtime_price(self, symbol, price):
        """실시간 가격 데이터 처리"""
        pass
    
    @abstractmethod
    async def get_trade_decisions(self) -> List[Dict[str, Any]]:
        """매매 의사결정 목록 반환"""
        pass