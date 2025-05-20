"""
전략 기본 인터페이스
"""
import logging
from abc import ABC, abstractmethod
from typing import Dict, List, Any, Optional
from datetime import datetime
from app.models.trade_models import TradingStrategy

logger = logging.getLogger(__name__)

class BaseTradingModel(ABC):
    """기본 트레이딩 모델 인터페이스"""
    
    def __init__(self, stock_cache=None):
        """트레이딩 모델 초기화"""
        self.stock_cache = stock_cache  # 직접 API 대신 StockCache 참조 (선택적)
        self.backend_client = None
        self.is_running = False
        
        # 계좌 정보 관리 (독립적으로 관리)
        self.account_info = {}
        self.positions = {}  # {symbol: position_info}
        self.cash_balance = 0
        
        # 전략 상태 관리 (독립적으로 관리)
        self.trading_signals = {}  # {symbol: signal_info}
        self.trade_history = {}  # {symbol: trade_history}
        
        # 중복 메시지 필터링을 위한 변수
        self.last_processed_prices = {}  # {symbol: price}
        self.last_processed_times = {}   # {symbol: datetime}
        self.min_price_change_pct = 0.1  # 최소 가격 변동 비율 (0.1%)
        self.min_process_interval = 5    # 최소 처리 간격 (초)
        
        # 전략 설정 (하위 클래스에서 오버라이드)
        self.max_positions = 10  # 최대 보유 종목 수
        self.trade_amount_per_stock = 5000000  # 종목당 매매 금액 (500만원)
        self.min_holding_period = 1  # 최소 보유 기간 (일)
    
    def set_stock_cache(self, stock_cache):
        """StockCache 설정"""
        self.stock_cache = stock_cache
    
    def set_backend_client(self, backend_client):
        """백엔드 클라이언트 설정"""
        self.backend_client = backend_client
    
    def update_account_info(self, account_info: Dict):
        """계좌 정보 업데이트 (독립적으로 관리)"""
        self.account_info = account_info
        # 포지션 데이터 구조 변환 (symbol을 키로 하는 딕셔너리)
        self.positions = self.account_info.get('positions', [])
        self.cash_balance = account_info.get('cash_balance', 0)
        logger.debug(f"계좌 정보 업데이트: 예수금={self.cash_balance}, 보유종목수={len(self.positions)}")
    
    def get_cash_balance(self) -> float:
        """현금 잔고 조회"""
        return self.cash_balance
    
    def get_positions(self) -> Dict:
        """보유 종목 목록 조회"""
        return self.positions
    
    def get_position(self, symbol: str) -> Optional[Dict]:
        """특정 종목 보유 정보 조회"""
        return self.positions.get(symbol)
    
    def is_holding(self, symbol: str) -> bool:
        """특정 종목 보유 여부 확인"""
        return symbol in self.positions
    
    def get_total_positions_count(self) -> int:
        """보유 종목 수 조회"""
        return len(self.positions)
    
    def add_trading_signal(self, symbol: str, signal_type: str, price: float, additional_data: Dict = None):
        """매매 신호 추가"""
        now = datetime.now()
        
        self.trading_signals[symbol] = {
            "signal": signal_type,
            "price": price,
            "timestamp": now,
            "additional_data": additional_data or {}
        }
        
        logger.info(f"매매 신호 추가: {symbol}, 타입: {signal_type}, 가격: {price:.2f}")
    
    def remove_trading_signal(self, symbol: str):
        """매매 신호 제거"""
        if symbol in self.trading_signals:
            del self.trading_signals[symbol]
            logger.debug(f"매매 신호 제거: {symbol}")
    
    def update_trade_history(self, symbol: str, action: str):
        """거래 이력 업데이트"""
        now = datetime.now()
        
        self.trade_history[symbol] = {
            "last_trade": now,
            "last_action": action
        }
        
        logger.debug(f"거래 이력 업데이트: {symbol}, 행동: {action}")
    
    def get_trading_signals(self) -> Dict:
        """매매 신호 목록 조회"""
        return self.trading_signals
    
    def get_trade_history(self, symbol: str = None) -> Dict:
        """거래 이력 조회"""
        if symbol:
            return self.trade_history.get(symbol, {})
        return self.trade_history
    
    def check_min_holding_period(self, symbol: str) -> bool:
        """최소 보유 기간 확인"""
        last_trade = self.trade_history.get(symbol, {})
        last_trade_time = last_trade.get("last_trade", None)
        
        if not last_trade_time:
            return True  # 거래 이력이 없으면 최소 보유 기간 제한 없음
        
        seconds_passed = (datetime.now() - last_trade_time).total_seconds()
        return seconds_passed >= (self.min_holding_period * 86400)  # 일 -> 초 변환
    
    def _should_process_price_update(self, symbol: str, price: float) -> bool:
        """가격 업데이트를 처리해야 하는지 판단 (중복 메시지 필터링)"""
        now = datetime.now()
        
        # 마지막 처리 시간 확인
        last_time = self.last_processed_times.get(symbol)
        if last_time:
            time_diff = (now - last_time).total_seconds()
            # 최소 처리 간격 미만이면 처리하지 않음
            if time_diff < self.min_process_interval:
                return False
        
        # 마지막 처리 가격 확인
        last_price = self.last_processed_prices.get(symbol)
        if last_price:
            # 가격 변동률 계산
            price_change_pct = abs(price - last_price) / last_price * 100
            # 최소 가격 변동률 미만이면 처리하지 않음
            if price_change_pct < self.min_price_change_pct:
                return False
        
        # 처리해야 할 경우 마지막 처리 정보 업데이트
        self.last_processed_prices[symbol] = price
        self.last_processed_times[symbol] = now
        return True
    
    def get_strategy_info(self) -> Dict:
        """전략 관련 정보 반환"""
        return {
            "max_positions": self.max_positions,
            "trade_amount_per_stock": self.trade_amount_per_stock,
            "min_holding_period": self.min_holding_period,
            "active_signals": len(self.trading_signals),
            "positions_count": len(self.positions),
            "cash_balance": self.cash_balance
        }
    
    def clean_old_signals(self, max_age_seconds: int = 600):
        """오래된 신호 제거"""
        now = datetime.now()
        for symbol, signal_info in list(self.trading_signals.items()):
            timestamp = signal_info["timestamp"]
            if (now - timestamp).total_seconds() > max_age_seconds:
                del self.trading_signals[symbol]
                logger.debug(f"종목 {symbol}의 오래된 신호 제거 ({max_age_seconds}초 경과)")
    
    def get_price(self, symbol: str) -> Optional[float]:
        """종목 가격 조회 (캐시 활용)"""
        if self.stock_cache:
            return self.stock_cache.get_price(symbol)
        return None
    
    def get_envelope_indicators(self, symbol: str, price: float = None) -> Optional[Dict]:
        """Envelope 지표 조회 (캐시 활용)"""
        if self.stock_cache:
            return self.stock_cache.get_envelope_indicators(symbol, price)
        return None
    
    def get_bollinger_bands(self, symbol: str, price: float = None) -> Optional[Dict]:
        """볼린저 밴드 지표 조회 (캐시 활용)"""
        if self.stock_cache:
            return self.stock_cache.get_bollinger_bands(symbol, price)
        return None
    
    async def refresh_indicators(self):
        """지표 갱신 (데이터 갱신 후 호출)"""
        logger.info(f"{self.__class__.__name__} 지표 갱신")
        # 하위 클래스에서 필요시 오버라이드
    
    def configure_strategy(self, config: Dict):
        """전략 설정 변경"""
        if 'max_positions' in config:
            self.max_positions = config['max_positions']
        
        if 'trade_amount_per_stock' in config:
            self.trade_amount_per_stock = config['trade_amount_per_stock']
            
        if 'min_holding_period' in config:
            self.min_holding_period = config['min_holding_period']
            
        if 'min_price_change_pct' in config:
            self.min_price_change_pct = config['min_price_change_pct']
            
        if 'min_process_interval' in config:
            self.min_process_interval = config['min_process_interval']
        
        logger.info(f"전략 설정 변경: {config}")
    
    @abstractmethod
    async def start(self):
        """트레이딩 모델 시작"""
        pass
    
    @abstractmethod
    async def stop(self):
        """트레이딩 모델 중지"""
        pass
    
    @abstractmethod
    async def handle_realtime_price(self, symbol, price, indicators=None):
        """실시간 가격 데이터 처리"""
        pass
    
    @abstractmethod
    async def get_trade_decisions(self, prices: Dict[str, float] = None) -> List[Dict[str, Any]]:
        """매매 의사결정 목록 반환"""
        pass