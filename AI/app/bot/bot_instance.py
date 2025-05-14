import logging
import asyncio
from typing import Dict, Any, Optional, List, Callable
from datetime import datetime

from app.auth.auth_client import AuthClient
from app.api.backend_client import BackendClient

from app.models.trade_models import TradingStrategy
from app.strategies.bollinger import BollingerBandTradingModel
from app.strategies.envelope import EnvelopeTradingModel
from app.strategies.short_term import ShortTermTradingModel
from app.bot.bot_stock_cache import BotStockCache  # 새로운 BotStockCache 임포트

logger = logging.getLogger(__name__)

class BotInstance:
    """개별 봇 인스턴스 클래스"""
    
    def __init__(self, email: str, strategy, shared_stock_cache=None):
        """
        봇 인스턴스 초기화
        
        :param email: 사용자 이메일
        :param strategy: 봇 전략 (ENVELOPE 또는 BOLLINGER)
        :param shared_stock_cache: 공유 StockCache 인스턴스
        """
        self.email = email
        self.strategy = strategy
        
        # 봇별 StockCache 생성 (전략별 독립적인 지표 관리)
        self.bot_stock_cache = None
        if shared_stock_cache:
            self.bot_stock_cache = BotStockCache(shared_stock_cache, strategy)
        
        # API 인스턴스
        self.auth_client = None
        self.backend_client = None
        
        # 트레이딩 모델
        self.trading_model = None
        
        # 계좌 정보 (독립적으로 관리)
        self.account_info = {}
        
        # 상태 정보
        self.is_running = False
        self.start_time = None
        self.last_data_update = None
        
        logger.info(f"봇 인스턴스 생성: {email} (전략: {strategy})")
    
    async def initialize(self, password: str, shared_stock_cache=None):
        """
        봇 초기화 및 로그인
        
        :param password: 사용자 비밀번호
        :param shared_stock_cache: 공유 StockCache (전달되면 기존 것 대체)
        """
        try:
            # 공유 캐시 전달되면 업데이트
            if shared_stock_cache and not self.bot_stock_cache:
                self.bot_stock_cache = BotStockCache(shared_stock_cache, self.strategy)
            
            # 인증 클라이언트 초기화
            self.auth_client = AuthClient()
            await self.auth_client.initialize()
            
            # 백엔드 서버 로그인
            login_success = await self.auth_client.login(self.email, password)
            if not login_success:
                logger.error(f"봇 {self.email} 로그인 실패")
                return False
            
            logger.info(f"봇 {self.email} 로그인 성공")
            
            # 백엔드 클라이언트 초기화 (각 봇마다 독립적인 클라이언트)
            self.backend_client = BackendClient()
            self.backend_client.set_auth_client(self.auth_client)
            await self.backend_client.start()
            
            # 전략에 따른 트레이딩 모델 초기화 (봇별 StockCache 전달)
            if self.strategy == TradingStrategy.ENVELOPE:
                self.trading_model = EnvelopeTradingModel(self.bot_stock_cache)  # 봇별 캐시 전달
                self.trading_model.set_backend_client(self.backend_client)
            elif self.strategy == TradingStrategy.BOLLINGER:
                self.trading_model = BollingerBandTradingModel(self.bot_stock_cache)  # 봇별 캐시 전달
                self.trading_model.set_backend_client(self.backend_client)
            elif self.strategy == TradingStrategy.SHORT_TERM:
                    self.trading_model = ShortTermTradingModel(self.bot_stock_cache)
                    self.trading_model.set_backend_client(self.backend_client)
            
            # 봇별 지표 초기 계산
            if self.bot_stock_cache:
                success_count = self.bot_stock_cache.calculate_strategy_indicators()
                logger.info(f"봇 {self.email} 초기 지표 계산: {success_count}개 종목 성공")
            
            logger.info(f"봇 {self.email} 초기화 완료 (전략: {self.strategy})")
            return True
        
        except Exception as e:
            logger.error(f"봇 {self.email} 초기화 중 오류: {str(e)}", exc_info=True)
            await self.cleanup()
            return False
    
    async def update_account_info(self):
        """계좌 정보 업데이트 (독립적으로 관리)"""
        try:
            if not self.backend_client:
                logger.error(f"봇 {self.email}의 백엔드 클라이언트가 초기화되지 않았습니다")
                return False
            
            account_info = await self.backend_client.request_account_info()
            if account_info:
                self.account_info = account_info
                
                # 트레이딩 모델에도 계좌 정보 전달 (각 봇마다 독립적인 계좌 정보)
                if self.trading_model and hasattr(self.trading_model, 'update_account_info'):
                    self.trading_model.update_account_info(account_info)
                
                return True
            else:
                logger.warning(f"봇 {self.email} 계좌 정보 조회 실패")
                return False
        
        except Exception as e:
            logger.error(f"봇 {self.email} 계좌 정보 업데이트 중 오류: {str(e)}")
            return False
    
    async def handle_realtime_price(self, symbol, price):
        """
        실시간 가격 처리 (봇별 독립적인 처리)
        
        :param symbol: 종목 코드
        :param price: 현재가
        """
        if not self.is_running or not self.trading_model:
            return
        
        try:
            # 각 봇의 전략에 맞는 지표 계산
            indicators = None
            if self.bot_stock_cache:
                if self.strategy == TradingStrategy.ENVELOPE:
                    indicators = self.bot_stock_cache.get_envelope_indicators(symbol, price)
                elif self.strategy == TradingStrategy.BOLLINGER:
                    indicators = self.bot_stock_cache.get_bollinger_bands(symbol, price)
                elif self.strategy == TradingStrategy.SHORT_TERM:
                    indicators = self.bot_stock_cache.get_short_term_indicators(symbol, price)
                
            # 트레이딩 모델에 전달 (각 봇 전략에 맞는 지표만 전달)
            if indicators:
                # 지표 객체를 트레이딩 모델 호환 형식으로 변환
                indicator_package = {}
                if self.strategy == TradingStrategy.ENVELOPE:
                    indicator_package = {'envelope': indicators}
                elif self.strategy == TradingStrategy.BOLLINGER:
                    indicator_package = {'bollinger_bands': indicators}
                elif self.strategy == TradingStrategy.SHORT_TERM:
                    indicator_package = {'short_term': indicators}
                    
                # 모델에 전달
                await self.trading_model.handle_realtime_price(symbol, price, indicator_package)
        
        except Exception as e:
            logger.error(f"봇 {self.email} 실시간 가격 처리 중 오류: {str(e)}")
    
    async def refresh_indicators(self):
        """봇의 전략별 지표 새로고침"""
        if not self.bot_stock_cache:
            logger.warning(f"봇 {self.email}의 StockCache가 초기화되지 않았습니다")
            return 0
        
        try:
            success_count = self.bot_stock_cache.refresh_indicators()
            self.last_data_update = datetime.now()
            
            logger.info(f"봇 {self.email} 지표 새로고침: {success_count}개 종목 성공")
            return success_count
        except Exception as e:
            logger.error(f"봇 {self.email} 지표 새로고침 중 오류: {str(e)}")
            return 0
    
    async def start(self):
        """봇 시작"""
        if self.is_running:
            logger.warning(f"봇 {self.email}이 이미 실행 중입니다")
            return False
        
        try:
            if not self.auth_client or not self.auth_client.is_authenticated:
                logger.error(f"봇 {self.email}이 인증되지 않았습니다")
                return False
            
            # 계좌 정보 초기화 (각 봇마다 독립적으로 관리)
            await self.update_account_info()
            
            # 트레이딩 모델 시작
            if self.trading_model:
                await self.trading_model.start()
            
            # 상태 업데이트
            self.is_running = True
            self.start_time = datetime.now()
            
            logger.info(f"봇 {self.email} 시작 완료 (전략: {self.strategy})")
            return True
        
        except Exception as e:
            logger.error(f"봇 {self.email} 시작 중 오류: {str(e)}")
            await self.cleanup()
            return False
    
    async def stop(self):
        """봇 중지"""
        if not self.is_running:
            logger.warning(f"봇 {self.email}이 이미 중지되었습니다")
            return True
        
        try:
            logger.info(f"봇 {self.email} 중지 중...")
            
            # 상태 업데이트 (먼저 실행 중 상태를 False로 변경하여 루프 종료)
            self.is_running = False
            
            # 트레이딩 모델 종료
            if self.trading_model:
                await self.trading_model.stop()
            
            # 백엔드 클라이언트 종료
            if self.backend_client:
                await self.backend_client.stop()
            
            logger.info(f"봇 {self.email} 중지 완료")
            return True
        
        except Exception as e:
            logger.error(f"봇 {self.email} 중지 중 오류: {str(e)}")
            return False
    
    async def cleanup(self):
        """리소스 정리"""
        try:
            # 봇 중지
            if self.is_running:
                await self.stop()
            
            # 인증 클라이언트 종료
            if self.auth_client:
                await self.auth_client.close()
            
            # 객체 참조 제거
            self.auth_client = None
            self.trading_model = None
            self.backend_client = None
            self.bot_stock_cache = None
            
            logger.info(f"봇 {self.email} 리소스 정리 완료")
            return True
        
        except Exception as e:
            logger.error(f"봇 {self.email} 리소스 정리 중 오류: {str(e)}")
            return False
    
    def get_status(self) -> Dict:
        """봇 상태 정보 반환"""
        return {
            "email": self.email,
            "strategy": self.strategy,
            "is_running": self.is_running,
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "last_data_update": self.last_data_update.isoformat() if self.last_data_update else None,
            "authenticated": self.auth_client.is_authenticated if self.auth_client else False,
            "account_info": {
                "cash_balance": self.account_info.get('cash_balance', 0),
                "positions_count": len(self.account_info.get('positions', [])),
                "total_asset_value": self.account_info.get('totalAsset', 0)
            } if self.account_info else None
        }
    
    def get_account_info(self) -> Dict:
        """계좌 정보 반환"""
        return self.account_info
    
    def get_cash(self) -> float:
        """예수금 조회"""
        return self.account_info.get('cash_balance', 0)
    
    def get_holdings(self) -> List:
        """보유 종목 목록 조회"""
        return self.account_info.get('positions', [])
    
    def get_holding(self, symbol: str) -> Optional[Dict]:
        """특정 종목 보유 정보 조회"""
        holdings = self.get_holdings()
        for holding in holdings:
            if holding.get('stockName') == symbol:
                return holding
        return None