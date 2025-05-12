import logging
import asyncio
from typing import Dict, Any
from datetime import datetime

from app.auth.auth_client import AuthClient
from app.auth.token_manager import TokenManager
from app.api.kiwoom_api import KiwoomAPI
from app.api.backend_client import BackendClient

from app.models.trade_models import TradingStrategy
from app.strategies.bollinger import BollingerBandTradingModel
from app.strategies.envelope import EnvelopeTradingModel

logger = logging.getLogger(__name__)

class BotInstance:
    """개별 봇 인스턴스 클래스"""
    
    def __init__(self, email: str, strategy):
        """봇 인스턴스 초기화"""
        self.email = email
        self.strategy = strategy
        
        # API 인스턴스
        self.auth_client = None
        self.kiwoom_api = None
        self.backend_client = None
        
        # 트레이딩 모델
        self.trading_model = None
        
        # 상태 정보
        self.is_running = False
        self.start_time = None
        self.last_data_update = None
        self.subscription_task = None
        self.trading_loop_task = None
        
        logger.info(f"봇 인스턴스 생성: {email} (전략: {strategy})")
    
    async def initialize(self, password: str):
        """봇 초기화 및 로그인"""
        try:
            # 인증 클라이언트 초기화
            self.auth_client = AuthClient()
            await self.auth_client.initialize()
            
            # 백엔드 서버 로그인
            login_success = await self.auth_client.login(self.email, password)
            if not login_success:
                logger.error(f"봇 {self.email} 로그인 실패")
                return False
            
            logger.info(f"봇 {self.email} 로그인 성공")
            
            # 토큰 관리자 초기화
            token_manager = TokenManager()
            await token_manager.initialize()
            
            # 키움 API 초기화
            self.kiwoom_api = KiwoomAPI(token_manager)
            
            # 백엔드 클라이언트 초기화
            self.backend_client = BackendClient()
            self.backend_client.set_auth_client(self.auth_client)
            await self.backend_client.start()
            
            # 전략에 따른 트레이딩 모델 초기화
            if self.strategy == TradingStrategy.ENVELOPE:
                self.trading_model = EnvelopeTradingModel(self.kiwoom_api)
                self.trading_model.set_backend_client(self.backend_client)
            elif self.strategy == TradingStrategy.BOLLINGER:
                self.trading_model = BollingerBandTradingModel(self.kiwoom_api)
                self.trading_model.set_backend_client(self.backend_client)
            
            logger.info(f"봇 {self.email} 초기화 완료 (전략: {self.strategy})")
            return True
        
        except Exception as e:
            logger.error(f"봇 {self.email} 초기화 중 오류: {str(e)}")
            await self.cleanup()
            return False
    
    async def start(self):
        """봇 시작"""
        if self.is_running:
            logger.warning(f"봇 {self.email}이 이미 실행 중입니다")
            return False
        
        try:
            if not self.auth_client or not self.auth_client.is_authenticated:
                logger.error(f"봇 {self.email}이 인증되지 않았습니다")
                return False
            
            # REST API 연결
            if not await self.kiwoom_api.connect():
                logger.error(f"봇 {self.email} API 연결 실패")
                return False
            
            # 종목 정보 초기화
            stock_list = await self.backend_client.get_all_stocks()
            await self.kiwoom_api.initialize_stock_list(stock_list)
            
            # 계좌 정보 초기화
            account_info = await self.backend_client.request_account_info()
            if account_info:
                self.kiwoom_api.update_account_info(account_info)
            
            # 필터링된 종목 리스트 가져오기 (시가총액 기준)
            initial_filtered_symbols = await self.kiwoom_api.get_filtered_symbols(450, 150)
            logger.info(f"봇 {self.email} 시가총액 기준 초기 필터링 완료: 총 {len(initial_filtered_symbols)}개 종목")
            
            # 필터링된 종목 중 실제 stock_list에 있는 종목만 추출
            available_symbols = [
                symbol for symbol in initial_filtered_symbols
                if any(stock.get('shortCode') == symbol for stock in stock_list)
            ]
            
            # 정확히 600개 종목을 선택 (또는 가능한 최대)
            target_count = min(600, len(available_symbols))
            final_symbols = available_symbols[:target_count]
            
            # 필터링된 종목 정보로 stock_cache 업데이트
            filtered_stock_list = [
                stock for stock in stock_list 
                if stock.get('shortCode') in final_symbols
            ]
            await self.kiwoom_api.initialize_stock_list(filtered_stock_list)
            
            filtered_stockcode_list = [
                stock.get("shortCode") for stock in filtered_stock_list if stock.get("shortCode")
            ]
            
            # StockCache에 필터링된 종목 리스트 설정
            self.kiwoom_api.stock_cache.set_filtered_stocks(filtered_stockcode_list)
            
            # 전략에 따른 차트 데이터 수집 및 지표 계산
            if self.strategy == TradingStrategy.ENVELOPE:
                # Envelope 지표 계산용 차트 데이터 초기화 (120일 데이터)
                await self.kiwoom_api.initialize_chart_data(filtered_stockcode_list, period=120)
                # Envelope 지표 계산
                self.kiwoom_api.stock_cache.calculate_envelope_indicators()
            elif self.strategy == TradingStrategy.BOLLINGER:
                # 볼린저 밴드 지표 계산용 차트 데이터 초기화 (60일 데이터)
                await self.kiwoom_api.initialize_chart_data(filtered_stockcode_list, period=60)
                # 볼린저 밴드 지표 계산
                self.kiwoom_api.stock_cache.calculate_bollinger_bands()
            
            # 마지막 데이터 업데이트 시간 기록
            self.last_data_update = datetime.now()
            
            # 트레이딩 모델 시작
            if self.trading_model:
                await self.trading_model.start()
            
            # 실시간 데이터 구독 준비
            await self.kiwoom_api.prepare_subscription_groups(filtered_stock_list, 30)
            
            # 실시간 데이터 구독 로테이션 시작
            self.subscription_task = asyncio.create_task(
                self.kiwoom_api.start_rotating_subscriptions(self.trading_model.handle_realtime_price)
            )
            
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
            
            # 태스크 취소
            if self.subscription_task:
                self.subscription_task.cancel()
                try:
                    await self.subscription_task
                except asyncio.CancelledError:
                    pass
                self.subscription_task = None
            
            if self.trading_loop_task:
                self.trading_loop_task.cancel()
                try:
                    await self.trading_loop_task
                except asyncio.CancelledError:
                    pass
                self.trading_loop_task = None
            
            # 트레이딩 모델 종료
            if self.trading_model:
                await self.trading_model.stop()
            
            # 백엔드 클라이언트 종료
            if self.backend_client:
                await self.backend_client.stop()
            
            # API 연결 종료
            if self.kiwoom_api:
                await self.kiwoom_api.close()
            
            # 상태 업데이트
            self.is_running = False
            
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
            self.kiwoom_api = None
            self.trading_model = None
            self.backend_client = None
            
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
                "cash_balance": self.kiwoom_api.get_cash_balance() if self.kiwoom_api else 0,
                "positions_count": len(self.kiwoom_api.get_positions()) if self.kiwoom_api else 0,
                "total_asset_value": self.kiwoom_api.get_total_asset_value() if self.kiwoom_api else 0
            } if self.kiwoom_api else None
        }