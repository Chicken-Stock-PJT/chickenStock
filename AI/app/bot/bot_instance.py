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
from app.bot.bot_stock_cache import BotStockCache

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
        self.password = None  # 비밀번호 필드 추가
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
        self.last_auth_check = None  # 마지막 인증 확인 시간 추가
        
        logger.info(f"봇 인스턴스 생성: {email} (전략: {strategy})")
    
    async def initialize(self, password: str, shared_stock_cache=None):
        """
        봇 초기화 및 로그인
        
        :param password: 사용자 비밀번호
        :param shared_stock_cache: 공유 StockCache (전달되면 기존 것 대체)
        """
        try:
            # 비밀번호 저장 (인증 정보 유지)
            self.password = password
            
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
            self.last_auth_check = datetime.now()  # 인증 확인 시간 업데이트
            
            # 백엔드 클라이언트 초기화 (각 봇마다 독립적인 클라이언트)
            self.backend_client = BackendClient()
            self.backend_client.set_auth_client(self.auth_client)
            await self.backend_client.start()

            # 문자열 전략을 Enum으로 변환
            try:
                # 문자열인 경우 Enum으로 변환
                if isinstance(self.strategy, str):
                    strategy_enum = TradingStrategy[self.strategy]
                else:
                    # 이미 Enum인 경우 그대로 사용
                    strategy_enum = self.strategy
                
                # Enum 기반으로 비교
                if strategy_enum == TradingStrategy.ENVELOPE:
                    logger.info(f"봇 {self.email} ENVELOPE 전략 모델 생성 시도")
                    self.trading_model = EnvelopeTradingModel(self.bot_stock_cache)
                    logger.info(f"봇 {self.email} ENVELOPE 전략 모델 생성 성공")
                    self.trading_model.set_backend_client(self.backend_client)
                elif strategy_enum == TradingStrategy.BOLLINGER:
                    logger.info(f"봇 {self.email} BOLLINGER 전략 모델 생성 시도")
                    self.trading_model = BollingerBandTradingModel(self.bot_stock_cache)
                    logger.info(f"봇 {self.email} BOLLINGER 전략 모델 생성 성공")
                    self.trading_model.set_backend_client(self.backend_client)
                elif strategy_enum == TradingStrategy.SHORT_TERM:
                    logger.info(f"봇 {self.email} SHORT_TERM 전략 모델 생성 시도")
                    self.trading_model = ShortTermTradingModel(self.bot_stock_cache)
                    logger.info(f"봇 {self.email} SHORT_TERM 전략 모델 생성 성공")
                    self.trading_model.set_backend_client(self.backend_client)
                else:
                    logger.warning(f"봇 {self.email} 알 수 없는 전략: {strategy_enum}")
                    
            except (KeyError, ValueError) as e:
                # Enum 변환 실패 시 로그 기록
                logger.error(f"봇 {self.email} 전략을 Enum으로 변환 실패: {str(e)}")
                logger.warning(f"봇 {self.email} 알 수 없는 전략: {self.strategy}")
            
            # 봇별 지표 초기 계산
            if self.bot_stock_cache:
                success_count = self.bot_stock_cache.calculate_strategy_indicators()
                logger.info(f"봇 {self.email} 초기 지표 계산: {success_count}개 종목 성공")
            
            logger.info(f"봇 {self.email} 초기화 완료 (전략: {self.strategy})")
            return True
        
        except Exception as e:
            logger.error(f"봇 {self.email} 초기화 중 오류: {str(e)}", exc_info=True)
            await self.cleanup(preserve_auth_info=True)  # 인증 정보 보존
            return False
    
    async def ensure_authentication(self) -> bool:
        """인증 상태 확인 및 필요시 토큰 갱신 또는 재로그인"""
        try:
            # 인증 클라이언트 상태 확인
            if not self.auth_client:
                logger.warning(f"봇 [{self.email}]의 인증 클라이언트가 없습니다. 새로 생성합니다.")
                self.auth_client = AuthClient()
                await self.auth_client.initialize()
            
            # 현재 인증 상태 확인
            if not self.auth_client.is_authenticated or not self.auth_client.is_token_valid():
                logger.warning(f"봇 [{self.email}]의 인증이 유효하지 않습니다.")
                
                # 1. 리프레시 토큰으로 먼저 시도
                if self.auth_client.refresh_token:
                    logger.info(f"봇 [{self.email}] 리프레시 토큰으로 액세스 토큰 갱신 시도")
                    refresh_success = await self.auth_client.refresh_access_token()
                    
                    if refresh_success:
                        logger.info(f"봇 [{self.email}] 액세스 토큰 갱신 성공")
                        # 백엔드 클라이언트에도 새 인증 정보 전달
                        if self.backend_client:
                            self.backend_client.set_auth_client(self.auth_client)
                        
                        self.last_auth_check = datetime.now()  # 인증 확인 시간 업데이트
                        return True
                    else:
                        logger.warning(f"봇 [{self.email}] 액세스 토큰 갱신 실패. 이메일/비밀번호로 로그인 시도")
                
                # 2. 리프레시 토큰이 없거나 갱신 실패 시 이메일/비밀번호로 로그인 시도
                if self.email and self.password:
                    try:
                        logger.info(f"봇 [{self.email}] 이메일/비밀번호로 로그인 시도")
                        login_success = await self.auth_client.login(self.email, self.password)
                        
                        if login_success:
                            logger.info(f"봇 [{self.email}] 이메일/비밀번호 로그인 성공")
                            # 백엔드 클라이언트에도 새 인증 정보 전달
                            if self.backend_client:
                                self.backend_client.set_auth_client(self.auth_client)
                            
                            self.last_auth_check = datetime.now()  # 인증 확인 시간 업데이트
                            return True
                        else:
                            logger.error(f"봇 [{self.email}] 이메일/비밀번호 로그인 실패")
                            return False
                    except Exception as e:
                        logger.error(f"봇 [{self.email}] 로그인 중 오류 발생: {str(e)}")
                        return False
                else:
                    logger.error(f"봇 [{self.email}]의 이메일 또는 비밀번호가 없습니다.")
                    return False
            
            # 이미 인증된 상태
            self.last_auth_check = datetime.now()  # 인증 확인 시간 업데이트
            return True
            
        except Exception as e:
            logger.error(f"봇 [{self.email}] 인증 확인 중 오류: {str(e)}")
            return False
    
    async def update_account_info(self):
        """계좌 정보 업데이트 (인증 확인 포함)"""
        try:
            # 인증 상태 확인
            if not self.backend_client:
                logger.error(f"봇 {self.email}의 백엔드 클라이언트가 초기화되지 않았습니다")
                return False
            
            # 인증 확인 및 필요시 갱신
            auth_ok = await self.ensure_authentication()
            if not auth_ok:
                logger.error(f"봇 {self.email} 인증 확인 실패. 계좌 정보를 업데이트할 수 없습니다.")
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
            
            # 인증 관련 오류인 경우 재인증 시도
            if "인증" in str(e) or "auth" in str(e).lower() or "token" in str(e).lower():
                logger.warning(f"봇 {self.email} 인증 관련 오류 감지. 재인증 시도...")
                auth_ok = await self.ensure_authentication()
                
                if auth_ok:
                    # 재인증 성공 시 계좌 정보 업데이트 다시 시도
                    logger.info(f"봇 {self.email} 재인증 성공. 계좌 정보 업데이트 다시 시도")
                    try:
                        account_info = await self.backend_client.request_account_info()
                        if account_info:
                            self.account_info = account_info
                            
                            # 트레이딩 모델에도 계좌 정보 전달
                            if self.trading_model and hasattr(self.trading_model, 'update_account_info'):
                                self.trading_model.update_account_info(account_info)
                            
                            return True
                    except Exception as retry_error:
                        logger.error(f"봇 {self.email} 계좌 정보 재시도 중 오류: {str(retry_error)}")
            
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
            # 인증 확인 (주기적으로만 수행 - 10분마다)
            current_time = datetime.now()
            if not self.last_auth_check or (current_time - self.last_auth_check).total_seconds() > 600:
                auth_ok = await self.ensure_authentication()
                if not auth_ok:
                    logger.warning(f"봇 {self.email} 인증 실패. 실시간 데이터 처리를 건너뜁니다.")
                    return
            
            # 각 봇의 전략에 맞는 지표 계산
            strategy_enum = None
            if isinstance(self.strategy, str):
                # 대소문자 문제 해결 - 항상 TradingStrategy 열거형과 문자열로 비교
                strategy_str = self.strategy.upper()  # 항상 대문자로 변환
                try:
                    strategy_enum = TradingStrategy[strategy_str]
                except KeyError:
                    logger.error(f"전략 변환 실패 - 존재하지 않는 전략: {strategy_str}")
                    return
            else:
                strategy_enum = self.strategy
            
            indicators = None
            if self.bot_stock_cache:
                # BotStockCache 메서드 호출 확인
                if strategy_enum == TradingStrategy.ENVELOPE:
                    indicators = self.bot_stock_cache.get_envelope_indicators(symbol, price)
                    if indicators is None:
                        logger.error(f"ENVELOPE 지표 계산 실패: 메서드가 None을 반환했습니다.")
                        # BotStockCache의 envelope 지표 계산 메서드 확인 필요
                    
                elif strategy_enum == TradingStrategy.BOLLINGER:
                    indicators = self.bot_stock_cache.get_bollinger_bands(symbol, price)
                    if indicators is None:
                        logger.error(f"BOLLINGER 지표 계산 실패: 메서드가 None을 반환했습니다.")
                        # BotStockCache의 bollinger 지표 계산 메서드 확인 필요
                    
                elif strategy_enum == TradingStrategy.SHORT_TERM:
                    indicators = self.bot_stock_cache.get_short_term_indicators(symbol, price)
                
            # 트레이딩 모델에 전달 (각 봇 전략에 맞는 지표만 전달)
            if indicators:
                # 지표 객체를 트레이딩 모델 호환 형식으로 변환
                indicator_package = {}
                
                # Enum 문자열 비교 대신 열거형 멤버 비교
                if strategy_enum == TradingStrategy.ENVELOPE:
                    indicator_package = {'envelope': indicators}
                elif strategy_enum == TradingStrategy.BOLLINGER:
                    indicator_package = {'bollinger_bands': indicators}
                elif strategy_enum == TradingStrategy.SHORT_TERM:
                    indicator_package = {'short_term': indicators}
                
                # 모델에 전달
                await self.trading_model.handle_realtime_price(symbol, price, indicator_package)
            else:
                logger.warning(f"봇 {self.email} - {symbol}에 대한 지표가 None입니다. 처리 건너뜀.")
        
        except Exception as e:
            logger.error(f"봇 {self.email} 실시간 가격 처리 중 오류: {str(e)}", exc_info=True)
            
            # 인증 관련 오류인 경우 재인증 시도 (다음 호출에서 처리되도록)
            if "인증" in str(e) or "auth" in str(e).lower() or "token" in str(e).lower():
                logger.warning(f"봇 {self.email} 실시간 처리 중 인증 오류 감지. 다음 처리에서 재인증을 시도합니다.")
                self.last_auth_check = None  # 다음 호출에서 인증 확인 강제
        
    async def refresh_indicators(self):
        """봇의 전략별 지표 새로고침"""
        if not self.bot_stock_cache:
            logger.warning(f"봇 {self.email}의 StockCache가 초기화되지 않았습니다")
            return 0
        
        try:
            # 인증 확인
            auth_ok = await self.ensure_authentication()
            if not auth_ok:
                logger.error(f"봇 {self.email} 인증 확인 실패. 지표를 새로고침할 수 없습니다.")
                return 0
            
            success_count = self.bot_stock_cache.refresh_indicators()
            self.last_data_update = datetime.now()
            
            logger.info(f"봇 {self.email} 지표 새로고침: {success_count}개 종목 성공")
            return success_count
        except Exception as e:
            logger.error(f"봇 {self.email} 지표 새로고침 중 오류: {str(e)}")
            return 0
    
    async def start(self):
        """봇 시작 (인증 확인 포함)"""
        if self.is_running:
            logger.warning(f"봇 {self.email}이 이미 실행 중입니다")
            return False
        
        try:
            # 인증 상태 확인 및 필요시 갱신
            auth_ok = await self.ensure_authentication()
            if not auth_ok:
                logger.error(f"봇 {self.email} 인증 확인 실패. 봇을 시작할 수 없습니다.")
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
            await self.cleanup(preserve_auth_info=True)  # 인증 정보 보존
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
    
    async def cleanup(self, preserve_auth_info=False):
        """
        리소스 정리
        
        :param preserve_auth_info: 인증 정보(이메일/비밀번호/토큰) 보존 여부
        """
        try:
            # 봇 중지
            if self.is_running:
                await self.stop()
            
            # 인증 클라이언트 종료 (보존 옵션이 있는 경우 유지)
            if self.auth_client and not preserve_auth_info:
                await self.auth_client.close()
                self.auth_client = None
            
            # 객체 참조 제거 (인증 정보 관련 필드는 옵션에 따라 보존)
            self.trading_model = None
            self.backend_client = None
            self.bot_stock_cache = None
            
            # 이메일과 비밀번호 유지 (preserve_auth_info=True인 경우)
            if not preserve_auth_info:
                self.email = None
                self.password = None
            
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
            "last_auth_check": self.last_auth_check.isoformat() if self.last_auth_check else None,
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