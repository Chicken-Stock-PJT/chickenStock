import logging
import asyncio
from typing import Dict, Any, Optional, List, Callable
from datetime import datetime

from app.auth.auth_client import AuthClient
from app.auth.token_manager import TokenManager
from app.auth.kiwoom_auth import KiwoomAuthClient
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
        self.token_manager = None
        self.kiwoom_auth_client = None
        self.kiwoom_api = None
        self.backend_client = None
        
        # 트레이딩 모델
        self.trading_model = None
        
        # 계좌 정보 (독립적으로 관리)
        self.account_info = {}
        
        # 상태 정보
        self.is_running = False
        self.start_time = None
        self.last_data_update = None
        self.subscription_task = None
        self.trading_loop_task = None
        
        logger.info(f"봇 인스턴스 생성: {email} (전략: {strategy})")
    
    async def initialize(self, password: str, shared_kiwoom_api: Optional[KiwoomAPI] = None):
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
            
            # 공유 KiwoomAPI가 제공되면 사용 (시장 데이터 효율적 공유)
            if shared_kiwoom_api:
                logger.info(f"봇 {self.email}에 공유 KiwoomAPI 설정")
                self.kiwoom_api = shared_kiwoom_api
            else:
                # 독립적인 KiwoomAPI 생성
                logger.info(f"봇 {self.email}에 독립적인 KiwoomAPI 생성")
                # 토큰 관리자 초기화
                self.token_manager = TokenManager()
                await self.token_manager.initialize()
                
                # 키움 인증 클라이언트 초기화 및 토큰 발급
                self.kiwoom_auth_client = KiwoomAuthClient()
                self.kiwoom_auth_client.set_token_manager(self.token_manager)
                await self.kiwoom_auth_client.initialize()
                
                # 키움 API 토큰 발급
                kiwoom_token = await self.kiwoom_auth_client.get_access_token()
                if not kiwoom_token:
                    logger.error(f"봇 {self.email}의 키움 API 토큰 발급 실패")
                    return False
                
                logger.info(f"봇 {self.email}의 키움 API 토큰 발급 성공")
                
                # 키움 API 초기화
                self.kiwoom_api = KiwoomAPI(self.token_manager)
            
            # 백엔드 클라이언트 초기화 (각 봇마다 독립적인 클라이언트)
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
                
                logger.info(f"봇 {self.email} 계좌 정보 업데이트: 예수금={account_info.get('memberMoney', 0)}, "
                          f"보유종목수={len(account_info.get('holdings', []))}")
                return True
            else:
                logger.warning(f"봇 {self.email} 계좌 정보 조회 실패")
                return False
        
        except Exception as e:
            logger.error(f"봇 {self.email} 계좌 정보 업데이트 중 오류: {str(e)}")
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
            
            # REST API 연결 - KiwoomAPI가 공유된 경우 스킵
            if not hasattr(self.kiwoom_api, 'websocket') or self.kiwoom_api.websocket is None:
                if not await self.kiwoom_api.connect():
                    logger.error(f"봇 {self.email} API 연결 실패")
                    return False
            
            # 계좌 정보 초기화 (각 봇마다 독립적으로 관리)
            await self.update_account_info()
            
            # 트레이딩 모델 시작
            if self.trading_model:
                await self.trading_model.start()
            
            # 거래 처리 루프 시작 (각 봇마다 독립적인 거래 로직)
            self.trading_loop_task = asyncio.create_task(
                self.trading_loop()
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
    
    async def trading_loop(self):
        """봇의 거래 처리 루프"""
        logger.info(f"봇 {self.email}의 거래 처리 루프 시작")
        
        try:
            # 마지막 처리 시간 초기화
            last_processing_time = datetime.now()
            
            while self.is_running:
                try:
                    # 현재 시간
                    current_time = datetime.now()
                    
                    # 10초마다 계좌 정보 업데이트 및 매매 결정 처리
                    if (current_time - last_processing_time).total_seconds() >= 10:
                        # 인증 상태 확인
                        if not self.auth_client or not self.auth_client.is_authenticated:
                            logger.warning(f"봇 {self.email}의 인증이 유효하지 않습니다. 거래는 건너뜁니다.")
                            await asyncio.sleep(30)  # 30초 후 재시도
                            continue
                        
                        # 계좌 정보 업데이트
                        await self.update_account_info()
                        
                        if self.trading_model:
                            # 트레이딩 모델에서 매매 결정 가져오기
                            decisions = await self.trading_model.get_trade_decisions()
                            
                            if decisions:
                                logger.info(f"봇 {self.email} 매매 결정: {len(decisions)}개")
                                
                                # 매매 결정이 있으면 백엔드 클라이언트로 요청 전송
                                for idx, decision in enumerate(decisions):
                                    try:
                                        symbol = decision.get("symbol")
                                        action = decision.get("action")
                                        quantity = decision.get("quantity", 0)
                                        price = decision.get("price", 0)
                                        
                                        logger.info(f"봇 {self.email} 매매 결정 #{idx+1}: {action} {symbol} {quantity}주 @ {price}원")
                                        
                                        # 백엔드 클라이언트를 통해 거래 요청 전송
                                        if action.lower() == "buy":
                                            result = await self.backend_client.request_buy(symbol, quantity, price)
                                            if result:
                                                logger.info(f"봇 {self.email} 매수 요청 성공: {symbol} {quantity}주 @ {price}원")
                                            else:
                                                logger.error(f"봇 {self.email} 매수 요청 실패: {symbol} {quantity}주 @ {price}원")
                                        
                                        elif action.lower() == "sell":
                                            result = await self.backend_client.request_sell(symbol, quantity, price)
                                            if result:
                                                logger.info(f"봇 {self.email} 매도 요청 성공: {symbol} {quantity}주 @ {price}원")
                                            else:
                                                logger.error(f"봇 {self.email} 매도 요청 실패: {symbol} {quantity}주 @ {price}원")
                                    
                                    except Exception as e:
                                        logger.error(f"봇 {self.email}의 거래 요청 전송 중 오류: {str(e)}")
                            else:
                                logger.debug(f"봇 {self.email} 매매 결정 없음")
                        
                        # 마지막 처리 시간 업데이트
                        last_processing_time = current_time
                    
                    # 1초 대기
                    await asyncio.sleep(1)
                
                except asyncio.CancelledError:
                    logger.info(f"봇 {self.email}의 거래 처리 루프 취소됨")
                    break
                except Exception as e:
                    logger.error(f"봇 {self.email}의 거래 처리 중 오류: {str(e)}")
                    await asyncio.sleep(5)  # 오류 시 5초 후 재시도
        
        except asyncio.CancelledError:
            logger.info(f"봇 {self.email}의 거래 처리 루프 취소됨")
        except Exception as e:
            logger.error(f"봇 {self.email}의 거래 처리 루프 실행 중 오류: {str(e)}")
        
        logger.info(f"봇 {self.email}의 거래 처리 루프 종료")
    
    async def stop(self):
        """봇 중지"""
        if not self.is_running:
            logger.warning(f"봇 {self.email}이 이미 중지되었습니다")
            return True
        
        try:
            logger.info(f"봇 {self.email} 중지 중...")
            
            # 상태 업데이트 (먼저 실행 중 상태를 False로 변경하여 루프 종료)
            self.is_running = False
            
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
            
            # API 연결 종료 (공유 API인 경우 스킵)
            if self.token_manager and self.kiwoom_api:
                await self.kiwoom_api.close()
            
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
            if self.token_manager:  # 공유 API가 아닌 경우에만
                self.kiwoom_api = None
                self.token_manager = None
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
                "cash_balance": self.account_info.get('memberMoney', 0),
                "positions_count": len(self.account_info.get('positions', [])),
                "total_asset_value": self.account_info.get('totalAsset', 0)
            } if self.account_info else None
        }
    
    def get_account_info(self) -> Dict:
        """계좌 정보 반환"""
        return self.account_info
    
    def get_cash(self) -> float:
        """예수금 조회"""
        return self.account_info.get('memberMoney', 0)
    
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