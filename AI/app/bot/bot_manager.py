import logging
import asyncio
from typing import Dict, Optional, Any, List

from app.models.trade_models import TradingStrategy
from app.bot.bot_instance import BotInstance
from app.api.kiwoom_api import KiwoomAPI
from app.auth.token_manager import TokenManager
from app.auth.kiwoom_auth import KiwoomAuthClient

logger = logging.getLogger(__name__)


class BotManager:
    """여러 봇 인스턴스를 관리하는 클래스"""
    
    def __init__(self):
        """봇 관리자 초기화"""
        # 이메일을 키로 하는 봇 인스턴스 딕셔너리
        self.bots: Dict[str, BotInstance] = {}
        
        # 공유 API 관련 변수
        self.shared_token_manager = None
        self.shared_kiwoom_api = None
        self.shared_api_initialized = False
        
        logger.info("봇 관리자 초기화 완료")
    
    async def initialize_shared_api(self):
        """공유 API 초기화"""
        try:
            if self.shared_api_initialized:
                logger.info("공유 API가 이미 초기화되어 있습니다.")
                return True
            
            logger.info("공유 API 초기화 시작")
            
            # 토큰 관리자 초기화
            self.shared_token_manager = TokenManager()
            await self.shared_token_manager.initialize()
            
            # 키움 인증 클라이언트 초기화
            kiwoom_auth_client = KiwoomAuthClient()
            kiwoom_auth_client.set_token_manager(self.shared_token_manager)
            await kiwoom_auth_client.initialize()
            
            # 키움 API 토큰 발급
            kiwoom_token = await kiwoom_auth_client.get_access_token()
            if not kiwoom_token:
                logger.error("공유 키움 API 토큰 발급 실패")
                return False
            
            # 키움 API 초기화
            self.shared_kiwoom_api = KiwoomAPI(self.shared_token_manager)
            
            # API 연결
            if not await self.shared_kiwoom_api.connect():
                logger.error("공유 키움 API 연결 실패")
                return False
            
            self.shared_api_initialized = True
            logger.info("공유 API 초기화 완료")
            return True
        
        except Exception as e:
            logger.error(f"공유 API 초기화 중 오류: {str(e)}")
            return False
    
    async def create_bot(self, email: str, password: str, strategy: TradingStrategy) -> Optional[BotInstance]:
        """새로운 봇 생성 및 초기화"""
        try:
            # 공유 API 초기화 (아직 초기화되지 않은 경우)
            if not self.shared_api_initialized:
                await self.initialize_shared_api()
            
            # 이미 존재하는 봇인지 확인
            if email in self.bots:
                # 동일한 전략이면 기존 봇 반환
                if self.bots[email].strategy == strategy:
                    logger.info(f"이미 존재하는 봇 반환: {email} (전략: {strategy})")
                    return self.bots[email]
                else:
                    # 다른 전략이면 기존 봇 제거 후 새로 생성
                    logger.info(f"다른 전략의 봇이 존재하여 제거 후 재생성: {email} "
                              f"({self.bots[email].strategy} -> {strategy})")
                    await self.remove_bot(email)
            
            # 새 봇 인스턴스 생성
            bot = BotInstance(email, strategy)
            
            # 봇 초기화 (로그인 등) - 공유 API 전달
            if await bot.initialize(password, self.shared_kiwoom_api):
                # 초기화 성공 시 봇 목록에 추가
                self.bots[email] = bot
                logger.info(f"새 봇 생성 성공: {email} (전략: {strategy})")
                return bot
            else:
                logger.error(f"봇 초기화 실패: {email} (전략: {strategy})")
                await bot.cleanup()
                return None
        
        except Exception as e:
            logger.error(f"봇 생성 중 오류: {str(e)}")
            return None
    
    async def start_bot(self, email: str) -> bool:
        """특정 봇 시작"""
        try:
            # 봇 존재 여부 확인
            if email not in self.bots:
                logger.error(f"시작할 봇을 찾을 수 없음: {email}")
                return False
            
            bot = self.bots[email]
            
            # 이미 실행 중인지 확인
            if bot.is_running:
                logger.info(f"봇이 이미 실행 중: {email}")
                return True
            
            # 봇 시작
            success = await bot.start()
            
            if success:
                logger.info(f"봇 시작 성공: {email}")
            else:
                logger.error(f"봇 시작 실패: {email}")
            
            return success
        
        except Exception as e:
            logger.error(f"봇 시작 중 오류: {email}, {str(e)}")
            return False
    
    async def stop_bot(self, email: str) -> bool:
        """특정 봇 중지"""
        try:
            # 봇 존재 여부 확인
            if email not in self.bots:
                logger.error(f"중지할 봇을 찾을 수 없음: {email}")
                return False
            
            bot = self.bots[email]
            
            # 실행 중이 아니면 이미 중지된 상태
            if not bot.is_running:
                logger.info(f"봇이 이미 중지됨: {email}")
                return True
            
            # 봇 중지
            success = await bot.stop()
            
            if success:
                logger.info(f"봇 중지 성공: {email}")
            else:
                logger.error(f"봇 중지 실패: {email}")
            
            return success
        
        except Exception as e:
            logger.error(f"봇 중지 중 오류: {email}, {str(e)}")
            return False
    
    async def remove_bot(self, email: str) -> bool:
        """봇 삭제 및 리소스 정리"""
        try:
            # 봇 존재 여부 확인
            if email not in self.bots:
                logger.warning(f"삭제할 봇을 찾을 수 없음: {email}")
                return False
            
            # 봇 중지 및 리소스 정리
            bot = self.bots[email]
            await bot.cleanup()
            
            # 딕셔너리에서 제거
            del self.bots[email]
            
            logger.info(f"봇 삭제 성공: {email}")
            return True
        
        except Exception as e:
            logger.error(f"봇 삭제 중 오류: {email}, {str(e)}")
            return False
    
    def get_bot(self, email: str) -> Optional[BotInstance]:
        """특정 이메일의 봇 인스턴스 조회"""
        return self.bots.get(email)
    
    def get_running_bots(self) -> Dict[str, BotInstance]:
        """실행 중인 봇 목록 조회"""
        running_bots = {email: bot for email, bot in self.bots.items() if bot.is_running}
        logger.debug(f"실행 중인 봇 조회: {len(running_bots)}개")
        return running_bots
    
    def get_all_bots(self) -> Dict[str, BotInstance]:
        """모든 봇 목록 조회"""
        return self.bots.copy()
    
    async def start_all_bots(self) -> Dict[str, bool]:
        """모든 봇 시작"""
        results = {}
        
        for email in self.bots.keys():
            results[email] = await self.start_bot(email)
        
        success_count = sum(1 for success in results.values() if success)
        logger.info(f"모든 봇 시작 요청 완료: {success_count}/{len(results)}개 성공")
        
        return results
    
    async def stop_all_bots(self) -> Dict[str, bool]:
        """모든 봇 중지"""
        results = {}
        
        for email in self.bots.keys():
            results[email] = await self.stop_bot(email)
        
        success_count = sum(1 for success in results.values() if success)
        logger.info(f"모든 봇 중지 요청 완료: {success_count}/{len(results)}개 성공")
        
        return results
    
    async def refresh_data(self, symbols: List[str]) -> bool:
        """공유 API의 차트 데이터 갱신"""
        try:
            if not self.shared_kiwoom_api or not self.shared_api_initialized:
                logger.error("공유 API가 초기화되지 않았습니다")
                return False
            
            logger.info(f"차트 데이터 갱신 시작: {len(symbols)}개 종목")
            
            # 차트 데이터 초기화 (120일 데이터)
            await self.shared_kiwoom_api.initialize_chart_data(symbols, period=120)
            
            # 두 전략 모두 지표 계산
            self.shared_kiwoom_api.stock_cache.calculate_envelope_indicators()
            logger.info("Envelope 지표 계산 완료")
            
            self.shared_kiwoom_api.stock_cache.calculate_bollinger_bands()
            logger.info("볼린저 밴드 지표 계산 완료")
            
            # 필요한 경우 모든 봇에게 업데이트 알림
            for email, bot in self.bots.items():
                if bot.trading_model and hasattr(bot.trading_model, 'refresh_indicators'):
                    await bot.trading_model.refresh_indicators()
                    logger.info(f"봇 {email}의 지표 갱신 완료")
            
            logger.info("차트 데이터 갱신 완료")
            return True
        
        except Exception as e:
            logger.error(f"차트 데이터 갱신 중 오류: {str(e)}")
            return False
    
    async def cleanup(self) -> None:
        """모든 봇 정리 (애플리케이션 종료 시 호출)"""
        logger.info("모든 봇 정리 시작...")
        
        # 모든 봇 중지
        await self.stop_all_bots()
        
        # 모든 봇 리소스 정리
        cleanup_tasks = []
        for email, bot in self.bots.items():
            task = asyncio.create_task(bot.cleanup())
            cleanup_tasks.append(task)
        
        # 모든 정리 태스크 완료 대기
        if cleanup_tasks:
            await asyncio.gather(*cleanup_tasks, return_exceptions=True)
        
        # 봇 딕셔너리 비우기
        self.bots.clear()
        
        # 공유 API 종료
        if self.shared_kiwoom_api:
            await self.shared_kiwoom_api.close()
            self.shared_kiwoom_api = None
        
        # 토큰 관리자 종료
        if self.shared_token_manager:
            await self.shared_token_manager.close()
            self.shared_token_manager = None
        
        self.shared_api_initialized = False
        
        logger.info("모든 봇 정리 완료")