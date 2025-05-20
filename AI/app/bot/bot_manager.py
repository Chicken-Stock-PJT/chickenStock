import logging
import asyncio
from typing import Dict, Optional, Any, List

from app.models.trade_models import TradingStrategy
from app.bot.bot_instance import BotInstance
from app.bot.bot_stock_cache import BotStockCache  # BotStockCache 임포트 추가

logger = logging.getLogger(__name__)


class BotManager:
    """여러 봇 인스턴스를 관리하는 클래스"""
    
    def __init__(self):
        """봇 관리자 초기화"""
        # 이메일을 키로 하는 봇 인스턴스 딕셔너리
        self.bots: Dict[str, BotInstance] = {}
        # 공유 StockCache 참조
        self.shared_stock_cache = None
        
        logger.info("봇 관리자 초기화 완료")
    
    async def initialize_shared_api(self):
        """공유 API 초기화 - 봇들이 공통으로 사용할 API 참조 설정"""
        logger.info("공유 API 초기화 완료")
        return True
    
    async def create_bot(self, email: str, password: str, strategy: TradingStrategy, shared_stock_cache=None) -> Optional[BotInstance]:
        try:
            # 이미 존재하는 봇인지 확인
            if email in self.bots:
                # 동일한 전략이면 기존 봇 반환
                if self.bots[email].strategy == strategy:
                    logger.info(f"이미 존재하는 봇 반환: {email} (전략: {strategy})")
                    # 이메일과 비밀번호 업데이트 (인증 정보 유지를 위해)
                    self.bots[email].email = email 
                    self.bots[email].password = password
                    return self.bots[email]
                else:
                    # 다른 전략이면 기존 봇 제거 후 새로 생성
                    logger.info(f"다른 전략의 봇이 존재하여 제거 후 재생성: {email} "
                            f"({self.bots[email].strategy} -> {strategy})")
                    await self.remove_bot(email)
            
            # 공유 캐시 결정 (지정된 것 없으면 관리자의 공유 캐시 사용)
            stock_cache_to_use = shared_stock_cache if shared_stock_cache else self.shared_stock_cache
            
            # 새 봇 인스턴스 생성 (공유 캐시 전달)
            # BotInstance의 생성자에 맞게 올바른 인자 전달
            bot = BotInstance(email, strategy, stock_cache_to_use)
            
            # 이메일 및 비밀번호 명시적 설정 (인증 정보 유지를 위해)
            bot.email = email
            bot.password = password
            
            # 봇 초기화 (로그인 등)
            if await bot.initialize(password, stock_cache_to_use):
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
    
    async def handle_realtime_data(self, symbol: str, price: float) -> None:
        """
        실시간 데이터 처리 (모든 봇에 전달)
        
        :param symbol: 종목 코드
        :param price: 현재가
        """
        # 실행 중인 봇만 처리
        running_bots = self.get_running_bots()
        
        # 병렬 처리 태스크 생성
        tasks = []
        for email, bot in running_bots.items():
            tasks.append(bot.handle_realtime_price(symbol, price))
        
        # 모든 봇의 실시간 데이터 처리를 병렬로 실행
        if tasks:
            await asyncio.gather(*tasks, return_exceptions=True)
    
    async def refresh_all_bot_indicators(self) -> Dict[str, int]:
        """모든 봇의 지표 새로고침"""
        results = {}
        refresh_tasks = []
        
        # 각 봇마다 별도의 태스크 생성
        for email, bot in self.bots.items():
            # 비동기 함수를 호출하는 태스크 생성
            task = asyncio.create_task(self._refresh_bot_indicators(email, bot))
            refresh_tasks.append(task)
        
        # 모든 태스크 완료 대기
        if refresh_tasks:
            await asyncio.gather(*refresh_tasks, return_exceptions=True)
            
            # 결과 수집
            for email, bot in self.bots.items():
                if hasattr(bot, 'last_refresh_count'):
                    results[email] = getattr(bot, 'last_refresh_count', 0)
        
        return results
    
    async def _refresh_bot_indicators(self, email: str, bot: BotInstance) -> int:
        """개별 봇 지표 새로고침 (내부 사용)"""
        try:
            success_count = await bot.refresh_indicators()
            # 결과를 봇 객체에 임시 저장
            bot.last_refresh_count = success_count
            logger.info(f"봇 {email} 지표 새로고침 완료: {success_count}개 종목")
            return success_count
        except Exception as e:
            logger.error(f"봇 {email} 지표 새로고침 중 오류: {str(e)}")
            return 0
    
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
        
        # 공유 캐시 참조 정리
        self.shared_stock_cache = None
        
        logger.info("모든 봇 정리 완료")