import logging
import asyncio
from typing import Dict, List, Optional, Any
from datetime import datetime

from app.kiwoom_api import KiwoomAPI
from app.auth_client import AuthClient
from app.backend_client import BackendClient
from app.envelope_trading import TradingModel as EnvelopeTradingModel
from app.bollinger_band_trading import BollingerBandTradingModel
from app.bot_instance import BotInstance, TradingStrategy

logger = logging.getLogger(__name__)

class BotManager:
    """여러 봇 인스턴스를 관리하는 클래스"""
    
    def __init__(self):
        """봇 관리자 초기화"""
        # 이메일을 키로 하는 봇 인스턴스 딕셔너리
        self.bots: Dict[str, BotInstance] = {}
        logger.info("봇 관리자 초기화 완료")
    
    async def create_bot(self, email: str, password: str, strategy: TradingStrategy) -> Optional[BotInstance]:
        """새로운 봇 생성 및 초기화"""
        try:
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
            
            # 봇 초기화 (로그인 등)
            if await bot.initialize(password):
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
        return {email: bot for email, bot in self.bots.items() if bot.is_running}
    
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
    
    async def cleanup(self) -> None:
        """모든 봇 정리 (애플리케이션 종료 시 호출)"""
        logger.info("모든 봇 정리 시작...")
        
        cleanup_tasks = []
        for email, bot in self.bots.items():
            task = asyncio.create_task(bot.cleanup())
            cleanup_tasks.append(task)
        
        # 모든 정리 태스크 완료 대기
        if cleanup_tasks:
            await asyncio.gather(*cleanup_tasks, return_exceptions=True)
        
        # 봇 딕셔너리 비우기
        self.bots.clear()
        
        logger.info("모든 봇 정리 완료")
    
    def get_bot_count(self) -> Dict[str, int]:
        """봇 수 통계 조회"""
        running_count = sum(1 for bot in self.bots.values() if bot.is_running)
        
        return {
            "total": len(self.bots),
            "running": running_count,
            "stopped": len(self.bots) - running_count
        }
    
    async def get_bot_status_summary(self) -> Dict[str, Any]:
        """모든 봇의 상태 요약 정보"""
        summary = {
            "total_bots": len(self.bots),
            "running_bots": 0,
            "strategies": {
                TradingStrategy.ENVELOPE.value: 0,
                TradingStrategy.BOLLINGER.value: 0
            },
            "total_asset_value": 0,
            "bots": []
        }
        
        for email, bot in self.bots.items():
            # 봇 상태 정보 추가
            bot_status = bot.get_status()
            summary["bots"].append(bot_status)
            
            # 실행 중인 봇 카운트
            if bot.is_running:
                summary["running_bots"] += 1
            
            # 전략별 카운트
            summary["strategies"][bot.strategy.value] += 1
            
            # 자산 가치 합산 (키움 API가 초기화된 경우만)
            if bot.kiwoom_api and "total_asset_value" in bot.kiwoom_api.account_info:
                summary["total_asset_value"] += bot.kiwoom_api.account_info["total_asset_value"]
        
        return summary