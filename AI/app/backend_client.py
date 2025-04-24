import logging
import aiohttp
from typing import Dict, Any
from datetime import datetime

from .config import settings
from .models import TradeDecision

logger = logging.getLogger(__name__)

class BackendClient:
    def __init__(self, trading_model):
        """백엔드 API 클라이언트 초기화"""
        self.api_base_url = settings.BACKEND_API_URL
        self.session = None
        self.trading_model = trading_model
        
    async def start(self):
        """클라이언트 세션 초기화"""
        self.session = aiohttp.ClientSession()
        logger.info("Backend client started")
        return True
    
    async def stop(self):
        """클라이언트 세션 종료"""
        if self.session:
            await self.session.close()
            self.session = None
        logger.info("Backend client stopped")
        return True
    
    async def fetch_account_info(self):
        """백엔드 서버에서 계좌 정보 가져오기"""
        if not self.session:
            self.session = aiohttp.ClientSession()
            
        try:
            async with self.session.get(f"{self.api_base_url}/account/info") as response:
                if response.status == 200:
                    data = await response.json()
                    logger.info(f"계좌 정보 조회 성공: 예수금={data['cash_balance']}, 포지션={len(data['positions'])}")
                    return data
                else:
                    logger.error(f"계좌 정보 조회 실패: HTTP {response.status}")
                    return None
        except Exception as e:
            logger.error(f"계좌 정보 조회 중 오류: {str(e)}")
            return None
    
    async def send_trade_result(self, decision: TradeDecision):
        """거래 결과를 백엔드로 전송"""
        if not self.session:
            self.session = aiohttp.ClientSession()
            
        try:
            # 백엔드 API로 거래 결과 전송
            async with self.session.post(
                f"{self.api_base_url}/trade/result", 
                json=decision.dict()
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    logger.info(f"Successfully sent trade result to backend: {decision.action} {decision.symbol}")
                    return data
                else:
                    logger.error(f"Failed to send trade result: HTTP {response.status}")
                    response_text = await response.text()
                    logger.error(f"Response: {response_text}")
                    return None
        except Exception as e:
            logger.error(f"Error sending trade result: {str(e)}")
            return None