"""
키움 API 인증 처리
"""
import logging
import aiohttp
import asyncio
from datetime import datetime, timedelta
from app.config import settings

logger = logging.getLogger(__name__)

class KiwoomAuthClient:
    """키움 API 인증 처리"""
    
    def __init__(self):
        self.base_url = settings.API_BASE_URL
        self.token_manager = None
        self.session = None
        self.token_lock = asyncio.Lock()  # 토큰 갱신을 동기화하기 위한 락
    
    def set_token_manager(self, token_manager):
        """토큰 관리자 설정"""
        self.token_manager = token_manager
    
    async def initialize(self):
        """클라이언트 초기화"""
        if not self.session:
            self.session = aiohttp.ClientSession()
        return True
    
    async def get_access_token(self):
        """현재 토큰 반환 (토큰이 없을 때만 새로 발급)"""
        async with self.token_lock:  # 락을 사용하여 동시 갱신 방지
            # 토큰이 이미 있으면 반환
            if self.token_manager and self.token_manager.token:
                return self.token_manager.token
            
            # 토큰이 없을 때만 새로 발급
            return await self._issue_new_token()
    
    async def refresh_token(self):
        """토큰 갱신 (명시적 호출 시에만)"""
        async with self.token_lock:
            return await self._issue_new_token()
    
    async def _issue_new_token(self):
        """실제 토큰 발급 로직"""
        try:
            # 세션 확인
            if not self.session:
                await self.initialize()
            
            logger.info("키움 API 토큰 발급 요청")
            # 실제 환경에서는 키움 API 인증 요청
            async with self.session.post(
                f"{self.base_url}/oauth2/token",
                json={
                    "grant_type": "client_credentials",
                    "appkey": settings.KIWOOM_APP_KEY,
                    "secretkey": settings.KIWOOM_APP_SECRET
                }
            ) as response:
                if response.status == 200:
                    token_data = await response.json()
                    
                    # 토큰 정보 추출
                    token = token_data.get("token")
                    token_type = token_data.get("token_type", "Bearer")
                    expires_in = token_data.get("expires_dt")

                    expires_dt = datetime.strptime(expires_in, "%Y%m%d%H%M%S")
                    
                    # 토큰 관리자에 저장
                    if self.token_manager:
                        self.token_manager.update_token(token, token_type, expires_dt)
                    
                    logger.info("키움 API 토큰 발급 성공")
                    return token
                else:
                    logger.error(f"키움 API 토큰 발급 실패: HTTP {response.status}")
                    return None
        
        except Exception as e:
            logger.error(f"키움 API 토큰 발급 중 오류: {str(e)}")
            return None