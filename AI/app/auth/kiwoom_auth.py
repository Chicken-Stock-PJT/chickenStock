"""
키움 API 인증 처리
"""
import logging
import aiohttp
from datetime import datetime, timedelta
from app.config import settings

logger = logging.getLogger(__name__)

class KiwoomAuthClient:
    """키움 API 인증 처리"""
    
    def __init__(self):
        self.base_url = settings.API_AUTH_URL
        self.token_manager = None
        self.session = None
    
    def set_token_manager(self, token_manager):
        """토큰 관리자 설정"""
        self.token_manager = token_manager
    
    async def initialize(self):
        """클라이언트 초기화"""
        if not self.session:
            self.session = aiohttp.ClientSession()
        return True
    
    async def get_access_token(self):
        """키움 API 액세스 토큰 발급 또는 갱신"""
        try:
            # 토큰이 이미 유효하면 반환
            if self.token_manager and self.token_manager.is_valid():
                return self.token_manager.token
            
            # 세션 확인
            if not self.session:
                await self.initialize()
            
            # 실제 환경에서는 키움 API 인증 요청
            # 여기서는 간단한 예시로 구현
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
                    expires_in = token_data.get("expires_in", 86400)  # 기본 1일
                    
                    # 만료 시간 계산
                    expires_dt = datetime.now() + timedelta(seconds=expires_in)
                    
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
    
    async def close(self):
        """리소스 정리"""
        if self.session and not self.session.closed:
            await self.session.close()
            self.session = None