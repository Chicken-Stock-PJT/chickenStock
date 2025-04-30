import aiohttp
import logging
from datetime import datetime, timedelta
from app.config import settings

logger = logging.getLogger(__name__)

class KiwoomAuthClient:
    """키움 API 토큰 발급 및 관리 클래스"""
    
    def __init__(self):
        """초기화"""
        self.base_url = settings.API_BASE_URL  # 모의투자/운영 도메인 설정
        self.appkey = settings.KIWOOM_API_KEY
        self.secretkey = settings.KIWOOM_API_SECRET
        
        # 토큰 관련 정보
        self.access_token = None
        self.token_expires_at = None
        
    async def get_access_token(self) -> str:
        """키움 API 액세스 토큰 발급"""
        # 기존 토큰이 유효한 경우 그대로 반환
        if self.access_token and self.token_expires_at and datetime.now() < self.token_expires_at:
            return self.access_token
        
        try:
            # 토큰 요청 페이로드 구성
            payload = {
                "grant_type": "client_credentials",
                "appkey": self.appkey,
                "secretkey": self.secretkey
            }
            
            # 토큰 발급 요청
            async with aiohttp.ClientSession() as session:
                async with session.post(
                    f"{self.base_url}/oauth2/token",
                    json=payload,
                    headers={
                        "Content-Type": "application/json;charset=UTF-8"
                    }
                ) as response:
                    # 응답 처리
                    if response.status == 200:
                        result = await response.json()

                        # 토큰 정보 저장
                        self.token = result.get("token")
                        
                        logger.info("키움 API 토큰 발급 성공")

                        return self.token
                    else:
                        # 오류 응답 처리
                        error_text = await response.text()
                        logger.error(f"키움 API 토큰 발급 실패: HTTP {response.status}, {error_text}")
                        return None
        
        except Exception as e:
            logger.error(f"키움 API 토큰 발급 중 오류: {str(e)}")
            return None