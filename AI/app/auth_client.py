import logging
import asyncio
from typing import Dict, Optional
from datetime import datetime, timedelta
import aiohttp

from app.config import settings

logger = logging.getLogger(__name__)

class AuthClient:
    """인증 및 토큰 관리를 위한 클라이언트"""
    
    def __init__(self):
        """인증 클라이언트 초기화"""
        # 백엔드 URL
        self.backend_url = settings.BACKEND_API_URL
        
        # 인증 정보
        self.access_token = None
        self.refresh_token = None
        self.access_token_expires_at = None
        
        # 세션
        self.session = None
        
        # 현재 상태
        self.is_authenticated = False
    
    async def initialize(self):
        """초기화 및 세션 생성"""
        if not self.session or self.session.closed:
            self.session = aiohttp.ClientSession(
                headers={
                    "Content-Type": "application/json",
                    "Accept": "application/json"
                }
            )
        return True
    
    async def login(self, email: str, password: str) -> bool:
        """백엔드 API로 로그인"""
        try:
            if not self.session:
                await self.initialize()
            
            login_data = {
                "email": "cuteai@gmail.com",
                "password": "ssafy123!@#",
                "platform": "mobile"
            }
            
            async with self.session.post(
                f"{self.backend_url}/api/auth/login",
                json=login_data
            ) as response:
                if response.status == 200:
                    auth_data = await response.json()
                    
                    # 토큰 저장
                    self.access_token = auth_data.get("accessToken")
                    self.refresh_token = auth_data.get("refreshToken")
                    
                    # 만료 시간 설정
                    access_expires_in = auth_data.get("accessTokenExpiresIn", 3600)
                    self.access_token_expires_at = datetime.now() + timedelta(seconds=access_expires_in)
                    
                    # 세션 헤더에 토큰 추가
                    self.session.headers.update({
                        "Authorization": f"Bearer {self.access_token}"
                    })
                    
                    self.is_authenticated = True
                    logger.info("백엔드 로그인 성공")
                    
                    # 토큰 갱신 태스크 시작
                    asyncio.create_task(self._token_refresh_loop())
                    
                    return True
                else:
                    logger.error(f"백엔드 로그인 실패: HTTP {response.status}")
                    return False
        
        except Exception as e:
            logger.error(f"로그인 중 오류: {str(e)}")
            return False
    
    async def refresh_access_token(self) -> bool:
        """리프레시 토큰을 사용하여 액세스 토큰 갱신"""
        try:
            if not self.refresh_token:
                logger.error("리프레시 토큰이 없습니다.")
                return False
            
            refresh_data = {
                "refreshToken": self.refresh_token
            }
            
            async with self.session.post(
                f"{self.backend_url}/api/auth/refresh",
                json=refresh_data
            ) as response:
                if response.status == 200:
                    token_data = await response.json()
                    
                    # 새 토큰 저장
                    self.access_token = token_data.get("accessToken")
                    
                    # 만료 시간 갱신
                    access_expires_in = token_data.get("accessTokenExpiresIn", 3600)
                    self.access_token_expires_at = datetime.now() + timedelta(seconds=access_expires_in)
                    
                    # 새 리프레시 토큰이 있다면 갱신
                    if "refreshToken" in token_data:
                        self.refresh_token = token_data.get("refreshToken")
                    
                    # 세션 헤더 업데이트
                    self.session.headers.update({
                        "Authorization": f"Bearer {self.access_token}"
                    })
                    
                    logger.info("액세스 토큰 갱신 성공")
                    return True
                else:
                    logger.error(f"액세스 토큰 갱신 실패: HTTP {response.status}")
                    self.is_authenticated = False
                    return False
        
        except Exception as e:
            logger.error(f"토큰 갱신 중 오류: {str(e)}")
            self.is_authenticated = False
            return False
    
    async def _token_refresh_loop(self):
        """토큰 자동 갱신 루프"""
        try:
            while self.is_authenticated:
                # 토큰 만료 10분 전에 갱신
                if self.access_token_expires_at:
                    now = datetime.now()
                    time_to_expiry = (self.access_token_expires_at - now).total_seconds()
                    
                    if time_to_expiry <= 600:  # 10분(600초) 이하로 남은 경우
                        logger.info("액세스 토큰 만료 시간이 10분 이내로 남아 갱신 시작")
                        await self.refresh_access_token()
                    else:
                        # 다음 갱신 시간까지 대기 (토큰 만료 10분 전)
                        wait_time = min(time_to_expiry - 600, 300)
                        await asyncio.sleep(wait_time)
                else:
                    # 만료 시간 정보가 없는 경우 30분마다 갱신
                    await asyncio.sleep(1800)
                    await self.refresh_access_token()
        
        except asyncio.CancelledError:
            logger.info("토큰 갱신 루프 취소됨")
        except Exception as e:
            logger.error(f"토큰 갱신 루프 오류: {str(e)}")
    
    def get_authorization_header(self) -> Dict[str, str]:
        """Authorization 헤더 반환"""
        if self.access_token:
            return {"Authorization": f"Bearer {self.access_token}"}
        return {}
    
    def is_token_valid(self) -> bool:
        """현재 액세스 토큰이 유효한지 확인"""
        if not self.access_token or not self.access_token_expires_at:
            return False
        
        # 현재 시간이 만료 시간보다 이전인지 확인
        return datetime.now() < self.access_token_expires_at
    
    async def close(self):
        """클라이언트 종료"""
        if self.session and not self.session.closed:
            await self.session.close()
        
        self.is_authenticated = False
        logger.info("인증 클라이언트 종료")