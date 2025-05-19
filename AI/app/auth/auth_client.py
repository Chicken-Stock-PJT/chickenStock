import logging
import asyncio
from typing import Optional, Dict
from datetime import datetime, timedelta
import aiohttp
from app.config import settings

logger = logging.getLogger(__name__)

class AuthClient:
    """
    인증 상태 중앙 관리 매니저
    - 토큰 유효성 검사 및 자동 갱신 처리
    - 인증 실패 시 자동 재로그인
    - 모든 인증 관련 로직 중앙화
    """
    
    def __init__(self):
        """인증 매니저 초기화"""
        # 백엔드 URL
        self.backend_url = settings.BACKEND_API_URL
        
        # 인증 정보
        self.email = None
        self.password = None
        self.access_token = None
        self.refresh_token = None
        self.access_token_expires_at = None
        
        # 세션
        self.session = None
        
        # 현재 상태
        self.is_authenticated = False
        
        # 토큰 갱신 태스크
        self._refresh_task = None
        
        # 마지막 인증 확인 시간
        self.last_auth_check = None
    
    async def initialize(self) -> bool:
        """초기화 및 세션 생성"""
        try:
            if not self.session or self.session.closed:
                self.session = aiohttp.ClientSession(
                    headers={
                        "Content-Type": "application/json",
                        "Accept": "application/json"
                    }
                )
            logger.info("인증 매니저 초기화 완료")
            return True
        except Exception as e:
            logger.error(f"인증 매니저 초기화 오류: {str(e)}")
            return False
    
    async def login(self, email: str, password: str) -> bool:
        """
        백엔드 API로 로그인
        
        :param email: 사용자 이메일
        :param password: 사용자 비밀번호
        :return: 로그인 성공 여부
        """
        try:
            # 인증 정보 저장
            self.email = email
            self.password = password
            
            if not self.session:
                await self.initialize()
            
            login_data = {
                "email": email,
                "password": password,
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
                    
                    # 만료 시간 계산
                    expires_in = 3600 * 9
                    self.access_token_expires_at = datetime.now() + timedelta(seconds=expires_in)
                    
                    # 세션 헤더에 토큰 추가
                    self.session.headers.update({
                        "Authorization": f"Bearer {self.access_token}"
                    })
                    
                    self.is_authenticated = True
                    self.last_auth_check = datetime.now()
                    
                    logger.info(f"백엔드 로그인 성공: {email}")
                    
                    # 이전 토큰 갱신 태스크가 있으면 취소
                    if self._refresh_task:
                        self._refresh_task.cancel()
                    
                    # 새로운 토큰 갱신 태스크 시작
                    self._refresh_task = asyncio.create_task(self._token_refresh_loop())
                    
                    return True
                else:
                    error_text = await response.text()
                    logger.error(f"백엔드 로그인 실패: HTTP {response.status}, {error_text}")
                    self.is_authenticated = False
                    return False
        
        except Exception as e:
            logger.error(f"로그인 중 오류: {str(e)}")
            self.is_authenticated = False
            return False
    
    async def refresh_access_token(self) -> bool:
        """
        리프레시 토큰을 사용하여 액세스 토큰 갱신
        
        :return: 토큰 갱신 성공 여부
        """
        try:
            if not self.refresh_token:
                logger.error("리프레시 토큰이 없습니다.")
                return False
            
            refresh_data = {
                "accessToken": self.access_token,
                "refreshToken": self.refresh_token
            }
            
            async with self.session.post(
                f"{self.backend_url}/api/auth/token/refresh-mobile",
                json=refresh_data
            ) as response:
                if response.status == 200:
                    token_data = await response.json()
                    
                    # 새 토큰 저장
                    self.access_token = token_data.get("accessToken")
                    self.refresh_token = token_data.get("refreshToken")
                    
                    # 만료 시간 갱신
                    access_expires_in = 3600 * 9
                    self.access_token_expires_at = datetime.now() + timedelta(seconds=access_expires_in)
                    
                    # 세션 헤더 업데이트
                    self.session.headers.update({
                        "Authorization": f"Bearer {self.access_token}"
                    })
                    
                    self.is_authenticated = True
                    self.last_auth_check = datetime.now()
                    
                    logger.info(f"액세스 토큰 갱신 성공: {self.email}")
                    return True
                else:
                    error_text = await response.text()
                    logger.error(f"액세스 토큰 갱신 실패: HTTP {response.status}, {error_text}")
                    self.is_authenticated = False
                    return False
        
        except Exception as e:
            logger.error(f"토큰 갱신 중 오류: {str(e)}")
            self.is_authenticated = False
            return False
    
    async def ensure_authentication(self) -> bool:
        """
        인증 상태 확인 및 필요시 토큰 갱신 또는 재로그인
        - 토큰이 만료되었거나 곧 만료될 예정이면 갱신 시도
        - 갱신 실패 시 재로그인 시도
        
        :return: 인증 성공 여부
        """
        try:
            # 마지막 인증 확인 시간이 10분 이내면 재확인 스킵 (최적화)
            if self.last_auth_check:
                time_since_check = (datetime.now() - self.last_auth_check).total_seconds()
                if time_since_check < 600 and self.is_authenticated:
                    return True
            
            # 세션 초기화 확인
            if not self.session or self.session.closed:
                await self.initialize()
            
            # 토큰 유효성 확인
            if not self.is_token_valid():
                logger.info(f"인증 토큰이 유효하지 않거나 곧 만료됩니다: {self.email}")
                
                # 1. 리프레시 토큰으로 갱신 시도
                if self.refresh_token:
                    refresh_success = await self.refresh_access_token()
                    if refresh_success:
                        return True
                
                # 2. 갱신 실패 시 재로그인 시도
                if self.email and self.password:
                    return await self.login(self.email, self.password)
                else:
                    logger.error("인증 정보(이메일/비밀번호)가 없어 재로그인할 수 없습니다")
                    self.is_authenticated = False
                    return False
            
            # 토큰이 유효한 경우
            self.last_auth_check = datetime.now()
            return True
        
        except Exception as e:
            logger.error(f"인증 확인 중 오류: {str(e)}")
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
                        logger.info(f"액세스 토큰 만료 시간이 10분 이내로 남아 갱신 시작: {self.email}")
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
            logger.info(f"토큰 갱신 루프 취소됨: {self.email}")
        except Exception as e:
            logger.error(f"토큰 갱신 루프 오류: {str(e)}")
    
    def is_token_valid(self) -> bool:
        """
        현재 액세스 토큰이 유효한지 확인
        - 만료 시간이 15분 이상 남아있으면 유효한 것으로 간주
        
        :return: 토큰 유효 여부
        """
        if not self.access_token or not self.access_token_expires_at:
            return False
        
        # 현재 시간이 만료 시간 15분 이전인지 확인 (여유 있게 체크)
        time_to_expiry = (self.access_token_expires_at - datetime.now()).total_seconds()
        return time_to_expiry > 900  # 15분(900초) 이상 남아있어야 유효
    
    def get_authorization_header(self) -> Dict[str, str]:
        """
        Authorization 헤더 반환
        
        :return: 인증 헤더 딕셔너리
        """
        if self.access_token:
            return {"Authorization": f"Bearer {self.access_token}"}
        return {}
    
    async def close(self):
        """인증 매니저 종료"""
        try:
            # 토큰 갱신 태스크 취소
            if self._refresh_task:
                self._refresh_task.cancel()
                try:
                    await self._refresh_task
                except asyncio.CancelledError:
                    pass
                self._refresh_task = None
            
            # 세션 종료
            if self.session and not self.session.closed:
                await self.session.close()
                self.session = None
            
            self.is_authenticated = False
            logger.info(f"인증 매니저 종료: {self.email}")
            
            return True
        except Exception as e:
            logger.error(f"인증 매니저 종료 중 오류: {str(e)}")
            return False