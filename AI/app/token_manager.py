import os
import logging
import aiohttp
import asyncio
from datetime import datetime, timedelta
from typing import Dict, Optional

from app.config import settings

logger = logging.getLogger(__name__)

class TokenManager:
    """키움증권 API 토큰 관리 클래스"""
    
    def __init__(self):
        """토큰 관리자 초기화"""
        self.base_url = settings.API_BASE_URL
        self.appkey = settings.KIWOOM_API_KEY
        self.secretkey = settings.KIWOOM_API_SECRET
        
        # 토큰 정보
        self.token = None
        self.token_type = None
        self.expires_dt = None
        self.session = None
        
        # 토큰 갱신 태스크
        self.refresh_task = None

    async def initialize(self):
        """토큰 관리자 초기화 및 첫 토큰 발급"""
        self.session = aiohttp.ClientSession()
        await self.get_token()
        
        # 토큰 갱신 태스크 시작
        self.refresh_task = asyncio.create_task(self._token_refresh_loop())
        
        return self.is_valid()

    async def close(self):
        """토큰 관리자 종료"""
        if self.refresh_task:
            self.refresh_task.cancel()
            try:
                await self.refresh_task
            except asyncio.CancelledError:
                pass
            self.refresh_task = None
            
        if self.session and not self.session.closed:
            await self.session.close()
            self.session = None

    async def get_token(self) -> Optional[str]:
        """토큰 발급 또는 갱신"""
        if not self.session:
            self.session = aiohttp.ClientSession()
        
        try:
            # API 명세에 맞게 요청 데이터 구성
            data = {
                "grant_type": "client_credentials",
                "appkey": self.appkey,
                "secretkey": self.secretkey
            }
            
            async with self.session.post(f"{self.base_url}/oauth2/tokenP", data=data) as response:
                if response.status == 200:
                    result = await response.json()
                    
                    # 응답 데이터 파싱
                    self.token = result.get("token")
                    self.token_type = result.get("token_type", "Bearer")
                    self.expires_dt = result.get("expires_dt")
                    
                    # 만료 시간 파싱
                    if self.expires_dt:
                        try:
                            # 만료 시간 형식에 따라 파싱 방법 조정 필요
                            # expires_dt가 ISO 8601 형식이라고 가정
                            self.expires_dt = datetime.fromisoformat(self.expires_dt)
                        except ValueError:
                            # 다른 형식일 경우 추가 처리
                            logger.warning(f"알 수 없는 만료 시간 형식: {self.expires_dt}")
                            # 기본값으로 1시간 후로 설정
                            self.expires_dt = datetime.now() + timedelta(hours=1)
                    else:
                        # 만료 시간이 없으면 기본값으로 1시간 후로 설정
                        self.expires_dt = datetime.now() + timedelta(hours=1)
                    
                    logger.info(f"토큰 발급 성공: 만료 시간 {self.expires_dt}")
                    return self.token
                else:
                    response_text = await response.text()
                    logger.error(f"토큰 발급 실패: HTTP {response.status}, {response_text}")
                    return None
        
        except Exception as e:
            logger.error(f"토큰 발급 중 오류: {str(e)}")
            return None

    def is_valid(self) -> bool:
        """현재 토큰이 유효한지 확인"""
        if not self.token or not self.expires_dt:
            return False
        
        # 현재 시간이 만료 시간보다 이전인지 확인
        return datetime.now() < self.expires_dt

    async def get_valid_token(self) -> Optional[str]:
        """유효한 토큰 반환 (필요시 갱신)"""
        if not self.is_valid():
            await self.get_token()
        
        return self.token

    async def _token_refresh_loop(self):
        """토큰 자동 갱신 태스크"""
        while True:
            try:
                if self.expires_dt:
                    now = datetime.now()
                    
                    # 만료 시간까지 남은 시간 계산 (초 단위)
                    time_to_expiry = (self.expires_dt - now).total_seconds()
                    
                    if time_to_expiry <= 300:  # 5분(300초) 이하로 남은 경우 즉시 갱신
                        logger.info("토큰 만료 시간이 5분 이내로 남아 갱신 시작")
                        await self.get_token()
                    else:
                        # 다음 갱신 시간까지 대기 (토큰 만료 5분 전)
                        wait_time = time_to_expiry - 300
                        await asyncio.sleep(min(wait_time, 3600))  # 최대 1시간씩 대기
                else:
                    # 만료 시간이 없으면 1시간마다 갱신
                    await asyncio.sleep(3600)
            except asyncio.CancelledError:
                # 태스크가 취소됨
                break
            except Exception as e:
                logger.error(f"토큰 갱신 태스크 오류: {str(e)}")
                await asyncio.sleep(60)  # 오류 발생 시 1분 후 재시도

    def get_auth_header(self) -> Dict[str, str]:
        """인증 헤더 반환"""
        if not self.token:
            return {}
        
        return {"Authorization": f"{self.token_type} {self.token}"}