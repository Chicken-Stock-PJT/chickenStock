"""
키움 API 토큰 관리
"""
import logging
from datetime import datetime, timedelta

logger = logging.getLogger(__name__)

class TokenManager:
    """키움 API 토큰 관리"""
    
    def __init__(self):
        self.token = None
        self.token_type = None
        self.expires_dt = None
    
    async def initialize(self):
        """토큰 관리자 초기화"""
        logger.info("토큰 관리자 초기화")
        return True
    
    def is_valid(self):
        """토큰이 유효한지 확인"""
        if not self.token or not self.expires_dt:
            return False
        return datetime.now() < self.expires_dt
    
    def update_token(self, token, token_type, expires_dt):
        """토큰 정보 업데이트"""
        self.token = token
        self.token_type = token_type
        self.expires_dt = expires_dt
        logger.info(f"토큰 업데이트 완료: 만료 시간 {expires_dt.isoformat()}")
    
    async def close(self):
        """리소스 정리"""
        self.token = None
        self.token_type = None
        self.expires_dt = None
        logger.info("토큰 관리자 종료")