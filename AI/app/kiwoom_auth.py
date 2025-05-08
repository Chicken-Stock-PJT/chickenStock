import logging
from app.token_manager import TokenManager

logger = logging.getLogger(__name__)

class KiwoomAuthClient:
    """키움 API 토큰 사용 클래스 (TokenManager 활용)"""
    
    def __init__(self):
        """초기화"""
        # TokenManager 인스턴스 가져오기
        self.token_manager = TokenManager()
    
    async def get_access_token(self) -> str:
        """키움 API 액세스 토큰 반환 (TokenManager에서 관리)"""
        # TokenManager에서 유효한 토큰 가져오기
        return await self.token_manager.get_valid_token()