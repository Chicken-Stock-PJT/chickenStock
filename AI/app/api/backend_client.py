"""
백엔드 API 클라이언트 - 인증 매니저 통합 버전
"""
import logging
import asyncio
from typing import Dict, List, Optional, Any
import aiohttp
from app.auth.auth_client import AuthClient
from app.config import settings

logger = logging.getLogger(__name__)

class BackendClient:
    """백엔드 API와 통신하는 클라이언트"""
    
    def __init__(self, auth_manager: Optional[AuthClient] = None):
        """클라이언트 초기화"""
        # 인증 매니저
        self.auth_manager = auth_manager
        
        # 트레이딩 모델 참조
        self.trading_model = None
        
        # 백엔드 URL
        self.backend_url = settings.BACKEND_API_URL
        
        # HTTP 세션
        self.session = None
        
        # 클라이언트 상태
        self.running = False
    
    def set_auth_manager(self, auth_manager: AuthClient):
        """인증 매니저 설정"""
        self.auth_manager = auth_manager
    
    def set_trading_model(self, trading_model):
        """트레이딩 모델 설정 (순환 참조 해결)"""
        self.trading_model = trading_model
    
    async def start(self):
        """클라이언트 시작"""
        if self.running:
            return True
        
        # HTTP 세션 생성
        if not self.session:
            self.session = aiohttp.ClientSession()
        
        self.running = True
        logger.info("백엔드 클라이언트 시작")
        return True
    
    async def close(self):
        """클라이언트 정지"""
        self.running = False
        
        # HTTP 세션 종료
        if self.session and not self.session.closed:
            await self.session.close()
            self.session = None
        
        logger.info("백엔드 클라이언트 정지")
        return True
    
    async def authenticated_request(self, method: str, endpoint: str, **kwargs) -> Optional[Dict]:
        """
        인증이 필요한 API 요청 공통 처리
        - 인증 상태 자동 확인 및 갱신
        - 요청 실패 시 1회 재시도 (인증 갱신 후)
        
        :param method: HTTP 메서드 ('get', 'post', 'put', 'delete')
        :param endpoint: API 엔드포인트 경로
        :param kwargs: 요청 파라미터 (json, data 등)
        :return: API 응답 데이터 또는 None(실패 시)
        """
        if not self.running:
            await self.start()
        
        # 인증 상태 확인
        if not self.auth_manager:
            logger.error("인증 매니저가 설정되지 않았습니다")
            return None
        
        # 인증 확인 및 필요시 갱신
        auth_ok = await self.auth_manager.ensure_authentication()
        if not auth_ok:
            logger.error("인증 실패. API 요청을 수행할 수 없습니다")
            return None
        
        # 인증 헤더 추가
        headers = self.auth_manager.get_authorization_header()
        if 'headers' in kwargs:
            kwargs['headers'].update(headers)
        else:
            kwargs['headers'] = headers
        
        # 완전한 URL 생성
        url = f"{self.backend_url}{endpoint}"
        
        try:
            # 요청 메서드에 따라 적절한 함수 호출
            http_method = getattr(self.session, method.lower())
            
            async with http_method(url, **kwargs) as response:
                if response.status in (200, 201):
                    return await response.json()
                elif response.status in (401, 403):
                    # 인증 오류 시 토큰 갱신 후 재시도
                    logger.warning(f"API 요청 인증 오류 (HTTP {response.status}). 토큰 갱신 후 재시도합니다")
                    
                    # 토큰 강제 갱신 (만료 시간과 관계없이)
                    refresh_ok = await self.auth_manager.refresh_access_token()
                    if not refresh_ok:
                        # 리프레시 실패 시 재로그인 시도
                        login_ok = await self.auth_manager.login(
                            self.auth_manager.email, 
                            self.auth_manager.password
                        )
                        if not login_ok:
                            logger.error("인증 갱신 및 재로그인 실패. API 요청을 수행할 수 없습니다")
                            return None
                    
                    # 인증 헤더 업데이트
                    headers = self.auth_manager.get_authorization_header()
                    if 'headers' in kwargs:
                        kwargs['headers'].update(headers)
                    else:
                        kwargs['headers'] = headers
                    
                    # 요청 재시도
                    async with http_method(url, **kwargs) as retry_response:
                        if retry_response.status in (200, 201):
                            return await retry_response.json()
                        else:
                            error_text = await retry_response.text()
                            logger.error(f"API 요청 재시도 실패: HTTP {retry_response.status}, {error_text}")
                            return None
                else:
                    error_text = await response.text()
                    logger.error(f"API 요청 실패: {endpoint} - HTTP {response.status}, {error_text}")
                    return None
        
        except Exception as e:
            logger.error(f"API 요청 중 오류: {endpoint} - {str(e)}")
            return None
    
    async def request_account_info(self):
        """백엔드 API에서 계좌 정보 요청"""
        result = await self.authenticated_request(
            'get', 
            '/api/members/holding-stocks'
        )
        
        if not result:
            return None
        
        try:
            # 계좌 정보 구성
            account_info = {
                "cash_balance": result.get("cashAmount", 0),
                "total_asset_value": 0,  # 초기값 설정
                "positions": {}
            }
            
            # 보유 종목 정보 업데이트
            for position in result.get("holdingStocks", []):
                code = position.get("stockCode")
                if code:
                    # 현재가는 API에서 제공하지 않으므로 평균 구매가로 대체 (추후 업데이트 필요)
                    current_price = position.get("averagePrice", 0)
                    quantity = position.get("quantity", 0)
                    avg_price = position.get("averagePrice", 0)
                    
                    account_info["positions"][code] = {
                        "code": code,
                        "name": position.get("stockName", ""),
                        "quantity": quantity,
                        "avgPrice": avg_price,
                    }
            
            # 총 자산 가치 계산 (현금 + 주식 평가 금액)
            stock_value = sum(pos["quantity"] * pos["avgPrice"] for pos in account_info["positions"].values())
            account_info["total_asset_value"] = account_info["cash_balance"] + stock_value
            
            return account_info
        except Exception as e:
            logger.error(f"계좌 정보 처리 중 오류: {str(e)}")
            return None
            
    async def get_all_stocks(self):
        """백엔드에서 모든 종목 정보 요청"""
        result = await self.authenticated_request(
            'get', 
            '/api/stocks/all'
        )
        
        if result:
            logger.info(f"종목 정보 조회 성공: {len(result)}개 종목")
        
        return result
    
    async def request_buy(self, symbol: str, quantity: int, price: float = 0) -> bool:
        """매수 요청을 백엔드 API로 전송"""
        # 요청 페이로드 구성
        payload = {
            "stockCode": symbol,
            "quantity": quantity,
            "price": "",  # 시장가로 설정
            "marketOrder": True
        }
        
        result = await self.authenticated_request(
            'post', 
            '/api/stock/trading/buy',
            json=payload
        )
        
        if result:
            logger.info(f"매수 요청 성공: {symbol} {quantity}주")
            return True
        else:
            logger.error(f"매수 요청 실패: {symbol} {quantity}주")
            return False
    
    async def request_sell(self, symbol: str, quantity: int, price: float = 0) -> bool:
        """매도 요청을 백엔드 API로 전송"""
        # 요청 페이로드 구성
        payload = {
            "stockCode": symbol,
            "quantity": quantity,
            "price": "",  # 시장가로 설정
            "marketOrder": True
        }
        
        result = await self.authenticated_request(
            'post', 
            '/api/stock/trading/sell',
            json=payload
        )
        
        if result:
            logger.info(f"매도 요청 성공: {symbol} {quantity}주")
            return True
        else:
            logger.error(f"매도 요청 실패: {symbol} {quantity}주")
            return False
    
    async def stop(self):
        """백엔드 클라이언트 정지"""
        await self.close()