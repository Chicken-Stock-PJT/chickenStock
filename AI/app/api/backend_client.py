"""
백엔드 API 클라이언트
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
    
    def __init__(self, auth_client: Optional[AuthClient] = None):
        """클라이언트 초기화"""
        # 인증 클라이언트
        self.auth_client = auth_client
        
        # 트레이딩 모델 참조
        self.trading_model = None
        
        # 백엔드 URL
        self.backend_url = settings.BACKEND_API_URL
        
        # HTTP 세션
        self.session = None
        
        # 클라이언트 상태
        self.running = False
    
    def set_auth_client(self, auth_client: AuthClient):
        """인증 클라이언트 설정"""
        self.auth_client = auth_client
    
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
    
    async def request_account_info(self):
        """백엔드 API에서 계좌 정보 요청"""
        try:
            if not self.running:
                await self.start()
                
            # 인증 상태 확인
            if not self.auth_client or not self.auth_client.is_authenticated:
                logger.error("인증되지 않았습니다. 계좌 정보를 요청할 수 없습니다.")
                return None
                
            # 백엔드 API 요청 헤더
            headers = self.auth_client.get_authorization_header()
            
            # 백엔드 API에서 보유 종목 정보 요청 (새로운 엔드포인트)
            async with self.session.get(
                f"{self.backend_url}/api/members/holding-stocks",
                headers=headers
            ) as response:
                if response.status == 200:
                    holding_data = await response.json()
                    
                    # 계좌 정보 구성
                    account_info = {
                        "cash_balance": holding_data.get("cashAmount", 0),
                        "total_asset_value": 0,  # 초기값 설정
                        "positions": {}
                    }
                    
                    # 보유 종목 정보 업데이트
                    for position in holding_data.get("holdingStocks", []):
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
                else:
                    logger.error(f"계좌 정보 조회 실패: HTTP {response.status}")
                    return None
        
        except Exception as e:
            logger.error(f"계좌 정보 요청 중 오류: {str(e)}")
            return None
            
    async def get_all_stocks(self):
        """백엔드에서 모든 종목 정보 요청"""
        try:
            if not self.running:
                await self.start()
                
            # 인증 상태 확인
            if not self.auth_client or not self.auth_client.is_authenticated:
                logger.error("인증되지 않았습니다. 종목 정보를 요청할 수 없습니다.")
                return None
                
            # 백엔드 API 요청 헤더
            headers = self.auth_client.get_authorization_header()
            
            # 백엔드 API에서 모든 종목 정보 요청
            async with self.session.get(
                f"{self.backend_url}/api/stocks/all",
                headers=headers
            ) as response:
                if response.status == 200:
                    stock_list = await response.json()
                    logger.info(f"종목 정보 조회 성공: {len(stock_list)}개 종목")
                    return stock_list
                else:
                    logger.error(f"종목 정보 요청 실패: HTTP {response.status}")
                    return None
        
        except Exception as e:
            logger.error(f"종목 정보 요청 중 오류: {str(e)}")
            return None
    
    async def request_buy(self, symbol: str, quantity: int, price: float = 0) -> bool:
        """매수 요청을 백엔드 API로 전송"""
        try:
            if not self.running:
                await self.start()
            
            # 인증 상태 확인
            if not self.auth_client or not self.auth_client.is_authenticated:
                logger.error("인증되지 않았습니다. 매수 요청을 보낼 수 없습니다.")
                return False
            
            # 백엔드 API 요청 헤더
            headers = self.auth_client.get_authorization_header()
            
            # 요청 페이로드 구성
            payload = {
                "stockCode": symbol,
                "quantity": quantity,
                "price": "",  # 시장가로 설정
                "marketOrder": True
            }
            
            # 백엔드 API 엔드포인트
            endpoint = f"{self.backend_url}/api/stock/trading/buy"
            
            # 매수 요청 전송
            async with self.session.post(
                endpoint,
                json=payload,
                headers=headers
            ) as response:
                if response.status == 200 or response.status == 201:
                    result = await response.json()
                    logger.info(f"매수 요청 성공: {symbol} {quantity}주")
                    
                    return True
                else:
                    error_text = await response.text()
                    logger.error(f"매수 요청 실패: {symbol} - HTTP {response.status}, {error_text}")
                    return False
        
        except Exception as e:
            logger.error(f"매수 요청 처리 중 오류: {str(e)}")
            return False
    
    async def request_sell(self, symbol: str, quantity: int, price: float = 0) -> bool:
        """매도 요청을 백엔드 API로 전송"""
        try:
            if not self.running:
                await self.start()
            
            # 인증 상태 확인
            if not self.auth_client or not self.auth_client.is_authenticated:
                logger.error("인증되지 않았습니다. 매도 요청을 보낼 수 없습니다.")
                return False
            
            # 백엔드 API 요청 헤더
            headers = self.auth_client.get_authorization_header()
            
            # 요청 페이로드 구성
            payload = {
                "stockCode": symbol,
                "quantity": quantity,
                "price": "",  # 시장가로 설정
                "marketOrder": True
            }
            
            # 백엔드 API 엔드포인트
            endpoint = f"{self.backend_url}/api/stock/trading/sell"
            
            # 매도 요청 전송
            async with self.session.post(
                endpoint,
                json=payload,
                headers=headers
            ) as response:
                if response.status == 200 or response.status == 201:
                    result = await response.json()
                    logger.info(f"매도 요청 성공: {symbol} {quantity}주")

                    return True
                else:
                    error_text = await response.text()
                    logger.error(f"매도 요청 실패: {symbol} - HTTP {response.status}, {error_text}")
                    return False
        
        except Exception as e:
            logger.error(f"매도 요청 처리 중 오류: {str(e)}")
            return False
        
    async def stop(self):
        """백엔드 클라이언트 정지"""
        logger.info("백엔드 클라이언트 정지")
        
        # 실행 중인 세션이 있으면 닫기
        if hasattr(self, 'session') and self.session and not self.session.closed:
            await self.session.close()
            logger.info("백엔드 클라이언트 세션 닫힘")