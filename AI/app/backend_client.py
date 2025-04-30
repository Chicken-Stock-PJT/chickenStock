import logging
import asyncio
from typing import Dict, Any, Optional, List
import aiohttp

from app.config import settings
from app.auth_client import AuthClient

logger = logging.getLogger(__name__)

class BackendClient:
    """백엔드 API와 통신하는 클라이언트"""
    
    def __init__(self, trading_model, auth_client: AuthClient):
        """클라이언트 초기화"""
        # 트레이딩 모델 참조
        self.trading_model = trading_model
        
        # 인증 클라이언트
        self.auth_client = auth_client
        
        # 백엔드 URL
        self.backend_url = settings.BACKEND_API_URL
        
        # HTTP 세션
        self.session = None
        
        # 클라이언트 상태
        self.running = False
        
        # 거래 결과 큐
        self.pending_results = []
    
    async def start(self):
        """클라이언트 시작"""
        if self.running:
            return True
        
        # HTTP 세션 생성
        if not self.session:
            self.session = aiohttp.ClientSession()
        
        self.running = True
        
        # 큐 처리 태스크 시작
        asyncio.create_task(self._process_pending_results())
        
        logger.info("백엔드 클라이언트 시작")
        return True
    
    async def stop(self):
        """클라이언트 정지"""
        self.running = False
        
        # HTTP 세션 종료
        if self.session and not self.session.closed:
            await self.session.close()
            self.session = None
        
        logger.info("백엔드 클라이언트 정지")
        return True
    
    async def send_trade_result(self, decision: Dict[str, Any], result: Dict[str, Any]):
        """매매 결과를 백엔드로 전송"""
        if not self.running:
            await self.start()
        
        # 백엔드로 전송할 형식으로 변환
        trade_data = {
            "stockCode": decision.get("symbol", ""),
            "stockName": decision.get("name", ""),
            "orderType": decision.get("action", "").upper(),
            "quantity": result.get("executed_quantity", 0),
            "price": result.get("executed_price", 0),
            "total": result.get("executed_quantity", 0) * result.get("executed_price", 0),
            "orderId": result.get("order_id", ""),
            "success": result.get("success", False),
            "reason": decision.get("reason", "")
        }
        
        # 큐에 추가
        self.pending_results.append(trade_data)
        
        # 트레이딩 모델에 결과 전달 (포지션 추적용)
        await self.trading_model.process_trade_result(
            symbol=decision.get("symbol", ""),
            action=decision.get("action", ""),
            quantity=result.get("executed_quantity", 0),
            price=result.get("executed_price", 0)
        )
        
        logger.info(f"거래 결과 큐에 추가: {trade_data['stockCode']} {trade_data['orderType']} {trade_data['quantity']}주 @ {trade_data['price']}")
        return True
    
    async def request_account_info(self):
        """백엔드 API에서 계좌 정보 요청"""
        try:
            if not self.running:
                await self.start()
                
            # 인증 상태 확인
            if not self.auth_client.is_authenticated:
                logger.error("인증되지 않았습니다. 계좌 정보를 요청할 수 없습니다.")
                return None
                
            # 백엔드 API 요청 헤더
            headers = self.auth_client.get_authorization_header()
            
            # 백엔드 API에서 포트폴리오 정보 요청
            async with self.session.get(
                f"{self.backend_url}/api/members/portfolio",
                headers=headers
            ) as response:
                if response.status == 200:
                    portfolio_data = await response.json()
                    
                    # 계좌 정보 구성
                    account_info = {
                        "cash_balance": portfolio_data.get("memberMoney", 0),
                        "total_asset_value": portfolio_data.get("totalAsset", 0),
                        "positions": {}
                    }
                    
                    # 보유 종목 정보 업데이트
                    for position in portfolio_data.get("positions", []):
                        code = position.get("stockCode")
                        if code:
                            account_info["positions"][code] = {
                                "code": code,
                                "name": position.get("stockName", ""),
                                "quantity": position.get("quantity", 0),
                                "purchase_price": position.get("averagePrice", 0),
                                "current_price": position.get("currentPrice", 0),
                                "eval_profit_loss": position.get("profitLoss", 0),
                                "earning_rate": position.get("returnRate", 0.0)
                            }
                    
                    logger.info(f"계좌정보 업데이트: 예수금={account_info['cash_balance']}, 종목수={len(account_info['positions'])}")
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
            if not self.auth_client.is_authenticated:
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
    
    async def _process_pending_results(self):
        """대기 중인 거래 결과 처리"""
        try:
            while self.running:
                # 큐에 결과가 있으면 처리
                if self.pending_results:
                    trade_data = self.pending_results[0]
                    success = await self._send_to_backend(trade_data)
                    
                    if success:
                        # 성공 시 큐에서 제거
                        self.pending_results.pop(0)
                    else:
                        # 실패 시 일정 시간 대기 후 재시도
                        await asyncio.sleep(5)
                
                # 큐가 비어있으면 대기
                else:
                    await asyncio.sleep(1)
        
        except asyncio.CancelledError:
            logger.info("거래 결과 처리 태스크 취소됨")
        
        except Exception as e:
            logger.error(f"거래 결과 처리 중 오류: {str(e)}")
            await asyncio.sleep(5)  # 오류 발생 시 대기 후 재시도
            asyncio.create_task(self._process_pending_results())
    
    async def _send_to_backend(self, trade_data: Dict[str, Any]) -> bool:
        """거래 결과를 백엔드로 전송"""
        try:
            # 인증 상태 확인
            if not self.auth_client.is_authenticated:
                logger.warning("인증되지 않아 백엔드로 거래 결과를 전송할 수 없습니다.")
                return False
            
            # HTTP 세션 확인
            if not self.session:
                self.session = aiohttp.ClientSession()
            
            # 헤더 설정
            headers = self.auth_client.get_authorization_header()
            
            # 백엔드로 거래 결과 전송
            async with self.session.post(
                f"{self.backend_url}/api/trades",
                json=trade_data,
                headers=headers
            ) as response:
                if response.status == 200 or response.status == 201:
                    logger.info(f"거래 결과 전송 성공: {trade_data['stockCode']} {trade_data['orderType']}")
                    return True
                else:
                    error_text = await response.text()
                    logger.error(f"거래 결과 전송 실패: HTTP {response.status}, {error_text}")
                    
                    # 인증 오류인 경우 토큰 갱신 시도
                    if response.status == 401:
                        success = await self.auth_client.refresh_access_token()
                        if not success:
                            logger.error("토큰 갱신 실패")
                    
                    return False
        
        except Exception as e:
            logger.error(f"거래 결과 전송 중 오류: {str(e)}")
            return False