import logging
import asyncio
from typing import Dict, List, Callable, Optional, Any
from datetime import datetime
import aiohttp
import json

from app.config import settings
from app.auth_client import AuthClient
from app.stock_cache import StockCache
from app.kiwoom_auth import KiwoomAuthClient

logger = logging.getLogger(__name__)

class KiwoomAPI:
    """REST API와 WebSocket을 사용하는 키움 API 클래스"""
    
    def __init__(self, auth_client: AuthClient):
        """API 초기화"""
        # REST API 설정
        self.base_url = settings.API_BASE_URL
        self.websocket_url = settings.WEBSOCKET_API_URL
        
        # 인증 클라이언트
        self.auth_client = auth_client

        # 키움 API 토큰 클라이언트 추가
        self.kiwoom_auth_client = KiwoomAuthClient()
        
        # HTTP 세션
        self.session = None
        
        # 연결 상태
        self.connected = False
        
        # 계좌 정보
        self.account_number = ""
        
        # 종목 캐시
        self.stock_cache = StockCache()
        
        # 실시간 데이터 콜백 함수
        self.real_data_callback = None
        
        # 웹소켓 연결 (실시간 데이터용)
        self.websocket = None
        
        # 매매 관련 정보
        self.account_info = {
            "cash_balance": 0,
            "positions": {},
            "total_asset_value": 0
        }
        
        # 웹소켓 구독 관리 태스크
        self._subscription_task = None
        
        # 구독 그룹 관리
        self._subscription_groups = []
        self._current_group_index = 0
        
    async def initialize_stock_list(self):
        """종목 정보 초기화 - 백엔드에서 모든 종목 정보 가져오기"""
        try:
            logger.info("백엔드에서 종목 정보 요청")
            
            if not self.session:
                self.session = aiohttp.ClientSession()
            
            # 백엔드 API에서 모든 종목 정보 요청
            headers = self.auth_client.get_authorization_header()
            
            async with self.session.get(
                f"{settings.BACKEND_API_URL}/api/stocks/all",
                headers=headers
            ) as response:
                if response.status == 200:
                    stock_list = await response.json()
                    
                    # 종목 캐시 초기화
                    success = self.stock_cache.init_stock_info(stock_list)
                    
                    if success:
                        logger.info(f"종목 정보 초기화 완료: {len(stock_list)}개 종목")
                        return True
                else:
                    logger.error(f"종목 정보 요청 실패: HTTP {response.status}")
                    return False
        
        except Exception as e:
            logger.error(f"종목 정보 초기화 중 오류: {str(e)}")
            return False
    
    async def connect(self) -> bool:
        """API 연결"""
        try:
            # HTTP 세션 생성
            if not self.session:
                self.session = aiohttp.ClientSession(
                    headers={
                        "Content-Type": "application/json",
                        "Accept": "application/json"
                    }
                )
            
            # 인증 상태 확인
            if not self.auth_client.is_authenticated:
                logger.error("인증되지 않았습니다. 먼저 로그인이 필요합니다.")
                return False
            
            # 키움 API 토큰 발급 (최초 1회)
            kiwoom_token = await self.kiwoom_auth_client.get_access_token()
            if not kiwoom_token:
                logger.error("키움 API 토큰 발급 실패")
                return False

            # 백엔드 API에서 계좌 정보 요청
            await self.request_account_info()
            
            # 종목 정보 초기화 (이미 수행되었을 수 있음)
            if not self.stock_cache.stock_info_cache:
                await self.initialize_stock_list()
            
            # 연결 상태 설정
            self.connected = True
            logger.info("REST API 연결 완료")
            return True
        
        except Exception as e:
            logger.error(f"API 연결 중 오류: {str(e)}")
            return False
    
    async def _setup_websocket_connection(self):
        """웹소켓 연결 및 로그인 설정"""
        try:
            # 웹소켓 연결 시도
            await self._start_websocket()
            
            # 웹소켓 로그인 시도
            login_success = await self._websocket_login()
            if not login_success:
                logger.warning("웹소켓 로그인 실패")
                return False
            
            logger.info("웹소켓 연결 및 로그인 성공")
            return True
        except Exception as e:
            logger.error(f"웹소켓 연결 설정 중 오류: {str(e)}")
            return False

    async def _start_websocket(self):
        """웹소켓 연결 시작"""
        try:
            # 웹소켓 URL 
            ws_url = f"{self.websocket_url}/api/dostk/websocket"
            logger.info(f"웹소켓 연결 시도: {ws_url}")
            
            # 웹소켓 연결
            self.websocket = await self.session.ws_connect(ws_url)
            logger.info("웹소켓 연결 성공")
            
            return True
        except Exception as e:
            logger.error(f"웹소켓 연결 중 오류: {str(e)}")
            self.websocket = None
            return False

    async def _websocket_login(self):
        """웹소켓 로그인 처리"""
        if not self.websocket or self.websocket.closed:
            logger.error("웹소켓 연결이 없어 로그인할 수 없습니다.")
            return False
        
        try:
            # 로그인 메시지 생성 - 액세스 토큰 사용
            login_data = {
                "trnm": "LOGIN",
                "token": self.auth_client.access_token
            }
            
            # 로그인 요청 전송
            await self.websocket.send_json(login_data)
            logger.info("웹소켓 로그인 요청 전송")
            
            # 로그인 응답 대기
            msg = await self.websocket.receive()
            if msg.type == aiohttp.WSMsgType.TEXT:
                data = json.loads(msg.data)
                if data.get("trnm") == "LOGIN" and data.get("return_code") == 0:
                    logger.info("웹소켓 로그인 성공")
                    return True
                else:
                    logger.error(f"웹소켓 로그인 실패: {data}")
                    return False
            
            logger.error("웹소켓 로그인 응답을 받지 못했습니다.")
            return False
        
        except Exception as e:
            logger.error(f"웹소켓 로그인 중 오류: {str(e)}")
            return False
    
    async def prepare_subscription_groups(self, all_symbols: List[str], group_size: int = 30):
        """구독 그룹 준비"""
        self._subscription_groups = []
        
        # group_size씩 그룹화
        for i in range(0, len(all_symbols), group_size):
            self._subscription_groups.append(all_symbols[i:i+group_size])
        
        logger.info(f"{len(self._subscription_groups)}개 구독 그룹 생성 (그룹당 최대 {group_size}개 종목)")
        self._current_group_index = 0
    
    async def start_rotating_subscriptions(self, callback: Callable = None):
        """3초마다 다른 그룹을 구독하는 로테이션 시작"""
        # 콜백 함수 설정
        if callback:
            self.real_data_callback = callback
        
        # 구독 태스크 시작
        if not self._subscription_task or self._subscription_task.done():
            self._subscription_task = asyncio.create_task(self._subscription_rotation_loop())
    
    async def _subscription_rotation_loop(self):
        """구독 로테이션 루프"""
        try:
            # 웹소켓 연결 및 로그인
            ws_connected = await self._setup_websocket_connection()
            if not ws_connected:
                logger.error("웹소켓 연결 실패: 실시간 데이터 구독을 할 수 없습니다.")
                return
            
            while self.connected:
                try:
                    # 현재 구독 중인 그룹 해제
                    current_group = self._subscription_groups[self._current_group_index]
                    await self._unregister_realtime_data(list(self.stock_cache.subscribed_symbols))
                    self.stock_cache.clear_subscribed_symbols()
                    
                    # 다음 그룹으로 이동
                    self._current_group_index = (self._current_group_index + 1) % len(self._subscription_groups)
                    next_group = self._subscription_groups[self._current_group_index]
                    
                    # 새 그룹 구독
                    await self._register_realtime_data(next_group)
                    for code in next_group:
                        self.stock_cache.add_subscribed_symbol(code)
                    
                    # 3초 대기
                    await asyncio.sleep(3)
                
                except Exception as e:
                    logger.error(f"구독 로테이션 중 오류: {str(e)}")
                    await asyncio.sleep(3)  # 오류 발생 시 3초 대기 후 재시도
        
        except asyncio.CancelledError:
            logger.info("구독 로테이션 루프가 취소되었습니다.")
        
        except Exception as e:
            logger.error(f"구독 로테이션 루프 오류: {str(e)}")
        
        finally:
            # 모든 구독 해제
            if self.websocket and not self.websocket.closed:
                await self._unregister_realtime_data(list(self.stock_cache.subscribed_symbols))
            self.stock_cache.clear_subscribed_symbols()

    async def _register_realtime_data(self, codes: List[str]):
        """실시간 데이터 구독 등록"""
        if not self.websocket or self.websocket.closed:
            logger.error("웹소켓 연결이 없습니다.")
            return False
        
        try:
            # 실시간 데이터 구독 요청 메시지 생성
            subscribe_data = {
                "trnm": "REG",
                "data": {
                    "item": codes,
                    "type": ["0B"]  # 현재가
                }
            }
            
            # 요청 전송
            await self.websocket.send_json(subscribe_data)
            logger.info(f"{len(codes)}개 종목 실시간 시세 구독 요청 전송")
            
            return True
        except Exception as e:
            logger.error(f"실시간 데이터 구독 요청 중 오류: {str(e)}")
            return False

    async def _unregister_realtime_data(self, codes: List[str] = None):
        """실시간 데이터 구독 해지"""
        if not self.websocket or self.websocket.closed:
            return False
        
        try:
            # 해지할 종목 코드가 없으면 전체 해지
            items = codes if codes else list(self.stock_cache.subscribed_symbols)
            
            if not items:
                return True
            
            # 실시간 데이터 구독 해지 요청 메시지 생성
            unsubscribe_data = {
                "trnm": "REMOVE",
                "data": {
                    "item": items,
                    "type": ["0B"]  # 현재가
                }
            }
            
            # 요청 전송
            await self.websocket.send_json(unsubscribe_data)
            logger.info(f"{len(items)}개 종목 실시간 시세 구독 해지 요청 전송")
            
            return True
        except Exception as e:
            logger.error(f"실시간 데이터 구독 해지 요청 중 오류: {str(e)}")
            return False
    
    async def handle_websocket_message(self):
        """웹소켓 메시지 처리 루프"""
        try:
            while self.connected and self.websocket and not self.websocket.closed:
                msg = await self.websocket.receive()
                
                if msg.type == aiohttp.WSMsgType.TEXT:
                    try:
                        data = json.loads(msg.data)
                        
                        # 실시간 시세 데이터 처리
                        if data.get("trnm") == "PUSH" and data.get("type") == "0B":
                            code = data.get("item")
                            price = int(data.get("price", 0))
                            
                            if code and price > 0:
                                # 캐시 업데이트
                                self.stock_cache.update_price(code, price)
                                
                                # 콜백 함수 호출
                                if self.real_data_callback:
                                    stock_name = self.stock_cache.get_stock_name(code)
                                    self.real_data_callback(code, price, stock_name)
                    
                    except json.JSONDecodeError:
                        logger.error(f"JSON 파싱 오류: {msg.data}")
                    except Exception as e:
                        logger.error(f"웹소켓 메시지 처리 중 오류: {str(e)}")
                
                elif msg.type == aiohttp.WSMsgType.CLOSED:
                    logger.warning("웹소켓 연결이 종료되었습니다.")
                    break
                
                elif msg.type == aiohttp.WSMsgType.ERROR:
                    logger.error(f"웹소켓 오류: {msg.data}")
                    break
        
        except asyncio.CancelledError:
            logger.info("웹소켓 메시지 처리 루프가 취소되었습니다.")
        
        except Exception as e:
            logger.error(f"웹소켓 메시지 처리 루프 오류: {str(e)}")
        
        finally:
            # 연결 종료 시 재연결 시도
            if self.connected:
                logger.info("웹소켓 메시지 처리 루프 종료, 5초 후 재연결 시도")
                await asyncio.sleep(5)
                asyncio.create_task(self._setup_websocket_connection())
    
    async def get_daily_chart_data(self, code, from_date=None, to_date=None, period=None):
        """일별 차트 데이터 조회 - Envelope 지표 계산용"""
        try:
            logger.info(f"일봉 데이터 요청: {code}")
            
            # API 요청 헤더 구성
            headers = self.auth_client.get_authorization_header()
            
            # API 요청 파라미터 구성
            params = {
                "code": code,
                "timeframe": "day"  # 일봉 데이터
            }
            
            # 기간 설정
            if from_date:
                params["from_date"] = from_date
            if to_date:
                params["to_date"] = to_date
            if period:
                params["period"] = period
            
            # API 요청
            async with self.session.get(
                f"{self.base_url}/market/charts", 
                params=params,
                headers=headers
            ) as response:
                if response.status == 200:
                    chart_data = await response.json()
                    
                    # 데이터 변환 및 반환
                    result = []
                    for item in chart_data.get("data", []):
                        result.append({
                            "date": item.get("date"),
                            "open": float(item.get("open", 0)),
                            "high": float(item.get("high", 0)),
                            "low": float(item.get("low", 0)),
                            "close": float(item.get("close", 0)),
                            "volume": int(item.get("volume", 0))
                        })
                    
                    logger.info(f"일봉 데이터 조회 성공: {code}, {len(result)}개 데이터")
                    return result
                else:
                    logger.error(f"차트 데이터 요청 실패: {code} HTTP {response.status}")
                    return []
        
        except Exception as e:
            logger.error(f"차트 데이터 요청 중 오류: {code} - {str(e)}")
            return []
    
    async def request_account_info(self):
        """백엔드 API에서 계좌 정보 요청"""
        try:
            # 백엔드 API 요청 헤더
            headers = self.auth_client.get_authorization_header()
            
            # 백엔드 API에서 포트폴리오 정보 요청
            async with self.session.get(
                f"{settings.BACKEND_API_URL}/api/members/portfolio",
                headers=headers
            ) as response:
                if response.status == 200:
                    portfolio_data = await response.json()
                    
                    # 계좌 정보 업데이트
                    self.account_info = {
                        "cash_balance": portfolio_data.get("memberMoney", 0),
                        "total_asset_value": portfolio_data.get("totalAsset", 0),
                        "positions": {}
                    }
                    
                    # 보유 종목 정보 업데이트
                    for position in portfolio_data.get("positions", []):
                        code = position.get("stockCode")
                        if code:
                            self.account_info["positions"][code] = {
                                "code": code,
                                "name": position.get("stockName", ""),
                                "quantity": position.get("quantity", 0),
                                "purchase_price": position.get("averagePrice", 0),
                                "current_price": position.get("currentPrice", 0),
                                "eval_profit_loss": position.get("profitLoss", 0),
                                "earning_rate": position.get("returnRate", 0.0)
                            }
                    
                    logger.info(f"계좌정보 업데이트: 예수금={self.account_info['cash_balance']}, 종목수={len(self.account_info['positions'])}")
                    return True
                else:
                    logger.error(f"계좌 정보 조회 실패: HTTP {response.status}")
                    return False
        
        except Exception as e:
            logger.error(f"계좌 정보 요청 중 오류: {str(e)}")
            return False

    async def execute_trade_decision(self, decision: Dict[str, Any]) -> Dict[str, Any]:
        """거래 결정을 실행하고 결과 반환"""
        try:
            symbol = decision.get("symbol")
            action = decision.get("action")  # "buy" 또는 "sell"
            quantity = decision.get("quantity", 0)
            
            if not symbol or not action or quantity <= 0:
                logger.error(f"거래 결정에 필요한 정보가 부족합니다: {decision}")
                return {
                    "success": False,
                    "message": "거래 결정에 필요한 정보가 부족합니다",
                    "executed_quantity": 0,
                    "executed_price": 0
                }
            
            # 인증 상태 확인
            if not self.auth_client.is_authenticated:
                logger.error("인증되지 않았습니다. 거래를 실행할 수 없습니다.")
                return {
                    "success": False,
                    "message": "인증되지 않았습니다. 거래를 실행할 수 없습니다.",
                    "executed_quantity": 0,
                    "executed_price": 0
                }
            
            # 거래 전 현재가 확인
            current_price = self.stock_cache.get_price(symbol)
            
            # API 요청 엔드포인트 및 페이로드 설정
            if action.lower() == "buy":
                endpoint = f"{settings.BACKEND_API_URL}/api/stock/trading/buy"
                payload = {
                    "stockCode": symbol,
                    "quantity": quantity,
                    "price": "",  # 시장가로 설정
                    "marketOrder": True
                }
            elif action.lower() == "sell":
                endpoint = f"{settings.BACKEND_API_URL}/api/stock/trading/sell"
                payload = {
                    "stockCode": symbol,
                    "quantity": quantity,
                    "price": "",  # 시장가로 설정
                    "marketOrder": True
                }
            else:
                logger.error(f"알 수 없는 거래 유형: {action}")
                return {
                    "success": False,
                    "message": f"알 수 없는 거래 유형: {action}",
                    "executed_quantity": 0,
                    "executed_price": 0
                }
            
            # 백엔드 API를 통한 주문 전송
            headers = self.auth_client.get_authorization_header()
            
            async with self.session.post(
                endpoint,
                json=payload,
                headers=headers
            ) as response:
                if response.status == 200 or response.status == 201:
                    # 주문 성공
                    result = await response.json()
                    
                    logger.info(f"주문 성공: {symbol} {action} {quantity}주")
                    
                    return {
                        "success": True,
                        "tradeHistoryId": result.get("tradeHistoryId"),
                        "orderId": result.get("orderId"),
                        "message": "주문이 성공적으로 접수되었습니다",
                        "executed_quantity": result.get("quantity"),
                        "executed_price": result.get("unitPrice"),
                        "symbol": result.get("stockCode"),
                        "stockName": result.get("stockName"),
                        "tradeType": result.get("tradeType"),
                        "totalPrice": result.get("totalPrice"),
                        "tradedAt": result.get("tradedAt"),
                        "status": result.get("status")
                    }
                else:
                    # 주문 실패
                    error_text = await response.text()
                    error_message = f"주문 처리 중 오류가 발생했습니다: HTTP {response.status}, {error_text}"
                    logger.error(f"주문 실패: {error_message}")
                    
                    return {
                        "success": False,
                        "message": error_message,
                        "executed_quantity": 0,
                        "executed_price": 0
                    }
                
        except Exception as e:
            logger.error(f"주문 처리 중 예외 발생: {str(e)}")
            return {
                "success": False,
                "message": f"주문 처리 중 예외 발생: {str(e)}",
                "executed_quantity": 0,
                "executed_price": 0
            }
    
    async def close(self):
        """API 연결 종료"""
        try:
            # 구독 관리 태스크 취소
            if self._subscription_task and not self._subscription_task.done():
                self._subscription_task.cancel()
            
            # 웹소켓 연결 종료
            if self.websocket and not self.websocket.closed:
                await self.websocket.close()
            
            # HTTP 세션 종료
            if self.session and not self.session.closed:
                await self.session.close()
            
            self.connected = False
            logger.info("REST API 연결 종료")
        except Exception as e:
            logger.error(f"API 연결 종료 중 오류: {str(e)}")