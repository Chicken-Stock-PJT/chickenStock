import logging
import asyncio
from typing import Dict, List, Set, Callable, Optional, Any
from datetime import datetime
import aiohttp
import json

from .models import TradeDecision
from .config import settings

logger = logging.getLogger(__name__)

class KiwoomAPI:
    """REST API를 사용하는 키움 API 클래스"""
    
    def __init__(self):
        """REST API 연결 및 초기화"""
        # REST API 설정
        self.base_url = settings.API_BASE_URL
        self.api_key = settings.KIWOOM_API_KEY
        self.api_secret = settings.KIWOOM_API_SECRET
        self.token = None
        
        # HTTP 세션
        self.session = None
        
        # 로그인 상태
        self.connected = False
        
        # 계좌 정보
        self.account_number = ""
        self.account_count = 0
        self.account_list = []
        
        # 종목 관련 데이터
        self.kospi_symbols = []
        self.kosdaq_symbols = []
        self.realtime_prices = {}
        self.subscribed_symbols = set()
        
        # 실시간 시세 콜백 함수
        self.real_data_callback = None
        
        # 웹소켓 연결 (실시간 데이터용)
        self.websocket = None
        
        # 매매 관련 정보
        self.account_info = {
            "cash_balance": 0,
            "positions": {},
            "total_asset_value": 0
        }
    
    async def connect(self) -> bool:
        """API 연결 및 로그인"""
        try:
            # HTTP 세션 생성
            self.session = aiohttp.ClientSession(
                headers={
                    "Content-Type": "application/json",
                    "Accept": "application/json"
                }
            )
            # 토큰이 이미 설정되어 있는지 확인
            if not self.token:
                logger.error("접근 토큰이 설정되지 않았습니다.")
                return False
                
            # 세션 헤더에 토큰 추가
            self.session.headers.update({
                "Authorization": f"Bearer {self.token}"
            })

            # 간단한 연결 테스트 요청 (예: 간단한 핑 또는 상태 확인)
            async with self.session.get(f"{self.base_url}/status") as response:
                
                if response.status == 200:
                    # 인증 성공
                    self.connected = True
                    
                    # 백엔드 API에서 계좌 정보 요청
                    await self.request_account_info()
                    
                    # 시장 데이터 초기화
                    await self._get_stock_list()
                    
                    logger.info("REST API 연결 완료")
                    return True
                else:
                    logger.error(f"API 연결 테스트 실패: HTTP {response.status}")
                    return False
        
        except Exception as e:
            logger.error(f"API 연결 중 오류: {str(e)}")
            return False
    
    async def _get_account_info(self):
        """계좌 정보 가져오기"""
        try:
            async with self.session.get(f"{self.base_url}/user/accounts") as response:
                if response.status == 200:
                    accounts = await response.json()
                    self.account_list = [account["accountNumber"] for account in accounts]
                    self.account_count = len(self.account_list)
                    
                    if self.account_list:
                        self.account_number = self.account_list[0]  # 첫 번째 계좌 선택
                        logger.info(f"계좌번호: {self.account_number}")
                else:
                    logger.error(f"계좌 정보 조회 실패: HTTP {response.status}")
        except Exception as e:
            logger.error(f"계좌 정보 조회 중 오류: {str(e)}")
    
    async def _init_market_data(self):
        """장 시작 시 필요한 데이터 초기화"""
        try:
            # 코스피, 코스닥 종목 리스트 가져오기
            await self._get_stock_list()
            
        except Exception as e:
            logger.error(f"시장 데이터 초기화 중 오류: {str(e)}")
    
    async def _get_stock_list(self):
        """종목 리스트 가져오기"""
        try:
            # 코스피 종목 리스트 요청
            headers = {
                "authorization": f"Bearer {self.access_token}",
                "api-id": "ka10099"
            }
            
            # 코스피 종목 요청
            kospi_body = {
                "mrkt_tp": "0"  # 코스피
            }
            
            kospi_stocks = []
            cont_yn = "Y"
            next_key = ""
            
            # 연속 조회로 모든 코스피 종목 가져오기
            while cont_yn == "Y":
                request_headers = headers.copy()
                if next_key:
                    request_headers["cont-yn"] = "Y"
                    request_headers["next-key"] = next_key
                    
                async with self.session.post(
                    f"{self.base_url}/api/dostk/stkinfo", 
                    headers=request_headers, 
                    json=kospi_body
                ) as response:
                    if response.status == 200:
                        resp_data = await response.json()
                        
                        # 연속 조회 정보 업데이트
                        cont_yn = response.headers.get("cont-yn", "N")
                        next_key = response.headers.get("next-key", "")
                        
                        # 종목 정보 추가
                        stock_list = resp_data.get("list", [])
                        for stock in stock_list:
                            if "code" in stock and "name" in stock and "lastPrice" in stock and "listCount" in stock:
                                try:
                                    # 전일종가와 상장주식수 숫자로 변환
                                    last_price = int(stock["lastPrice"])
                                    list_count = int(stock["listCount"])
                                    # 시가총액 계산 (전일종가 * 상장주식수)
                                    market_cap = last_price * list_count
                                    
                                    kospi_stocks.append({
                                        "code": stock["code"],
                                        "name": stock["name"],
                                        "market_cap": market_cap
                                    })
                                except (ValueError, TypeError):
                                    logger.warning(f"코스피 종목 데이터 변환 오류: {stock}")
                    else:
                        logger.error(f"코스피 종목 조회 실패: HTTP {response.status}")
                        break
            
            # 코스닥 종목 요청
            kosdaq_body = {
                "mrkt_tp": "10"  # 코스닥
            }
            
            kosdaq_stocks = []
            cont_yn = "Y"
            next_key = ""
            
            # 연속 조회로 모든 코스닥 종목 가져오기
            while cont_yn == "Y":
                request_headers = headers.copy()
                if next_key:
                    request_headers["cont-yn"] = "Y"
                    request_headers["next-key"] = next_key
                    
                async with self.session.post(
                    f"{self.base_url}/api/dostk/stkinfo", 
                    headers=request_headers, 
                    json=kosdaq_body
                ) as response:
                    if response.status == 200:
                        resp_data = await response.json()
                        
                        # 연속 조회 정보 업데이트
                        cont_yn = response.headers.get("cont-yn", "N")
                        next_key = response.headers.get("next-key", "")
                        
                        # 종목 정보 추가
                        stock_list = resp_data.get("list", [])
                        for stock in stock_list:
                            if "code" in stock and "name" in stock and "lastPrice" in stock and "listCount" in stock:
                                try:
                                    # 전일종가와 상장주식수 숫자로 변환
                                    last_price = int(stock["lastPrice"])
                                    list_count = int(stock["listCount"])
                                    # 시가총액 계산 (전일종가 * 상장주식수)
                                    market_cap = last_price * list_count
                                    
                                    kosdaq_stocks.append({
                                        "code": stock["code"],
                                        "name": stock["name"],
                                        "market_cap": market_cap
                                    })
                                except (ValueError, TypeError):
                                    logger.warning(f"코스닥 종목 데이터 변환 오류: {stock}")
                    else:
                        logger.error(f"코스닥 종목 조회 실패: HTTP {response.status}")
                        break
            
            # 시가총액 기준 정렬 (내림차순)
            kospi_stocks.sort(key=lambda x: x["market_cap"], reverse=True)
            kosdaq_stocks.sort(key=lambda x: x["market_cap"], reverse=True)
            
            # 상위 종목만 선택
            kospi_top = kospi_stocks[:450] if len(kospi_stocks) > 450 else kospi_stocks
            kosdaq_top = kosdaq_stocks[:150] if len(kosdaq_stocks) > 150 else kosdaq_stocks
            
            # 종목 코드만 추출하여 저장
            self.kospi_symbols = [stock["code"] for stock in kospi_top]
            self.kosdaq_symbols = [stock["code"] for stock in kosdaq_top]
            
            # 디버그 모드일 경우 종목 수 제한
            if settings.DEBUG_MODE:
                self.kospi_symbols = self.kospi_symbols[:30]
                self.kosdaq_symbols = self.kosdaq_symbols[:10]
            
            logger.info(f"코스피 종목 수: {len(self.kospi_symbols)}, 코스닥 종목 수: {len(self.kosdaq_symbols)}")
            
        except Exception as e:
            logger.error(f"종목 리스트 조회 중 오류: {str(e)}")
            # 오류 발생 시 기본 필터링 설정
            if settings.DEBUG_MODE:
                self.kospi_symbols = self.kospi_symbols[:30] if self.kospi_symbols else []
                self.kosdaq_symbols = self.kosdaq_symbols[:10] if self.kosdaq_symbols else []
    
    async def get_stock_name(self, code):
        """종목 코드로 종목명 조회"""
        try:
            async with self.session.get(f"{self.base_url}/market/stocks/{code}") as response:
                if response.status == 200:
                    stock_info = await response.json()
                    return stock_info.get("name", "")
                else:
                    logger.error(f"종목 정보 조회 실패: HTTP {response.status}")
                    return ""
        except Exception as e:
            logger.error(f"종목명 조회 중 오류: {str(e)}")
            return ""
    
    async def subscribe_realtime_data(self, codes: List[str], callback: Callable = None):
        """실시간 데이터 구독 및 시세 캐시 업데이트"""
        # 이미 구독 중인 종목은 제외
        new_codes = [code for code in codes if code not in self.subscribed_symbols]
        
        # 콜백 함수 설정
        if callback:
            self.real_data_callback = callback
        
        # 이미 구독 중인 종목이면 시세 정보만 갱신
        existing_codes = [code for code in codes if code in self.subscribed_symbols]
        for code in existing_codes:
            try:
                # 현재가 조회
                price = await self._get_current_price(code)
                
                if price > 0:
                    # 시세 데이터 캐시 업데이트
                    if code in self.realtime_prices:
                        self.realtime_prices[code]["price"] = price
                        self.realtime_prices[code]["timestamp"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                    else:
                        stock_name = await self.get_stock_name(code)
                        self.realtime_prices[code] = {
                            "symbol": code,
                            "name": stock_name,
                            "price": price,
                            "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                        }
                    
                    # 콜백 함수 호출
                    if self.real_data_callback:
                        self.real_data_callback(code, price)
            except Exception as e:
                logger.error(f"기존 종목({code}) 시세 갱신 중 오류: {str(e)}")
        
        if not new_codes:
            return
        
        try:
            # 실시간 데이터 구독
            if not self.websocket:
                # 웹소켓 연결 시작
                await self._start_websocket()
            
            # 신규 종목 구독
            subscribe_data = {
                "action": "subscribe",
                "codes": new_codes
            }
            
            await self.websocket.send_json(subscribe_data)
            
            # 구독 종목 추가
            self.subscribed_symbols.update(new_codes)
            logger.info(f"{len(new_codes)}개 종목 실시간 시세 구독 추가, 총 {len(self.subscribed_symbols)}개 구독 중")
            
            # 신규 구독 종목의 초기 시세 가져오기
            for code in new_codes:
                try:
                    # 현재가 조회
                    price = await self._get_current_price(code)
                    
                    if price > 0:
                        # 시세 데이터 캐시 초기화
                        stock_name = await self.get_stock_name(code)
                        self.realtime_prices[code] = {
                            "symbol": code,
                            "name": stock_name,
                            "price": price,
                            "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                        }
                        
                        # 콜백 함수 호출
                        if self.real_data_callback:
                            self.real_data_callback(code, price)
                except Exception as e:
                    logger.error(f"신규 종목({code}) 시세 초기화 중 오류: {str(e)}")
        
        except Exception as e:
            logger.error(f"실시간 시세 구독 중 오류: {str(e)}")
    
    async def _start_websocket(self):
        """웹소켓 연결 시작"""
        try:
            ws_url = f"{self.base_url.replace('http', 'ws')}/ws/quotes?token={self.token}"
            self.websocket = await self.session.ws_connect(ws_url)
            
            # 웹소켓 메시지 수신 태스크 시작
            asyncio.create_task(self._websocket_listener())
            
            logger.info("웹소켓 연결 성공")
            return True
        except Exception as e:
            logger.error(f"웹소켓 연결 중 오류: {str(e)}")
            return False
    
    async def _websocket_listener(self):
        """웹소켓 메시지 수신 처리"""
        try:
            async for msg in self.websocket:
                if msg.type == aiohttp.WSMsgType.TEXT:
                    data = json.loads(msg.data)
                    
                    # 시세 데이터 처리
                    if data.get("type") == "QUOTE":
                        code = data.get("code")
                        price = data.get("price")
                        
                        if code and price:
                            # 실시간 가격 데이터 업데이트
                            if code in self.realtime_prices:
                                self.realtime_prices[code]["price"] = price
                                self.realtime_prices[code]["timestamp"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                            else:
                                stock_name = await self.get_stock_name(code)
                                self.realtime_prices[code] = {
                                    "symbol": code,
                                    "name": stock_name,
                                    "price": price,
                                    "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                                }
                            
                            # 콜백 함수 호출
                            if self.real_data_callback:
                                self.real_data_callback(code, price)
                
                elif msg.type == aiohttp.WSMsgType.ERROR:
                    logger.error(f"웹소켓 오류: {msg.data}")
                    break
                
                elif msg.type == aiohttp.WSMsgType.CLOSED:
                    logger.warning("웹소켓 연결 종료")
                    break
        
        except Exception as e:
            logger.error(f"웹소켓 리스너 오류: {str(e)}")
        finally:
            # 연결이 끊어진 경우 재연결 시도
            if not self.websocket.closed:
                await self.websocket.close()
            self.websocket = None
            
            # 3초 후 재연결 시도
            await asyncio.sleep(3)
            if self.connected:
                asyncio.create_task(self._start_websocket())
    
    async def _get_current_price(self, code: str) -> int:
        """종목 현재가 조회"""
        try:
            async with self.session.get(f"{self.base_url}/market/stocks/{code}/price") as response:
                if response.status == 200:
                    price_data = await response.json()
                    return abs(int(price_data.get("price", 0)))
                else:
                    logger.error(f"현재가 조회 실패: HTTP {response.status}")
                    return 0
        except Exception as e:
            logger.error(f"현재가 조회 중 오류: {str(e)}")
            return 0
    
    async def unsubscribe_realtime_data(self, codes: List[str] = None):
        """실시간 데이터 구독 해제"""
        try:
            if not self.websocket or self.websocket.closed:
                return
            
            if codes is None:
                # 전체 구독 해제
                unsubscribe_data = {
                    "action": "unsubscribe",
                    "codes": list(self.subscribed_symbols)
                }
                
                await self.websocket.send_json(unsubscribe_data)
                self.subscribed_symbols.clear()
                logger.info("모든 종목 실시간 시세 구독 해제")
            else:
                # 특정 종목만 구독 해제
                codes_to_unsubscribe = [code for code in codes if code in self.subscribed_symbols]
                
                if codes_to_unsubscribe:
                    unsubscribe_data = {
                        "action": "unsubscribe",
                        "codes": codes_to_unsubscribe
                    }
                    
                    await self.websocket.send_json(unsubscribe_data)
                    
                    for code in codes_to_unsubscribe:
                        self.subscribed_symbols.remove(code)
                    
                    logger.info(f"{len(codes_to_unsubscribe)}개 종목 실시간 시세 구독 해제")
        
        except Exception as e:
            logger.error(f"실시간 시세 구독 해제 중 오류: {str(e)}")
    
    async def request_account_info(self):
        """백엔드 API에서 계좌 정보 요청"""
        try:
            # 백엔드 API에서 포트폴리오 정보 요청
            async with self.session.get(f"{settings.BACKEND_API_URL}/api/members/portfolio") as response:
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
    
    async def buy_stock(self, code, quantity, price):
        """주식 매수"""
        try:
            # 주문 데이터 구성
            order_data = {
                "accountNumber": self.account_number,
                "code": code,
                "orderType": "BUY",
                "quantity": quantity,
                "price": price,
                "priceType": "LIMIT" if price > 0 else "MARKET"
            }
            
            # 주문 요청
            async with self.session.post(f"{self.base_url}/trading/order", json=order_data) as response:
                if response.status == 200:
                    result = await response.json()
                    order_id = result.get("orderId")
                    logger.info(f"매수 주문 전송 성공: {code} {quantity}주 {price}원, 주문번호: {order_id}")
                    return True
                else:
                    error_data = await response.json()
                    logger.error(f"매수 주문 전송 실패: {code} - {error_data.get('message', '알 수 없는 오류')}")
                    return False
        
        except Exception as e:
            logger.error(f"매수 주문 중 오류: {str(e)}")
            return False
    
    async def sell_stock(self, code, quantity, price):
        """주식 매도"""
        try:
            # 주문 데이터 구성
            order_data = {
                "accountNumber": self.account_number,
                "code": code,
                "orderType": "SELL",
                "quantity": quantity,
                "price": price,
                "priceType": "LIMIT" if price > 0 else "MARKET"
            }
            
            # 주문 요청
            async with self.session.post(f"{self.base_url}/trading/order", json=order_data) as response:
                if response.status == 200:
                    result = await response.json()
                    order_id = result.get("orderId")
                    logger.info(f"매도 주문 전송 성공: {code} {quantity}주 {price}원, 주문번호: {order_id}")
                    return True
                else:
                    error_data = await response.json()
                    logger.error(f"매도 주문 전송 실패: {code} - {error_data.get('message', '알 수 없는 오류')}")
                    return False
        
        except Exception as e:
            logger.error(f"매도 주문 중 오류: {str(e)}")
            return False
    
    async def execute_trade_decision(self, decision: Dict[str, Any]) -> Dict[str, Any]:
        """거래 결정을 실행하고 결과 반환"""
        try:
            symbol = decision.get("symbol")
            action = decision.get("action")  # "buy" 또는 "sell"
            quantity = decision.get("quantity", 0)
            price_type = decision.get("price_type", "market")  # "market", "limit" 등
            price = decision.get("price", 0)  # 지정가일 경우 사용
            
            if not symbol or not action or quantity <= 0:
                logger.error(f"거래 결정에 필요한 정보가 부족합니다: {decision}")
                return {
                    "success": False,
                    "message": "거래 결정에 필요한 정보가 부족합니다",
                    "executed_quantity": 0,
                    "executed_price": 0
                }
            
            # 토큰 유효성 확인
            if not self.token:
                logger.error("유효한 접근 토큰이 없습니다")
                return {
                    "success": False,
                    "message": "유효한 접근 토큰이 없습니다",
                    "executed_quantity": 0,
                    "executed_price": 0
                }
            
            # 거래 전 현재가 확인
            current_price = self.realtime_prices.get(symbol, {}).get("price", 0)
            if current_price <= 0 and price_type == "market":
                logger.error(f"종목 {symbol}의 현재가를 확인할 수 없습니다")
                return {
                    "success": False, 
                    "message": f"종목 {symbol}의 현재가를 확인할 수 없습니다",
                    "executed_quantity": 0,
                    "executed_price": 0
                }
            
            # 주문 데이터 생성
            order_data = {
                "account_number": self.account_number,
                "symbol": symbol,
                "order_type": action.upper(),  # "BUY" 또는 "SELL"
                "price_type": price_type.upper(),  # "MARKET" 또는 "LIMIT"
                "quantity": quantity
            }
            
            # 지정가 주문인 경우 가격 추가
            if price_type.lower() == "limit" and price > 0:
                order_data["price"] = price
            
            # REST API를 통한 주문 전송
            async with self.session.post(
                f"{self.api_base_url}/order",
                json=order_data,
                headers={"Authorization": f"Bearer {self.token}"}
            ) as response:
                result = await response.json()
                
                if response.status == 200:
                    # 주문 성공
                    logger.info(f"주문 성공: {symbol} {action} {quantity}주")
                    
                    # 실제 체결 정보 반환
                    return {
                        "success": True,
                        "message": "주문이 성공적으로 접수되었습니다",
                        "order_id": result.get("orderId", ""),
                        "executed_quantity": quantity,  # 실제로는 체결 수량을 API로부터 받아야 함
                        "executed_price": current_price if price_type.lower() == "market" else price
                    }
                else:
                    # 주문 실패
                    error_message = result.get("message", "주문 처리 중 오류가 발생했습니다")
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
    
    async def get_master_code_name(self, code):
        """종목코드로 종목명 얻기"""
        return await self.get_stock_name(code)
    
    def get_all_symbols(self):
        """모든 관심 종목 반환"""
        return self.kospi_symbols + self.kosdaq_symbols
    
    def get_kospi_symbols(self):
        """코스피 종목 반환"""
        return self.kospi_symbols
    
    def get_kosdaq_symbols(self):
        """코스닥 종목 반환"""
        return self.kosdaq_symbols
    
    def get_account_info(self):
        """계좌 정보 반환"""
        return self.account_info
    
    async def close(self):
        """API 연결 종료"""
        try:
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