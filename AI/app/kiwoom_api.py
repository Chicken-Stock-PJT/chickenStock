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
from app.kiwoom_websocket import KiwoomWebSocket

logger = logging.getLogger(__name__)

class KiwoomAPI:
    """REST API와 WebSocket을 사용하는 키움 API 클래스"""
    
    def __init__(self, auth_client: AuthClient):
        """API 초기화"""
        # REST API 설정
        self.base_url = settings.API_BASE_URL
        self.websocket_url = settings.WEBSOCKET_API_URL
        
        # 인증 클라이언트 (백엔드 서버 인증용)
        self.auth_client = auth_client

        # 키움 API 토큰 클라이언트 (키움증권 API 인증용)
        self.kiwoom_auth_client = KiwoomAuthClient()
        
        # HTTP 세션
        self.session = None
        
        # 연결 상태
        self.connected = False
        
        # 계좌 정보
        self.account_number = ""
        
        # 종목 캐시
        self.stock_cache = StockCache()
        
        # 차트 데이터 캐시
        self.chart_cache = {}
        
        # 웹소켓 클라이언트 (초기화는 connect에서)
        self.websocket_client = None
        
        # 매매 관련 정보
        self.account_info = {
            "cash_balance": 0,
            "positions": {},
            "total_asset_value": 0
        }
    
    async def initialize_stock_list(self, stock_list=None):
        """종목 정보 초기화 - 외부에서 종목 정보 제공받거나 갱신"""
        try:
            if stock_list:
                # 외부에서 종목 정보를 제공받은 경우
                success = self.stock_cache.init_stock_info(stock_list)
                
                if success:
                    logger.info(f"종목 정보 초기화 완료: {len(stock_list)}개 종목")
                    return True
                else:
                    logger.error("종목 캐시 초기화 실패")
                    return False
            else:
                logger.error("종목 정보가 제공되지 않았습니다")
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
            
            # 백엔드 서버 인증 상태 확인
            if not self.auth_client.is_authenticated:
                logger.error("백엔드 서버 인증되지 않았습니다. 먼저 로그인이 필요합니다.")
                return False
            
            # 키움 API 토큰 발급 (최초 1회)
            kiwoom_token = await self.kiwoom_auth_client.get_access_token()
            if not kiwoom_token:
                logger.error("키움 API 토큰 발급 실패")
                return False
            
            # 웹소켓 클라이언트 초기화
            self.websocket_client = KiwoomWebSocket(
                base_url=self.websocket_url,
                session=self.session,
                stock_cache=self.stock_cache
            )
            
            # 연결 상태 설정
            self.connected = True
            logger.info("REST API 연결 완료")
            return True
        
        except Exception as e:
            logger.error(f"API 연결 중 오류: {str(e)}")
            return False
    
    def update_account_info(self, account_info):
        """외부에서 제공된 계좌 정보로 업데이트"""
        if account_info:
            self.account_info = account_info
            logger.info(f"계좌정보 업데이트: 예수금={self.account_info['cash_balance']}, 종목수={len(self.account_info['positions'])}")
            return True
        return False
    
    async def get_filtered_symbols(self, top_kospi: int = 450, top_kosdaq: int = 150) -> List[str]:
        """시가총액 기준으로 종목 필터링"""
        try:
            # 키움 API 토큰 가져오기
            kiwoom_token = await self.kiwoom_auth_client.get_access_token()
            
            if not kiwoom_token:
                logger.error("키움 API 토큰을 가져올 수 없습니다")
                return []

            # 키움 API 요청 헤더 구성
            headers = {
                "Content-Type": "application/json",
                "Accept": "application/json",
                "Authorization": f"Bearer {kiwoom_token}",
                "api-id": "ka10099"
            }

            # 코스피, 코스닥 시장 정보 요청
            market_types = [('0', 'KOSPI'), ('10', 'KOSDAQ')]
            all_stocks = []

            for market_code, market_name in market_types:
                # 로깅 추가
                logger.info(f"{market_name} 시장 종목 조회 시작")
                
                # 요청 파라미터
                params = {'mrkt_tp': market_code}
                
                async with self.session.post(
                    f"{self.base_url}/api/dostk/stkinfo", 
                    headers=headers, 
                    json=params
                ) as response:
                    logger.info(f"{market_name} 시장 조회 응답 상태: {response.status}")
                    
                    if response.status == 200:
                        data = await response.json()
                        stocks = data.get('list', [])
                        
                        logger.info(f"{market_name} 시장 종목 수: {len(stocks)}")
                        
                        # 종목 정보 처리
                        all_market_stocks = []
                        for stock in stocks:
                            try:
                                list_count = int(stock.get('listCount', '0').replace(',', ''))
                                last_price = int(stock.get('lastPrice', '0').replace(',', ''))
                                
                                market_cap = list_count * last_price
                                
                                all_market_stocks.append({
                                    'code': stock.get('code'),
                                    'name': stock.get('name'),
                                    'market_cap': market_cap
                                })
                            except (ValueError, TypeError) as e:
                                logger.warning(f"종목 처리 중 오류: {e}")
                                continue
                        
                        # 시가총액 기준 정렬
                        all_market_stocks.sort(key=lambda x: x['market_cap'], reverse=True)
                        
                        # 상위 N개 종목 선택 (코스피 450, 코스닥 150)
                        top_count = top_kospi if market_name == 'KOSPI' else top_kosdaq
                        selected_stocks = all_market_stocks[:top_count]
                        
                        logger.info(f"{market_name} 시장 전체 조회 종목 수: {len(all_market_stocks)}, 상위 {top_count}개 선택")
                        
                        all_stocks.extend([stock['code'] for stock in selected_stocks])
                    else:
                        logger.error(f"{market_name} 종목 정보 조회 실패: HTTP {response.status}")
                        error_body = await response.text()
                        logger.error(f"오류 응답: {error_body}")

            logger.info(f"총 선택된 종목 수: {len(all_stocks)}")
            return all_stocks

        except Exception as e:
            logger.error(f"종목 필터링 중 오류: {str(e)}")
            return []
            
    async def initialize_chart_data(self, symbols, from_date=None, period=90):
        """여러 종목의 차트 데이터를 한 번에 초기화하고 캐싱"""
        try:
            logger.info(f"차트 데이터 일괄 초기화 시작: {len(symbols)}개 종목")
            
            # 세션이 없으면 생성
            if not self.session:
                self.session = aiohttp.ClientSession()

            kiwoom_token = await self.kiwoom_auth_client.get_access_token()
                
            # 차트 캐시 초기화
            self.chart_cache = {}
            
            # 필터링된 종목 리스트 저장 (다른 메소드에서 접근할 수 있도록)
            self.filtered_stockcode_list = symbols
            
            # 동시 요청 제한 (5개씩 처리)
            batch_size = 5
            total_batches = (len(symbols) + batch_size - 1) // batch_size
            processed_count = 0
            
            for batch_index in range(total_batches):
                start_idx = batch_index * batch_size
                end_idx = min(start_idx + batch_size, len(symbols))
                current_batch = symbols[start_idx:end_idx]
                
                logger.info(f"처리 중인 배치: {batch_index+1}/{total_batches}, 종목: {current_batch}")
                
                # 배치 내 종목 병렬 처리 (gather 사용)
                tasks = [
                    self.get_daily_chart_data(
                        code, 
                        from_date=from_date,
                        period=period,
                        force_reload=True,
                        kiwoom_token=kiwoom_token
                    ) for code in current_batch
                ]
                
                results = await asyncio.gather(*tasks, return_exceptions=True)
                
                # 결과 처리
                for i, result in enumerate(results):
                    code = current_batch[i]
                    if isinstance(result, Exception):
                        logger.error(f"종목 {code} 차트 데이터 조회 실패: {str(result)}")
                    elif result and len(result) > 0:
                        processed_count += 1
                        logger.debug(f"종목 {code} 차트 데이터 조회 성공: {len(result)}개 데이터")
                
                # 배치 간 간격 (API 부하 제어)
                await asyncio.sleep(1)
            
            logger.info(f"차트 데이터 일괄 초기화 완료: {processed_count}/{len(symbols)} 성공")
            return processed_count > 0
        
        except Exception as e:
            logger.error(f"차트 데이터 일괄 초기화 중 오류: {str(e)}")
            return False
    
    async def get_daily_chart_data(self, code, from_date=None, to_date=None, period=None, force_reload=None, kiwoom_token=None):
        """일별 차트 데이터 조회 - Envelope 지표 계산용"""
        try:
            # 캐시 키 생성 (코드 + 기간 정보로)
            cache_key = f"daily_{code}_{from_date or ''}_{to_date or ''}_{period or ''}"
            
            # 새로운 데이터 요청
            logger.info(f"일봉 데이터 요청: {code}")
            
            # 키움 API 토큰 가져오기
            kiwoom_token = await self.kiwoom_auth_client.get_access_token()
        
            if not kiwoom_token:
                logger.error(f"키움 API 토큰을 가져올 수 없습니다: {code}")
                return []
                
            # 키움 API 요청 헤더 구성
            headers = {
                "Content-Type": "application/json",
                "Accept": "application/json",
                "Authorization": f"Bearer {kiwoom_token}",
                "api-id": "ka10081"  # API ID 설정
            }
            
            # API 요청 파라미터 구성
            params = {
                "stk_cd": code,  # 종목코드
                "upd_stkpc_tp": "1"  # 수정주가구분
            }
            
            # 기준일자 설정 (to_date가 있으면 사용, 없으면 오늘 날짜)
            if to_date:
                params["base_dt"] = to_date
            else:
                params["base_dt"] = datetime.now().strftime("%Y%m%d")

            # API 요청
            async with self.session.post(
                f"{self.base_url}/api/dostk/chart",
                json=params,
                headers=headers
            ) as response:
                if response.status == 200:
                    chart_data = await response.json()
                    
                    # 응답 데이터 처리
                    all_data = []
                    chart_items = chart_data.get("stk_dt_pole_chart_qry", [])
                    
                    for item in chart_items:
                        data_item = {
                            "date": item.get("dt", ""),  # 일자
                            "open": float(item.get("open_pric", "0") or "0"),  # 시가
                            "high": float(item.get("high_pric", "0") or "0"),  # 고가
                            "low": float(item.get("low_pric", "0") or "0"),    # 저가
                            "close": float(item.get("cur_prc", "0") or "0"),   # 현재가 (종가)
                            "volume": int(item.get("trde_qty", "0") or "0")    # 거래량
                        }
                        all_data.append(data_item)
                    
                    # 날짜 기준 내림차순 정렬 (최신 데이터가 앞에 오도록)
                    all_data.sort(key=lambda x: x["date"], reverse=True)

                    logger.info(f"일봉 데이터 조회 성공: {code}, {len(all_data)}개 데이터")
                    
                    # 캐시 초기화 (필요 시)
                    if not hasattr(self, 'chart_cache'):
                        self.chart_cache = {}
                        
                    # 캐시에 저장
                    self.chart_cache[cache_key] = all_data
                    
                    return all_data
                else:
                    logger.error(f"차트 데이터 요청 실패: {code} HTTP {response.status}")
                    error_body = await response.text()
                    logger.error(f"오류 응답: {error_body}")
                    return []
        
        except Exception as e:
            logger.error(f"차트 데이터 요청 중 오류: {code} - {str(e)}")
            return []
    
    async def start_websocket(self, real_data_callback: Callable = None):
        """웹소켓 연결 시작 및 구독 설정"""
        try:
            if not self.websocket_client:
                logger.error("웹소켓 클라이언트가 초기화되지 않았습니다.")
                return False
            
            # 웹소켓 연결 및 로그인
            kiwoom_token = await self.kiwoom_auth_client.get_access_token()
            
            connected = await self.websocket_client.connect(kiwoom_token)
            if not connected:
                logger.error("웹소켓 연결 실패")
                return False
            
            # 콜백 함수 설정
            if real_data_callback:
                self.websocket_client.real_data_callback = real_data_callback
            
            # 구독 종목 그룹 준비
            if self.stock_cache.stock_info_cache:
                all_symbols = list(self.stock_cache.stock_info_cache.keys())
                await self.websocket_client.prepare_subscription_groups(all_symbols)
                
                # 구독 로테이션 시작
                await self.websocket_client.start_rotating_subscriptions(
                    callback=real_data_callback,
                    kiwoom_token=kiwoom_token  # 변경: access_token에서 kiwoom_token으로 변경
                )
                
                # 메시지 처리 루프 시작
                asyncio.create_task(self.websocket_client.handle_websocket_message())
                
                logger.info("웹소켓 연결 및 구독 설정 완료")
                return True
            else:
                logger.error("종목 정보가 초기화되지 않았습니다.")
                return False
        
        except Exception as e:
            logger.error(f"웹소켓 연결 시작 중 오류: {str(e)}")
            return False
    
    async def prepare_subscription_groups(self, all_symbols: List[str], group_size: int = 30):
        """구독 그룹 준비 - 웹소켓 클라이언트에 위임"""
        if not self.websocket_client:
            logger.error("웹소켓 클라이언트가 초기화되지 않았습니다.")
            return False
            
        return await self.websocket_client.prepare_subscription_groups(all_symbols, group_size)
        
    async def start_rotating_subscriptions(self, callback: Callable = None):
        """구독 로테이션 시작 - 웹소켓 클라이언트에 위임"""
        if not self.websocket_client:
            logger.error("웹소켓 클라이언트가 초기화되지 않았습니다.")
            return False
        
        # 키움 API 토큰 가져오기
        kiwoom_token = await self.kiwoom_auth_client.get_access_token()
        if not kiwoom_token:
            logger.error("키움 API 토큰을 가져올 수 없습니다.")
            return False
            
        return await self.websocket_client.start_rotating_subscriptions(
            callback=callback,
            kiwoom_token=kiwoom_token  # 변경: access_token에서 kiwoom_token으로 변경
        )
        
    async def handle_websocket_message(self):
        """웹소켓 메시지 처리 루프 - 웹소켓 클라이언트에 위임"""
        if not self.websocket_client:
            logger.error("웹소켓 클라이언트가 초기화되지 않았습니다.")
            return False
            
        return await self.websocket_client.handle_websocket_message()
    
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
            
            # 백엔드 서버 인증 상태 확인
            if not self.auth_client.is_authenticated:
                logger.error("백엔드 서버 인증되지 않았습니다. 거래를 실행할 수 없습니다.")
                return {
                    "success": False,
                    "message": "백엔드 서버 인증되지 않았습니다. 거래를 실행할 수 없습니다.",
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
            
            # 백엔드 API를 통한 주문 전송 (백엔드 서버 API 토큰 사용)
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
            # 웹소켓 연결 종료
            if self.websocket_client:
                await self.websocket_client.close()
            
            # HTTP 세션 종료
            if self.session and not self.session.closed:
                await self.session.close()
            
            self.connected = False
            logger.info("REST API 연결 종료")
            return True
        except Exception as e:
            logger.error(f"API 연결 종료 중 오류: {str(e)}")
            return False