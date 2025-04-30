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
            
            # 인증 상태 확인
            if not self.auth_client.is_authenticated:
                logger.error("인증되지 않았습니다. 먼저 로그인이 필요합니다.")
                return False
            
            # 키움 API 토큰 발급 (최초 1회)
            kiwoom_token = await self.kiwoom_auth_client.get_access_token()
            if not kiwoom_token:
                logger.error("키움 API 토큰 발급 실패")
                return False

            # 종목 정보는 외부(백엔드 클라이언트)에서 제공받도록 변경
            # 계좌 정보도 외부(백엔드 클라이언트)에서 제공받도록 변경
            # 이 두 가지는 이제 BackendClient에서 처리
            
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
        """시가총액 기준으로 종목 필터링 - 연속조회 지원"""
        try:
            # 키움 API 토큰 가져오기
            kiwoom_token = await self.kiwoom_auth_client.get_access_token()
            if not kiwoom_token:
                logger.error("키움 API 토큰을 가져올 수 없습니다")
                return []

            # API 요청 헤더 구성
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
                
                # 연속조회를 위한 변수 초기화
                cont_yn = 'Y'  # 처음에는 데이터가 있다고 가정
                next_key = ''
                page_count = 0
                all_market_stocks = []
                
                # 연속조회 루프 시작
                while cont_yn == 'Y':
                    page_count += 1
                    logger.info(f"{market_name} 시장 종목 조회 - 페이지 {page_count}")
                    
                    # 요청 파라미터
                    params = {'mrkt_tp': market_code}
                    
                    # 연속조회 키가 있으면 추가
                    if next_key:
                        headers['cont-yn'] = 'Y'
                        headers['next-key'] = next_key
                    
                    async with self.session.post(
                        f"{self.base_url}/api/dostk/stkinfo", 
                        headers=headers, 
                        json=params
                    ) as response:
                        logger.info(f"{market_name} 시장 조회 응답 상태: {response.status}")
                        
                        if response.status == 200:
                            data = await response.json()
                            stocks = data.get('list', [])
                            
                            logger.info(f"{market_name} 시장 페이지 {page_count}의 종목 수: {len(stocks)}")
                            
                            # 종목 정보 처리
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
                            
                            # 응답 헤더에서 연속조회 정보 확인
                            cont_yn = response.headers.get('cont-yn', 'N')
                            next_key = response.headers.get('next-key', '')
                            
                            logger.info(f"{market_name} 연속조회 여부: {cont_yn}, 다음 키: {next_key}")
                            
                            # 만약 더 이상 데이터가 없거나 충분한 데이터를 모았다면 종료
                            if cont_yn != 'Y' or len(all_market_stocks) >= max(top_kospi, top_kosdaq) * 2:
                                break
                            
                            # 연속조회 사이에 잠시 대기 (API 제한 고려)
                            await asyncio.sleep(0.5)
                        else:
                            logger.error(f"{market_name} 종목 정보 조회 실패: HTTP {response.status}")
                            error_body = await response.text()
                            logger.error(f"오류 응답: {error_body}")
                            break
                
                # 시가총액 기준 정렬
                all_market_stocks.sort(key=lambda x: x['market_cap'], reverse=True)
                
                # 상위 N개 종목 선택 (코스피 450, 코스닥 150)
                top_count = top_kospi if market_name == 'KOSPI' else top_kosdaq
                selected_stocks = all_market_stocks[:top_count]
                
                logger.info(f"{market_name} 시장 전체 조회 종목 수: {len(all_market_stocks)}, 상위 {len(selected_stocks)}개 선택")
                
                all_stocks.extend([stock['code'] for stock in selected_stocks])

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
                
            # 차트 캐시 초기화
            self.chart_cache = {}
            
            # 동시 요청 제한 (10개씩 처리)
            batch_size = 10
            total_batches = (len(symbols) + batch_size - 1) // batch_size
            processed_count = 0
            
            for batch_index in range(total_batches):
                start_idx = batch_index * batch_size
                end_idx = min(start_idx + batch_size, len(symbols))
                current_batch = symbols[start_idx:end_idx]
                
                # 배치 내 모든 요청을 병렬로 실행
                tasks = []
                for code in current_batch:
                    task = self.get_daily_chart_data(code, from_date=from_date, period=period, force_reload=True)
                    tasks.append(task)
                
                # 모든 태스크 완료 대기
                batch_results = await asyncio.gather(*tasks, return_exceptions=True)
                
                # 결과 확인
                for i, result in enumerate(batch_results):
                    if isinstance(result, Exception):
                        logger.error(f"종목 {current_batch[i]} 차트 데이터 조회 실패: {str(result)}")
                    else:
                        processed_count += 1
                
                # 배치 간 잠깐 대기 (API 제한 고려)
                await asyncio.sleep(0.5)
                
                # 진행 상황 로깅
                logger.info(f"차트 데이터 초기화 진행: {processed_count}/{len(symbols)} 완료 ({(batch_index+1)}/{total_batches} 배치)")
            
            logger.info(f"차트 데이터 일괄 초기화 완료: {processed_count}/{len(symbols)} 성공")
            return processed_count > 0
        
        except Exception as e:
            logger.error(f"차트 데이터 일괄 초기화 중 오류: {str(e)}")
            return False
    
    async def get_daily_chart_data(self, code, from_date=None, to_date=None, period=None, force_reload=False):
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
                
            # API 요청 헤더 구성 (키움 API 토큰 사용)
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
            async with self.session.get(
                f"{self.base_url}/api/dostk/chart",
                params=params,
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
                    
                    # 조회 기간에 맞게 데이터 필터링
                    if from_date:
                        all_data = [item for item in all_data if item["date"] >= from_date]
                    if period:
                        all_data = all_data[:period]
                    
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
            logger.info(kiwoom_token)
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
                    access_token=self.auth_client.access_token
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
            
        return await self.websocket_client.start_rotating_subscriptions(
            callback=callback,
            access_token=self.auth_client.access_token
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