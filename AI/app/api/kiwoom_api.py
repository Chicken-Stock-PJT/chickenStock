"""
키움 API 클라이언트
"""
import logging
import asyncio
import aiohttp
from typing import Dict, List, Callable, Optional, Any
from datetime import datetime
from app.auth.token_manager import TokenManager
from app.cache.stock_cache import StockCache
from app.api.kiwoom_websocket import KiwoomWebSocket
from app.config import settings

logger = logging.getLogger(__name__)

class KiwoomAPI:
    """REST API와 WebSocket을 사용하는 키움 API 클래스"""
    
    def __init__(self, token_manager: TokenManager):
        """API 초기화"""
        # REST API 설정
        self.base_url = settings.API_BASE_URL
        self.websocket_url = settings.WEBSOCKET_API_URL
        
        # 토큰 관리자
        self.token_manager = token_manager
        
        # 키움 API 토큰
        self.kiwoom_token = ""
        
        # HTTP 세션
        self.session = None
        
        # 연결 상태
        self.connected = False
        
        # 종목 캐시
        self.stock_cache = StockCache()
        
        # 웹소켓 클라이언트
        self.websocket_client = None
        
        # 계좌 정보
        self.account_info = {
            "cash_balance": 0,
            "positions": {},
            "total_asset_value": 0
        }
    
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
            
            # 키움 API 토큰 가져오기
            self.kiwoom_token = self.token_manager.token
            if not self.kiwoom_token:
                logger.error("키움 API 토큰을 가져올 수 없습니다")
                return False
            
            # 웹소켓 클라이언트 초기화
            self.websocket_client = KiwoomWebSocket(
                base_url=self.websocket_url,
                stock_cache=self.stock_cache
            )

            # 실제 웹소켓 연결 추가
            websocket_connected = await self.websocket_client.connect(self.kiwoom_token)
            if not websocket_connected:
                logger.error("웹소켓 연결 실패")
                return False
            
            # 연결 상태 설정
            self.connected = True
            logger.info("키움 API 연결 완료")
            return True
        
        except Exception as e:
            logger.error(f"API 연결 중 오류: {str(e)}")
            return False
    
    async def initialize_stock_list(self, stock_list=None):
        """종목 정보 초기화"""
        try:
            if stock_list:
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
    
    def update_account_info(self, account_info):
        """외부에서 제공된 계좌 정보로 업데이트"""
        if account_info:
            self.account_info = account_info
            return True
        return False
    
    def get_account_info(self):
        return self.account_info
    
    def get_positions(self):
        """보유 종목 정보 반환"""
        return self.account_info.get("positions", {})

    def get_cash_balance(self):
        """예수금 반환"""
        return self.account_info.get("cash_balance", 0)
    
    def get_total_asset_value(self):
        """총 자산 가치 반환"""
        return self.account_info.get("total_asset_value", 0)
    
    async def get_filtered_symbols(self, top_kospi: int = 450, top_kosdaq: int = 150) -> dict:
      """시가총액 기준으로 코스피/코스닥 종목 필터링하고 시장 유형과 함께 반환"""
      try:
          if not self.session:
              self.session = aiohttp.ClientSession()
          
          # 키움 API 요청 헤더 구성
          headers = {
              "Content-Type": "application/json",
              "Accept": "application/json",
              "Authorization": f"Bearer {self.kiwoom_token}",
              "api-id": "ka10099"  # API ID 설정
          }

          # 코스피, 코스닥 시장 정보 요청
          market_types = [('0', 'KOSPI'), ('10', 'KOSDAQ')]
          filtered_stocks = {"KOSPI": [], "KOSDAQ": []}

          for market_code, market_name in market_types:
              logger.info(f"{market_name} 시장 종목 조회 시작")
              
              # 요청 파라미터
              params = {'mrkt_tp': market_code}
              
              try:
                  async with self.session.post(
                      f"{self.base_url}/api/dostk/stkinfo", 
                      headers=headers, 
                      json=params
                  ) as response:
                      if response.status == 200:
                          data = await response.json()
                          stocks = data.get('list', [])
                          
                          # 종목 정보 처리
                          market_stocks = []
                          
                          for stock in stocks:
                              try:
                                  # 상장주식수와 종가 데이터가 있는지 확인
                                  if not stock.get('listCount') or not stock.get('lastPrice'):
                                      continue
                                  
                                  list_count = int(stock.get('listCount', '0').replace(',', ''))
                                  last_price = int(stock.get('lastPrice', '0').replace(',', ''))
                                  
                                  # 시가총액이 0인 종목 제외
                                  market_cap = list_count * last_price
                                  if market_cap <= 0:
                                      continue
                                  
                                  market_stocks.append({
                                      'code': stock.get('code'),
                                      'name': stock.get('name'),
                                      'market_cap': market_cap,
                                      'market_type': market_name
                                  })
                              except (ValueError, TypeError) as e:
                                  logger.warning(f"종목({stock.get('code', 'unknown')}) 처리 중 오류: {e}")
                                  continue
                          
                          # 시가총액 기준 정렬
                          market_stocks.sort(key=lambda x: x['market_cap'], reverse=True)
                          
                          # 상위 N개 종목 선택 (코스피 450, 코스닥 150)
                          top_count = top_kospi if market_name == 'KOSPI' else top_kosdaq
                          selected_count = min(top_count, len(market_stocks))
                          selected_stocks = market_stocks[:selected_count]
                          
                          # 시장별로 저장 - 종목 코드와 시장 유형 함께 저장
                          filtered_stocks[market_name] = [
                              {'code': stock['code'], 'market_type': market_name}
                              for stock in selected_stocks
                          ]
                          
                          logger.info(f"{market_name} 시장 상위 {len(filtered_stocks[market_name])}개 선택 (목표: {top_count}개)")
                      else:
                          error_msg = await response.text()
                          logger.error(f"{market_name} 종목 정보 조회 실패: HTTP {response.status}, 응답: {error_msg}")
              except Exception as e:
                  logger.error(f"{market_name} 시장 데이터 요청 중 오류: {str(e)}")
          
          # 로깅
          kospi_count = len(filtered_stocks["KOSPI"])
          kosdaq_count = len(filtered_stocks["KOSDAQ"])
          total_count = kospi_count + kosdaq_count
          
          logger.info(f"필터링된 종목 수: 코스피 {kospi_count}개, 코스닥 {kosdaq_count}개, 총 {total_count}개")
          
          return filtered_stocks

      except Exception as e:
          logger.error(f"종목 필터링 중 오류: {str(e)}", exc_info=True)
          return {"KOSPI": [], "KOSDAQ": []}
      
    async def get_top_trading_amount(self, market_type="0", include_managed=0, exchange_type="3", limit=100):
        try:
            # 세션 확인
            if not self.session:
                self.session = aiohttp.ClientSession()
                
            # 키움 API 요청 헤더 구성
            headers = {
                "Content-Type": "application/json",
                "Accept": "application/json",
                "Authorization": f"Bearer {self.kiwoom_token}",
                "api-id": "ka10032"  # 거래대금상위요청 API ID
            }
            
            # API 요청 파라미터 구성
            params = {
                "mrkt_tp": market_type,  # 시장구분
                "mang_stk_incls": str(include_managed),  # 관리종목 포함 여부
                "stex_tp": exchange_type  # 거래소구분
            }
            
            # API 요청
            async with self.session.post(
                f"{self.base_url}/api/dostk/rkinfo",
                json=params,
                headers=headers
            ) as response:
                if response.status == 200:
                    data = await response.json()
                    
                    # 응답 데이터 처리
                    top_stocks = []
                    
                    if "trde_prica_upper" in data and isinstance(data["trde_prica_upper"], list):
                        stocks = data["trde_prica_upper"][:limit]  # 상위 limit개만 가져오기
                        
                        for stock in stocks:
                            stock_info = {
                                "code": stock.get("stk_cd", ""),  # 종목 코드
                                "name": stock.get("stk_nm", ""),  # 종목명
                                "rank": int(stock.get("now_rank", "0") or "0"),  # 현재 순위
                                "price": float(stock.get("cur_prc", "0").replace(",", "") or "0"),  # 현재가
                                "change_rate": float(stock.get("flu_rt", "0").replace("%", "") or "0"),  # 등락률
                                "volume": int(stock.get("now_trde_qty", "0").replace(",", "") or "0"),  # 거래량
                                "trading_amount": float(stock.get("trde_prica", "0").replace(",", "") or "0"),  # 거래대금
                                "market_type": "KOSPI" if market_type == "001" else "KOSDAQ" if market_type == "101" else "ALL"
                            }
                            top_stocks.append(stock_info)
                    
                    logger.info(f"거래 대금 상위 종목 조회 성공: {len(top_stocks)}개")
                    return top_stocks
                else:
                    logger.error(f"거래 대금 상위 종목 조회 실패: HTTP {response.status}")
                    return []
        
        except Exception as e:
            logger.error(f"거래 대금 상위 종목 조회 중 오류: {str(e)}")
            return []

    async def get_all_top_trading_amount(self, limit=100):
        kospi_top = await self.get_top_trading_amount(market_type="001", limit=limit)
        kosdaq_top = await self.get_top_trading_amount(market_type="101", limit=limit)
        
        # 코스피와 코스닥 종목 통합
        all_top = kospi_top + kosdaq_top
        
        # 거래대금 기준으로 내림차순 정렬
        all_top.sort(key=lambda x: x.get("trading_amount", 0), reverse=True)
        
        # 상위 limit개만 선택
        return all_top[:limit]
            
    async def initialize_chart_data(self, symbols, from_date=None, period=90):
        """여러 종목의 차트 데이터를 한 번에 초기화하고 캐싱"""
        try:
            logger.info(f"차트 데이터 일괄 초기화 시작: {len(symbols)}개 종목")
            
            # 세션이 없으면 생성
            if not self.session:
                self.session = aiohttp.ClientSession()
                
            # 동시 요청 제한 (5개씩 처리)
            batch_size = 5
            total_batches = (len(symbols) + batch_size - 1) // batch_size
            processed_count = 0
            
            for batch_index in range(total_batches):
                start_idx = batch_index * batch_size
                end_idx = min(start_idx + batch_size, len(symbols))
                current_batch = symbols[start_idx:end_idx]
                
                # 배치 내 종목 병렬 처리
                tasks = [
                    self.get_daily_chart_data(code, from_date=from_date, period=period)
                    for code in current_batch
                ]
                
                results = await asyncio.gather(*tasks, return_exceptions=True)
                
                # 결과 처리
                for i, result in enumerate(results):
                    code = current_batch[i]
                    if isinstance(result, Exception):
                        logger.error(f"종목 {code} 차트 데이터 조회 실패: {str(result)}")
                    elif result and len(result) > 0:
                        processed_count += 1
                
                # 배치 간 간격 (API 부하 제어)
                await asyncio.sleep(1)
            
            logger.info(f"차트 데이터 일괄 초기화 완료: {processed_count}/{len(symbols)} 성공")
            return processed_count > 0
        
        except Exception as e:
            logger.error(f"차트 데이터 일괄 초기화 중 오류: {str(e)}")
            return False
    
    async def get_daily_chart_data(self, code, from_date=None, to_date=None, period=None):
        """일별 차트 데이터 조회"""
        try:
            # 세션 확인
            if not self.session:
                self.session = aiohttp.ClientSession()
                
            # 키움 API 요청 헤더 구성
            headers = {
                "Content-Type": "application/json",
                "Accept": "application/json",
                "Authorization": f"Bearer {self.kiwoom_token}",
                "api-id": "ka10081"  # API ID 설정
            }
            
            # API 요청 파라미터 구성
            params = {
                "stk_cd": code,  # 종목코드
                "upd_stkpc_tp": "1"  # 수정주가구분
            }
            
            # 기준일자 설정
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
                    
                    # StockCache에 차트 데이터 저장
                    self.stock_cache.add_chart_data(code, all_data)
                    
                    return all_data
                else:
                    logger.error(f"차트 데이터 요청 실패: {code} HTTP {response.status}")
                    return []
        
        except Exception as e:
            logger.error(f"차트 데이터 요청 중 오류: {code} - {str(e)}")
            return []
        
    async def get_minute_chart(self, symbol: str, time_interval: int = 5, update_cache: bool = True):
        try:
            # 세션 확인
            if not self.session:
                self.session = aiohttp.ClientSession()
                
            # 키움 API 요청 헤더 구성
            headers = {
                "Content-Type": "application/json",
                "Accept": "application/json",
                "Authorization": f"Bearer {self.kiwoom_token}",
                "api-id": "ka10080"  # 분봉 차트 API ID
            }
            
            # API 요청 파라미터 구성
            request_data = {
                "stk_cd": symbol,  # 종목 코드
                "tic_scope": str(time_interval),  # 시간 간격
                "upd_stkpc_tp": "0"  # 수정주가구분 (0: 원주가, 1: 수정주가)
            }
            
            # API 요청
            async with self.session.post(
                f"{self.base_url}/api/dostk/chart",
                json=request_data,
                headers=headers
            ) as response:
                if response.status == 200:
                    response_data = await response.json()
                    
                    if "stk_min_pole_chart_qry" not in response_data:
                        logger.warning(f"종목 {symbol}의 분봉 데이터 응답 형식 오류")
                        return []
                    
                    # 분봉 데이터 파싱
                    minute_chart_data = response_data["stk_min_pole_chart_qry"]
                    
                    # 필요한 경우 데이터 변환 (API 응답 형식에 따라 조정)
                    processed_data = []
                    for item in minute_chart_data:
                        processed_item = {
                            "time": item.get("cntr_tm", ""),  # 체결시간
                            "open": float(item.get("open_pric", 0) or 0),  # 시가
                            "high": float(item.get("high_pric", 0) or 0),  # 고가
                            "low": float(item.get("low_pric", 0) or 0),  # 저가
                            "close": float(item.get("cur_prc", 0) or 0),  # 종가 (현재가)
                            "volume": float(item.get("trde_qty", 0) or 0)  # 거래량
                        }
                        processed_data.append(processed_item)
                    
                    # 최근 50개만 유지
                    if len(processed_data) > 50:
                        processed_data = processed_data[:50]
                    
                    # 캐시 업데이트
                    if update_cache and self.stock_cache:
                        self.stock_cache.add_minute_chart_data(symbol, processed_data)
                    
                    return processed_data
                else:
                    logger.error(f"종목 {symbol}의 분봉 데이터 요청 실패: HTTP {response.status}")
                    return []
        except Exception as e:
            logger.error(f"종목 {symbol}의 분봉 데이터 조회 중 오류: {str(e)}")
            return []
    
    async def initialize_minute_chart_data(self, symbols: List[str], time_interval: int = 5):

        logger.info(f"{len(symbols)}개 종목의 {time_interval}분봉 데이터 초기화 시작")
        
        success_count = 0
        
        # 여러 종목을 병렬로 처리하기 위한 코루틴 함수
        async def fetch_minute_chart(symbol):
            try:
                result = await self.get_minute_chart(symbol, time_interval)
                return len(result) > 0
            except Exception as e:
                logger.error(f"종목 {symbol}의 분봉 데이터 초기화 중 오류: {str(e)}")
                return False
        
        # 동시 요청 수 제한 (너무 많은 동시 요청은 API 제한에 걸릴 수 있음)
        batch_size = 5
        
        # 종목을 배치로 나누어 처리
        for i in range(0, len(symbols), batch_size):
            batch = symbols[i:i+batch_size]
            
            # 배치 내 종목들을 병렬로 처리
            tasks = [fetch_minute_chart(symbol) for symbol in batch]
            results = await asyncio.gather(*tasks, return_exceptions=True)
            
            # 성공 횟수 계산
            success_count += sum(1 for result in results if result == True)
            
            # API 호출 제한을 고려한 딜레이
            await asyncio.sleep(1)
        
        logger.info(f"{time_interval}분봉 데이터 초기화 완료: {success_count}/{len(symbols)}개 성공")
        return success_count > 0
    
    async def prepare_subscription_groups(self, filtered_stock_list, group_size: int = 30):
        """구독 그룹 준비"""
        if not self.websocket_client:
            logger.error("웹소켓 클라이언트가 초기화되지 않았습니다.")
            return False
            
        all_symbols = [stock.get("shortCode") for stock in filtered_stock_list if stock.get("shortCode")]
        return await self.websocket_client.prepare_subscription_groups(all_symbols, group_size)
        
    async def start_rotating_subscriptions(self, callback: Callable = None):
        """구독 로테이션 시작"""
        if not self.websocket_client:
            logger.error("웹소켓 클라이언트가 초기화되지 않았습니다.")
            return False
        
        return await self.websocket_client.start_rotating_subscriptions(
            callback=callback,
            kiwoom_token=self.kiwoom_token
        )
    
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
            logger.info("키움 API 연결 종료")
            return True
        except Exception as e:
            logger.error(f"API 연결 종료 중 오류: {str(e)}")
            return False