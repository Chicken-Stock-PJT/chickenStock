import logging
import asyncio
import json
from typing import Dict, List, Callable, Any
import aiohttp
from datetime import datetime

from app.stock_cache import StockCache
from app.kiwoom_auth import KiwoomAuthClient

logger = logging.getLogger(__name__)

class KiwoomWebSocket:
    """키움 API 웹소켓 연결 관리 클래스"""
    
    def __init__(self, base_url: str, session: aiohttp.ClientSession, stock_cache: StockCache):
        """웹소켓 클래스 초기화"""
        # 웹소켓 URL
        self.websocket_url = base_url
        
        # HTTP 세션 (외부에서 제공)
        self.session = session
        
        # 종목 캐시 (외부에서 제공)
        self.stock_cache = stock_cache
        
        # 연결 상태
        self.connected = False
        
        # 실시간 데이터 콜백 함수
        self.real_data_callback = None
        
        # 웹소켓 연결
        self.websocket = None
        
        # 웹소켓 구독 관리 태스크
        self._subscription_task = None
        
        # 구독 그룹 관리
        self._subscription_groups = []
        self._current_group_index = 0

        # 키움 API 토큰 클라이언트 추가
        self.kiwoom_auth_client = KiwoomAuthClient()

        # 키움 토큰 저장
        self.kiwoom_token = ''

        # PING 메시지 관련 설정
        self._ping_interval = 3  # 3초마다 PING 메시지 전송
        self._ping_task = None
    
    async def connect(self, kiwoom_token: str = None) -> bool:
        """웹소켓 연결 및 로그인"""
        try:
            # 키움 API 토큰 가져오기 또는 전달받은 토큰 사용
            if kiwoom_token:
                self.kiwoom_token = kiwoom_token
            else:
                self.kiwoom_token = await self.kiwoom_auth_client.get_access_token()

            # 웹소켓 연결 시도
            await self._start_websocket()
            
            # 웹소켓 로그인 시도
            login_success = await self._websocket_login(self.kiwoom_token)
            
            if not login_success:
                logger.warning("웹소켓 로그인 실패")
                return False
            
            # 연결 상태 업데이트
            self.connected = True
            logger.info("웹소켓 연결 및 로그인 성공")

            # PING 태스크 시작
            self._start_ping_task()

            # 메시지 핸들링 태스크 시작 - 이 부분 추가
            self._message_task = asyncio.create_task(self.handle_websocket_message())
            logger.info("웹소켓 메시지 처리 태스크 시작됨")
            
            return True
        except Exception as e:
            logger.error(f"웹소켓 연결 설정 중 오류: {str(e)}")
            return False
        
    def _start_ping_task(self):
        """주기적으로 PING 메시지를 보내는 태스크 시작"""
        if not self._ping_task or self._ping_task.done():
            self._ping_task = asyncio.create_task(self._ping_loop())
            logger.info(f"PING 메시지 전송 태스크 시작 (간격: {self._ping_interval}초)")

    async def _ping_loop(self):
        """주기적으로 PING 메시지를 보내는 루프"""
        try:
            logger.info("PING 메시지 전송 루프 시작")
            while self.connected and self.websocket and not self.websocket.closed:
                try:
                    # PING 메시지 전송
                    ping_data = {"trnm": "PING"}
                    ping_message = json.dumps(ping_data)
                    await self.websocket.send_str(ping_message)
                    logger.debug("PING 메시지 전송 완료")
                    
                    # 다음 PING까지 대기
                    await asyncio.sleep(self._ping_interval)
                except Exception as e:
                    logger.error(f"PING 메시지 전송 중 오류: {str(e)}")
                    await asyncio.sleep(5)  # 오류 발생 시 5초 후 재시도
        except asyncio.CancelledError:
            logger.info("PING 메시지 전송 루프가 취소되었습니다.")
        except Exception as e:
            logger.error(f"PING 메시지 전송 루프 오류: {str(e)}")

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

    async def _websocket_login(self, kiwoom_token: str):
        """웹소켓 로그인 처리 - 키움증권 API 토큰 사용"""
        if not self.websocket or self.websocket.closed:
            logger.error("웹소켓 연결이 없어 로그인할 수 없습니다.")
            return False
        
        try:
            # 로그인 메시지 생성 - 키움증권 API 토큰 사용
            login_data = {
                "trnm": "LOGIN",
                "token": kiwoom_token
            }
            
            # 로그인 요청 전송 (딕셔너리를 문자열로 직렬화하여 전송)
            login_message = json.dumps(login_data)
            await self.websocket.send_str(login_message)
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
        
        # all_symbols가 딕셔너리 리스트인 경우(객체 리스트) 코드 문자열만 추출
        if all_symbols and isinstance(all_symbols[0], dict):
            # 종목 코드만 추출
            symbol_codes = []
            for stock in all_symbols:
                if 'shortCode' in stock:
                    symbol_codes.append(stock['shortCode'])
                elif 'code' in stock:
                    symbol_codes.append(stock['code'])
            all_symbols = symbol_codes
        
        # group_size씩 그룹화
        for i in range(0, len(all_symbols), group_size):
            self._subscription_groups.append(all_symbols[i:i+group_size])
        
        logger.info(f"{len(self._subscription_groups)}개 구독 그룹 생성 (그룹당 최대 {group_size}개 종목)")
        self._current_group_index = 0
        return True
    
    async def start_rotating_subscriptions(self, callback: Callable = None, kiwoom_token: str = None):
        """3초마다 다른 그룹을 구독하는 로테이션 시작 - 키움증권 API 토큰 사용"""
        # 콜백 함수 설정
        if callback:
            self.real_data_callback = callback
        
        # 키움 API 토큰 설정
        if kiwoom_token:
            self.kiwoom_token = kiwoom_token
        
        # 구독 태스크 시작
        if not self._subscription_task or self._subscription_task.done():
            self._subscription_task = asyncio.create_task(self._subscription_rotation_loop())
            return True
        return False
    
    async def _subscription_rotation_loop(self):
        """구독 로테이션 루프 - 키움증권 API 토큰 사용"""
        try:
            # 웹소켓 연결 및 로그인
            if not self.connected:
                ws_connected = await self.connect(self.kiwoom_token)
                if not ws_connected:
                    logger.error("웹소켓 연결 실패: 실시간 데이터 구독을 할 수 없습니다.")
                    return
            
            while self.connected:
                try:
                    # 현재 시간 확인 - 16시 이후인지 체크
                    current_time = datetime.now()
                    after_market_hours = current_time.hour >= 16
                    
                    # 현재 구독 중인 그룹 해제
                    if self._subscription_groups and self._current_group_index < len(self._subscription_groups):
                        # 구독된 심볼이 있는 경우에만 해지 요청
                        subscribed_symbols = list(self.stock_cache.subscribed_symbols)
                        if subscribed_symbols:
                            await self._unregister_realtime_data(subscribed_symbols)
                        
                        # 구독 캐시 초기화
                        self.stock_cache.clear_subscribed_symbols()
                        
                        # 다음 그룹으로 이동
                        self._current_group_index = (self._current_group_index + 1) % len(self._subscription_groups)
                        next_group = self._subscription_groups[self._current_group_index]
                        
                        # 16시 이후인 경우 더 큰 구독 그룹 사용
                        if after_market_hours:
                            # 현재 그룹과 다음 2개 그룹을 합쳐서 구독 (최대 100개 종목)
                            extended_group = list(next_group)
                            max_additional_groups = 3  # 현재 그룹 포함 최대 4개 그룹(약 120개 종목)
                            
                            for i in range(1, max_additional_groups):
                                next_index = (self._current_group_index + i) % len(self._subscription_groups)
                                extended_group.extend(self._subscription_groups[next_index])
                                
                                # 100개 제한에 도달하면 중단
                                if len(extended_group) >= 100:
                                    break
                            
                            # 100개로 제한
                            if len(extended_group) > 100:
                                extended_group = extended_group[:100]
                            
                            logger.info(f"16시 이후 확장 구독 모드: {len(extended_group)}개 종목 구독")
                            
                            # 확장 그룹 구독
                            await self._register_realtime_data(extended_group)
                            for code in extended_group:
                                self.stock_cache.add_subscribed_symbol(code)
                        else:
                            # 일반 시간대 - 기존 방식으로 다음 그룹만 구독
                            await self._register_realtime_data(next_group)
                            for code in next_group:
                                self.stock_cache.add_subscribed_symbol(code)
                        
                        # 대기 시간도 시간대별로 다르게 설정
                        if after_market_hours:
                            await asyncio.sleep(30)  # 16시 이후는 30초 대기
                        else:
                            await asyncio.sleep(10)  # 일반 시간대는 10초 대기
                    else:
                        logger.warning("구독할 그룹이 없습니다.")
                        await asyncio.sleep(3)
                
                except Exception as e:
                    logger.error(f"구독 로테이션 중 오류: {str(e)}")
                    await asyncio.sleep(3)  # 오류 발생 시 3초 대기 후 재시도
        
        except asyncio.CancelledError:
            logger.info("구독 로테이션 루프가 취소되었습니다.")
        
        except Exception as e:
            logger.error(f"구독 로테이션 루프 오류: {str(e)}")
        
        finally:
            # 모든 구독 해제 - 구독된 심볼이 있는 경우에만
            subscribed_symbols = list(self.stock_cache.subscribed_symbols)
            if self.websocket and not self.websocket.closed and subscribed_symbols:
                await self._unregister_realtime_data(subscribed_symbols)
            self.stock_cache.clear_subscribed_symbols()
            
    async def _register_realtime_data(self, codes: List[str]):
        """실시간 데이터 구독 등록 - 키움증권 API 토큰 사용"""
        if not self.websocket or self.websocket.closed:
            logger.error("웹소켓 연결이 없습니다.")
            return False
        
        try:
            # 실시간 데이터 구독 요청 메시지 생성
            subscribe_data = {
                "trnm": "REG",
                "grp_no": "1",
                "refresh": "1",
                "data": [{
                    "item": [code + "_AL" for code in codes],
                    "type": ['0B']
                }]
            }

            # 요청 전송
            subscribe_message = json.dumps(subscribe_data)
            await self.websocket.send_str(subscribe_message)
            logger.info(f"{len(codes)}개 종목 실시간 시세 구독 요청 전송")
            
            return True
        except Exception as e:
            logger.error(f"실시간 데이터 구독 요청 중 오류: {str(e)}")
            return False

    async def _unregister_realtime_data(self, codes: List[str] = None):
        """실시간 데이터 구독 해지 - 키움증권 API 토큰 사용"""
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
                "grp_no": "1",
                "data": {
                    "item": [item + "_AL" for item in items],
                    "type": ['0B']
                }
            }
            
            # 요청 전송
            unsubscribe_message = json.dumps(unsubscribe_data)
            await self.websocket.send_str(unsubscribe_message)
            logger.info(f"{len(items)}개 종목 실시간 시세 구독 해지 요청 전송")
            
            return True
        except Exception as e:
            logger.error(f"실시간 데이터 구독 해지 요청 중 오류: {str(e)}")
            return False
    
    async def handle_websocket_message(self):
        """웹소켓 메시지 처리 루프 - 키움증권 API 토큰 사용"""
        try:
            # 가격 업데이트 통계를 위한 카운터
            update_counter = 0
            last_log_time = datetime.now()
            
            logger.info("웹소켓 메시지 처리 루프 시작")
            
            while self.connected and self.websocket and not self.websocket.closed:
                msg = await self.websocket.receive()
                
                if msg.type == aiohttp.WSMsgType.TEXT:
                    try:
                        data = json.loads(msg.data)
                        
                        # 원본 메시지 구조 로깅
                        trnm = data.get('trnm')
                        
                        # PING 응답 처리
                        if trnm == "PING":
                            logger.debug("PING 응답 수신")
                            continue
                        
                        # 실시간 시세 데이터 처리
                        if trnm == "REAL":
                            data_list = data.get("data", [])
                            
                            for item in data_list:
                                item_type = item.get("type")
                                item_code = item.get("item")
                                
                                if item_type == "0B":  # 주식체결 타입
                                    values = item.get("values", {})
                                    
                                    price_str = values.get("10", "0")  # 현재가
                                    
                                    # 현재가가 문자열로 오므로 정수로 변환 (부호 처리)
                                    try:
                                        # 부호가 포함된 문자열 처리
                                        price = int(price_str.replace('+', '').replace('-', '').replace(',', ''))
                                        
                                        if item_code and price > 0:
                                            # 캐시 업데이트
                                            self.stock_cache.update_price(item_code, price)
                                            
                                            # 업데이트 카운터 증가
                                            update_counter += 1
                                            
                                            # 콜백 함수 호출
                                            if self.real_data_callback:
                                                asyncio.create_task(self.real_data_callback(item_code, price))
                                    except ValueError as ve:
                                        logger.error(f"현재가 변환 오류: {price_str} - {str(ve)}")
                        
                        # 등록 응답 처리
                        elif trnm == "REG":
                            return_code = data.get("return_code")
                            return_msg = data.get("return_msg", "")
                            
                            if return_code == 0:
                                logger.info("실시간 시세 구독 등록 성공")
                            else:
                                logger.error(f"실시간 시세 구독 등록 실패: {return_code} - {return_msg}")
                        
                        # 10초마다 가격 업데이트 통계 로깅
                        current_time = datetime.now()
                        if (current_time - last_log_time).total_seconds() >= 10:
                            cache_size = len(self.stock_cache.price_cache)
                            logger.info(f"실시간 가격 업데이트: 지난 10초간 {update_counter}건 수신, 현재 캐시 크기: {cache_size}건")
                            
                            # 카운터 및 시간 초기화
                            update_counter = 0
                            last_log_time = current_time
                            
                    except json.JSONDecodeError:
                        logger.error(f"JSON 파싱 오류: {msg.data}")
                    except Exception as e:
                        logger.error(f"웹소켓 메시지 처리 중 오류: {str(e)}")
                        import traceback
                        logger.error(traceback.format_exc())
                
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
            import traceback
            logger.error(traceback.format_exc())
        
        finally:
            # 연결 종료 시 재연결 시도
            if self.connected:
                logger.info("웹소켓 메시지 처리 루프 종료, 5초 후 재연결 시도")
                await asyncio.sleep(5)
                if self.session:
                    asyncio.create_task(self.connect(self.kiwoom_token))

    async def close(self):
        """웹소켓 연결 종료"""
        try:
            # PING 태스크 취소
            if self._ping_task and not self._ping_task.done():
                self._ping_task.cancel()
                logger.info("PING 메시지 전송 태스크 취소됨")

            # 메시지 처리 태스크 취소 - 이 부분 추가
            if self._message_task and not self._message_task.done():
                self._message_task.cancel()
                logger.info("웹소켓 메시지 처리 태스크 취소됨")

            # 구독 관리 태스크 취소
            if self._subscription_task and not self._subscription_task.done():
                self._subscription_task.cancel()
                
            # 웹소켓 연결 종료
            if self.websocket and not self.websocket.closed:
                await self.websocket.close()
                
            self.connected = False
            logger.info("웹소켓 연결 종료")
            return True
        except Exception as e:
            logger.error(f"웹소켓 연결 종료 중 오류: {str(e)}")
            return False