import logging
import asyncio
import aiohttp
import json
from typing import List, Callable, Dict, Any
from app.cache.stock_cache import StockCache

logger = logging.getLogger(__name__)

class KiwoomWebSocket:
    """키움 API 웹소켓 연결 및 데이터 구독"""
    
    def __init__(self, base_url: str, stock_cache: StockCache):
        self.base_url = base_url
        self.stock_cache = stock_cache
        self.ws = None
        self.session = None  # aiohttp 세션 저장
        self.subscription_groups = []  # 구독 그룹 목록
        self.current_group_index = 0  # 현재 구독 중인 그룹 인덱스
        self.rotation_task = None  # 로테이션 태스크
        self.ping_task = None      # 핑 태스크
        self.message_task = None   # 메시지 처리 태스크
        self.reconnect_task = None  # 재연결 태스크
        self.callback = None  # 실시간 데이터 처리 콜백
        self.running = False  # 실행 상태
        self.is_logged_in = False  # 로그인 상태
        self.message_lock = asyncio.Lock()  # 메시지 수신용 락
        self.connection_lock = asyncio.Lock()  # 연결 관리용 락
        self.kiwoom_token = None  # 인증 토큰 저장
        self.last_activity_time = 0  # 마지막 활동 시간
        self.reconnect_attempts = 0  # 재연결 시도 횟수
        self.max_reconnect_attempts = 5  # 최대 재연결 시도 횟수
        self.reconnect_delay = 5  # 초기 재연결 지연 시간(초)
        self.max_reconnect_delay = 60  # 최대 재연결 지연 시간(초)
    
    async def connect(self, kiwoom_token: str):
        """웹소켓 연결 및 로그인"""
        if not kiwoom_token:
            logger.error("유효하지 않은 토큰: 토큰이 비어 있습니다")
            return False
            
        async with self.connection_lock:  # 연결 동시성 제어
            try:
                # 저장된 토큰 업데이트
                self.kiwoom_token = kiwoom_token
                
                # 기존 연결이 있으면 종료
                if self.ws:
                    await self.close_connection()
                
                # 웹소켓 연결 URL
                ws_url = f"{self.base_url}/api/dostk/websocket"
                
                logger.info(f"웹소켓 연결 시도: {ws_url}")
                
                # 웹소켓 연결
                if not self.session or self.session.closed:
                    self.session = aiohttp.ClientSession()
                
                # 연결 시도에 타임아웃 추가
                try:
                    self.ws = await asyncio.wait_for(
                        self.session.ws_connect(
                            ws_url, 
                            timeout=30,
                            heartbeat=20  # 하트비트 간격(초)
                        ),
                        timeout=15  # 연결 시도 타임아웃
                    )
                except asyncio.TimeoutError:
                    logger.error("웹소켓 연결 시도 타임아웃")
                    await self.close_connection()
                    return False
                
                logger.info("웹소켓 서버 연결 성공, 로그인 시도 중...")
                
                # 로그인 메시지 전송
                login_msg = {
                    "trnm": "LOGIN",
                    "token": kiwoom_token
                }
                await self.ws.send_json(login_msg)
                
                # 로그인 응답 대기
                async with self.message_lock:
                    login_response = await self.ws.receive_json(timeout=10)  # 타임아웃 추가
                
                if login_response.get("trnm") == "LOGIN":
                    if login_response.get("return_code") == 0:
                        logger.info("로그인 성공")
                        self.is_logged_in = True
                        self.running = True
                        self.reconnect_attempts = 0  # 재연결 시도 횟수 초기화
                        self.last_activity_time = asyncio.get_event_loop().time()  # 활동 시간 업데이트
                        
                        # 메시지 처리 루프 시작
                        if not self.message_task or self.message_task.done():
                            self.message_task = asyncio.create_task(self.message_loop())
                        
                        # 핑 태스크 시작
                        if not self.ping_task or self.ping_task.done():
                            self.ping_task = asyncio.create_task(self.ping_loop())
                        
                        # 상태 모니터링 태스크 시작
                        if not self.reconnect_task or self.reconnect_task.done():
                            self.reconnect_task = asyncio.create_task(self.connection_monitor())
                        
                        return True
                    else:
                        logger.error(f"로그인 실패: {login_response.get('return_msg')}")
                        await self.close_connection()
                        return False
                else:
                    logger.error("예상치 못한 응답을 받았습니다.")
                    await self.close_connection()
                    return False
            
            except asyncio.TimeoutError:
                logger.error("로그인 응답 대기 중 타임아웃 발생")
                await self.close_connection()
                return False
            except Exception as e:
                logger.error(f"웹소켓 연결 또는 로그인 실패: {str(e)}")
                await self.close_connection()
                return False
    
    async def close_connection(self):
        """웹소켓 연결만 종료 (태스크는 유지)"""
        try:
            # 웹소켓 연결 종료
            if self.ws and not self.ws.closed:
                await self.ws.close()
                self.ws = None
            
            # 세션 종료
            if self.session and not self.session.closed:
                await self.session.close()
                self.session = None
            
            self.is_logged_in = False
            logger.info("웹소켓 연결 종료됨")
        except Exception as e:
            logger.error(f"웹소켓 연결 종료 중 오류: {str(e)}")
    
    async def connection_monitor(self):
        """연결 상태 모니터링 및 자동 재연결"""
        try:
            reconnect_in_progress = False
            
            while self.running:
                try:
                    await asyncio.sleep(5)  # 5초마다 확인
                    
                    # 재연결이 이미 진행 중인 경우 스킵
                    if reconnect_in_progress:
                        continue
                    
                    # 연결 상태 확인
                    connection_lost = (
                        not self.ws or 
                        (self.ws and self.ws.closed) or 
                        not self.is_logged_in
                    )
                    
                    # 활동 시간 체크 (2분 이상 활동 없으면 재연결)
                    current_time = asyncio.get_event_loop().time()
                    inactivity_timeout = current_time - self.last_activity_time > 120
                    
                    if connection_lost or inactivity_timeout:
                        logger.warning(
                            f"연결 상태 확인: 연결 끊김={connection_lost}, "
                            f"활동 없음={inactivity_timeout}, "
                            f"재연결 시도 #{self.reconnect_attempts+1}"
                        )
                        
                        if self.kiwoom_token:
                            # 재연결 플래그 설정
                            reconnect_in_progress = True
                            
                            try:
                                # 재연결 시도
                                if self.reconnect_attempts < self.max_reconnect_attempts:
                                    self.reconnect_attempts += 1
                                    
                                    # 지수 백오프 적용
                                    delay = min(self.reconnect_delay * (2 ** (self.reconnect_attempts - 1)), self.max_reconnect_delay)
                                    logger.info(f"재연결 지연: {delay}초 후 시도")
                                    await asyncio.sleep(delay)
                                    
                                    # 재연결 시도
                                    async with self.connection_lock:
                                        reconnect_success = await self.connect(self.kiwoom_token)
                                    
                                    if reconnect_success:
                                        logger.info("재연결 성공")
                                        self.reconnect_attempts = 0  # 성공 시 카운터 초기화
                                        
                                        # 현재 그룹 재구독
                                        if self.subscription_groups:
                                            try:
                                                await self.subscribe_group(self.current_group_index, self.kiwoom_token)
                                                logger.info(f"그룹 {self.current_group_index} 재구독 성공")
                                            except Exception as e:
                                                logger.error(f"그룹 재구독 실패: {str(e)}")
                                    else:
                                        logger.error("재연결 실패")
                                else:
                                    logger.error(f"최대 재연결 시도 횟수({self.max_reconnect_attempts})를 초과하여 중단합니다.")
                                    self.running = False
                                    break
                            finally:
                                # 재연결 플래그 해제
                                reconnect_in_progress = False
                
                except asyncio.CancelledError:
                    raise  # 상위 예외 처리로 전달
                except Exception as e:
                    logger.error(f"연결 모니터링 내부 오류: {str(e)}")
                    reconnect_in_progress = False  # 오류 발생 시 플래그 초기화
        
        except asyncio.CancelledError:
            logger.info("연결 모니터링 태스크 취소됨")
        except Exception as e:
            logger.error(f"연결 모니터링 중 오류: {str(e)}")
    
    async def ping_loop(self):
        """PING 메시지 주기적 전송 루프"""
        try:
            while self.running:
                await asyncio.sleep(30)  # 30초마다 PING 메시지 전송
                
                if self.ws and not self.ws.closed and self.is_logged_in:
                    try:
                        await self.ws.send_json({"trnm": "PING"})
                        logger.debug("PING 메시지 전송 완료")
                        self.last_activity_time = asyncio.get_event_loop().time()  # 활동 시간 업데이트
                    except Exception as e:
                        logger.error(f"PING 메시지 전송 중 오류: {str(e)}")
        
        except asyncio.CancelledError:
            logger.info("PING 루프 취소됨")
        except Exception as e:
            logger.error(f"PING 루프 오류: {str(e)}")
    
    async def prepare_subscription_groups(self, symbols: List[str], group_size: int = 30):
        """구독 그룹 준비"""
        try:
            # 그룹으로 분할 (최대 group_size 개씩)
            self.subscription_groups = []
            for i in range(0, len(symbols), group_size):
                group = symbols[i:i+group_size]
                self.subscription_groups.append(group)
            
            logger.info(f"구독 그룹 준비 완료: {len(self.subscription_groups)}개 그룹, 총 {len(symbols)}개 종목")
            return True
        
        except Exception as e:
            logger.error(f"구독 그룹 준비 중 오류: {str(e)}")
            return False
    
    async def subscribe_group(self, group_index: int, kiwoom_token: str):
        """특정 그룹 구독"""
        try:
            # 연결 상태 확인
            if not self.ws or self.ws.closed or not self.is_logged_in:
                logger.warning("구독 전 연결 확인: 연결이 없거나 로그인되지 않음, 재연결 시도")
                success = await self.connect(kiwoom_token)
                if not success:
                    logger.error("그룹 구독을 위한 재연결 실패")
                    return False
            
            if group_index >= len(self.subscription_groups):
                logger.error(f"유효하지 않은 그룹 인덱스: {group_index}")
                return False
            
            # 현재 그룹의 종목 코드 목록
            symbols = self.subscription_groups[group_index]
            
            # '_AL' 접미사를 붙인 종목코드 생성
            suffixed_symbols = [f"{symbol}_AL" for symbol in symbols]
            
            # 키움 API 명세서에 맞게 구독 메시지 구성
            subscribe_msg = {
                "trnm": "REG",              # 등록
                "grp_no": f"{group_index+1:04d}",  # 그룹번호(4자리)
                "refresh": "1",             # 기존등록유지
                "data": [{
                    "item": suffixed_symbols,  # '_AL' 접미사가 붙은 종목코드 배열
                    "type": ["0B"]          # 주식체결 TR
                }]
            }
            
            # 구독 요청 전송
            await self.ws.send_json(subscribe_msg)
            self.last_activity_time = asyncio.get_event_loop().time()  # 활동 시간 업데이트
            
            # 구독된 종목 저장 - 원래 종목코드 사용 (접미사 없는)
            for symbol in symbols:
                self.stock_cache.add_subscribed_symbol(symbol)

            return True
        
        except Exception as e:
            logger.error(f"그룹 구독 중 오류: {str(e)}")
            # 연결 끊김 오류인 경우 연결 상태 업데이트
            if "Cannot write to closing transport" in str(e) or "Connection closed" in str(e):
                logger.warning("전송 중 연결이 닫힘, 연결 상태 업데이트")
                self.is_logged_in = False
                self.ws = None
            return False
    
    async def unsubscribe_group(self, group_index: int):
        """특정 그룹 구독 해제"""
        try:
            # 연결 상태 확인
            if not self.ws or self.ws.closed:
                logger.warning("구독 해제 시도: 연결이 없거나 닫힘")
                # 구독 해제된 것으로 처리하고 성공 반환
                if group_index < len(self.subscription_groups):
                    symbols = self.subscription_groups[group_index]
                    for symbol in symbols:
                        self.stock_cache.remove_subscribed_symbol(symbol)
                return True
            
            if group_index >= len(self.subscription_groups):
                logger.error(f"유효하지 않은 그룹 인덱스: {group_index}")
                return False
            
            # 현재 그룹의 종목 코드 목록
            symbols = self.subscription_groups[group_index]
            
            # '_AL' 접미사를 붙인 종목코드 생성
            suffixed_symbols = [f"{symbol}_AL" for symbol in symbols]
            
            # 키움 API 명세서에 맞게 구독 해제 메시지 구성
            unsubscribe_msg = {
                "trnm": "REMOVE",           # 해지
                "grp_no": f"{group_index+1:04d}",  # 그룹번호(4자리)
                "data": [{
                    "item": suffixed_symbols,  # '_AL' 접미사가 붙은 종목코드 배열
                    "type": ["0B"]          # 주식체결 TR
                }]
            }
            
            # 구독 해제 요청 전송
            await self.ws.send_json(unsubscribe_msg)
            self.last_activity_time = asyncio.get_event_loop().time()  # 활동 시간 업데이트
            
            # 구독 해제된 종목 제거 - 원래 종목코드 사용 (접미사 없는)
            for symbol in symbols:
                self.stock_cache.remove_subscribed_symbol(symbol)

            return True
        
        except Exception as e:
            logger.error(f"그룹 구독 해제 중 오류: {str(e)}")
            # 연결 끊김 오류인 경우 연결 상태 업데이트
            if "Cannot write to closing transport" in str(e) or "Connection closed" in str(e):
                logger.warning("전송 중 연결이 닫힘, 연결 상태 업데이트")
                self.is_logged_in = False
                self.ws = None
            return False
    
    async def start_rotating_subscriptions(self, callback: Callable = None, kiwoom_token: str = None):
        """구독 로테이션 시작"""
        if not self.subscription_groups:
            logger.error("구독 그룹이 준비되지 않았습니다.")
            return False
        
        self.callback = callback
        self.kiwoom_token = kiwoom_token  # 토큰 저장
        
        # 웹소켓 연결 및 로그인 확인
        if not self.is_logged_in:
            success = await self.connect(kiwoom_token)
            if not success:
                return False
        
        # 로테이션 태스크 시작
        if not self.rotation_task or self.rotation_task.done():
            self.rotation_task = asyncio.create_task(self.rotation_loop(kiwoom_token))
        
        logger.info("구독 로테이션 시작")
        return True
    
    async def rotation_loop(self, kiwoom_token: str):
        """구독 로테이션 루프"""
        try:
            while self.running:
                try:
                    # 연결 상태 확인
                    if not self.ws or self.ws.closed or not self.is_logged_in:
                        logger.warning("로테이션 중 연결 끊김 감지, 재연결 대기...")
                        await asyncio.sleep(1)  # 짧은 대기 후 다음 루프로
                        continue
                    
                    # 이전 그룹 구독 해제
                    if len(self.subscription_groups) > 1:  # 그룹이 2개 이상인 경우만 로테이션
                        unsubscribe_success = await self.unsubscribe_group(self.current_group_index)
                        if not unsubscribe_success:
                            logger.warning(f"그룹 {self.current_group_index} 구독 해제 실패, 재시도...")
                            await asyncio.sleep(1)
                            continue
                    
                    # 다음 그룹 인덱스로 이동
                    self.current_group_index = (self.current_group_index + 1) % len(self.subscription_groups)
                    
                    # 새 그룹 구독
                    subscribe_success = await self.subscribe_group(self.current_group_index, kiwoom_token)
                    if not subscribe_success:
                        logger.warning(f"그룹 {self.current_group_index} 구독 실패, 재시도...")
                        await asyncio.sleep(1)
                        continue
                    
                    # 각 그룹당 5초 동안 유지
                    await asyncio.sleep(5)
                
                except Exception as e:
                    logger.error(f"로테이션 루프 내부 오류: {str(e)}")
                    await asyncio.sleep(1)  # 오류 발생 시 잠시 대기
        
        except asyncio.CancelledError:
            logger.info("구독 로테이션 루프 취소됨")
        except Exception as e:
            logger.error(f"구독 로테이션 루프 오류: {str(e)}")
    
    async def message_loop(self):
        """웹소켓 메시지 처리 루프"""
        try:
            # 메시지 카운터 초기화
            message_count = 0
            last_group_index = self.current_group_index
            
            # 웹소켓 연결 상태 로깅
            logger.info(f"메시지 루프 시작: 웹소켓 연결 상태 = {not self.ws.closed if self.ws else 'None'}")
            
            while self.running:
                try:
                    # 연결 상태 확인
                    if not self.ws or self.ws.closed:
                        logger.warning("메시지 루프: 연결이 끊어짐, 대기 중...")
                        await asyncio.sleep(1)
                        continue
                    
                    # 메시지 수신 (타임아웃 추가)
                    try:
                        async with self.message_lock:
                            msg = await asyncio.wait_for(self.ws.receive(), timeout=60)
                    except asyncio.TimeoutError:
                        logger.warning("메시지 수신 타임아웃, 연결 상태 확인 중...")
                        if self.ws and not self.ws.closed:
                            try:
                                # 핑 메시지 전송하여 연결 상태 확인
                                await self.ws.send_json({"trnm": "PING"})
                                logger.debug("타임아웃 후 PING 전송 성공, 연결 유지 확인")
                                self.last_activity_time = asyncio.get_event_loop().time()
                                continue
                            except Exception:
                                logger.error("타임아웃 후 PING 전송 실패, 연결이 끊어진 것으로 간주")
                                self.is_logged_in = False
                                continue
                        continue
                    
                    # 활동 시간 업데이트
                    self.last_activity_time = asyncio.get_event_loop().time()
                    
                    # 메시지 타입 및 데이터 안전하게 로깅
                    data_length = 0
                    if hasattr(msg, 'data'):
                        if isinstance(msg.data, (str, bytes, bytearray)):
                            data_length = len(msg.data)
                        else:
                            data_length = 0 if msg.data is None else 1
                    
                    logger.debug(f"웹소켓 메시지 수신: 타입={msg.type}, 데이터 길이={data_length}")
                    
                    # 연결 종료 확인
                    if msg.type == aiohttp.WSMsgType.CLOSED:
                        logger.warning("웹소켓 연결 닫힘 감지")
                        self.is_logged_in = False
                        break
                    elif msg.type == aiohttp.WSMsgType.ERROR:
                        logger.error(f"웹소켓 오류 감지: {self.ws.exception()}")
                        self.is_logged_in = False
                        break
                    
                    # 데이터 메시지 처리
                    if msg.type == aiohttp.WSMsgType.TEXT:
                        try:
                            # 안전한 JSON 파싱
                            if not msg.data:
                                logger.warning("빈 데이터 메시지 수신됨")
                                continue
                                
                            try:
                                data = json.loads(msg.data)
                            except json.JSONDecodeError:
                                logger.error(f"JSON 디코딩 오류: {msg.data[:100]}...")  # 처음 100자만 로깅
                                continue
                            
                            # 모든 메시지 타입 로깅 (디버깅)
                            trnm = data.get("trnm", "UNKNOWN")
                            logger.debug(f"수신 메시지 타입: {trnm}")
                            
                            # PING 메시지 응답
                            if trnm == "PING":
                                logger.debug("PING 메시지 수신, 응답 전송")
                                await self.ws.send_json(data)  # 동일한 내용으로 응답
                                continue
                                    
                            # 그룹이 변경되었을 때 이전 그룹의 메시지 통계 출력
                            if data.get("trnm") == "REG" and last_group_index != self.current_group_index:
                                # 이전 그룹에 대한 통계 출력
                                cache_size = len(self.stock_cache.price_cache)
                                logger.info(f"수신 메시지 {message_count}개, 현재 캐시 크기: {cache_size}개")
                                
                                # 카운터 초기화 및 현재 그룹 인덱스 저장
                                message_count = 0
                                last_group_index = self.current_group_index
                                continue
                            
                            # 실시간 데이터 처리 (trnm이 REAL인 경우)
                            if data.get("trnm") == "REAL":
                                real_data = data.get("data", {})[0]
                                logger.debug(f"REAL 데이터 수신: 타입={real_data.get('type')}, 종목={real_data.get('item')}")
                                
                                # 주식체결(0B) 처리
                                if real_data.get("type") == "0B":
                                    message_count += 1  # 메시지 카운터 증가
                                    
                                    # 수신한 종목코드에서 '_AL' 접미사 제거
                                    suffixed_symbol = real_data.get("item")
                                    symbol = suffixed_symbol
                                    if suffixed_symbol and suffixed_symbol.endswith("_AL"):
                                        symbol = suffixed_symbol[:-3]  # '_AL' 접미사 제거
                                    
                                    values = real_data.get("values", {})
                                    logger.debug(f"종목 {symbol} 체결 데이터 수신: values={values}")

                                    price_str = values.get("10")
                                    if price_str:
                                        try:
                                            price = abs(float(price_str.replace(",", "")))
                                            if symbol and price != 0:
                                                # 접미사가 제거된 종목코드로 캐싱
                                                logger.debug(f"종목 {symbol} (원본: {suffixed_symbol}) 가격 업데이트: {price}")
                                                self.stock_cache.update_price(symbol, price)
                                                if self.callback:
                                                    await self.callback(symbol, price)
                                        except ValueError:
                                            logger.error(f"현재가 변환 오류: {price_str}")
                                    else:
                                        logger.warning(f"현재가 정보 없음: values={values}")
                        
                        except json.JSONDecodeError:
                            logger.error(f"JSON 디코딩 오류: {msg.data}")
                        except Exception as e:
                            logger.error(f"메시지 처리 중 오류: {str(e)}")
                
                except Exception as e:
                    logger.error(f"메시지 루프 내부 오류: {str(e)}")
                    await asyncio.sleep(1)  # 오류 발생 시 잠시 대기
        
        except asyncio.CancelledError:
            logger.info("메시지 처리 루프 취소됨")
        except Exception as e:
            logger.error(f"메시지 처리 루프 오류: {str(e)}")
    
    async def close(self):
        """웹소켓 연결 종료"""
        self.running = False
        self.is_logged_in = False
        
        # 모든 태스크 취소
        for task_name, task in [
            ("rotation_task", self.rotation_task),
            ("ping_task", self.ping_task),
            ("message_task", self.message_task),
            ("reconnect_task", self.reconnect_task)
        ]:
            if task and not task.done():
                task.cancel()
                try:
                    await task
                except asyncio.CancelledError:
                    logger.debug(f"{task_name} 취소됨")
                except Exception as e:
                    logger.error(f"{task_name} 취소 중 오류: {str(e)}")
        
        self.rotation_task = None
        self.ping_task = None
        self.message_task = None
        self.reconnect_task = None
        
        # 웹소켓 연결 종료
        await self.close_connection()
        
        # 구독 상태 초기화
        self.stock_cache.clear_subscribed_symbols()
        logger.info("웹소켓 연결 종료 완료")