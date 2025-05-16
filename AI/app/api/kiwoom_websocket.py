import logging
import asyncio
import aiohttp
import json
import random
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
        self.health_check_task = None  # 헬스 체크 태스크
        self.callback = None  # 실시간 데이터 처리 콜백
        self.running = False  # 실행 상태
        self.is_logged_in = False  # 로그인 상태
        self.message_lock = asyncio.Lock()  # 메시지 수신용 락
        
        # 장애 복구 관련 설정
        self.reconnect_attempts = 0  # 재연결 시도 횟수
        self.max_reconnect_attempts = 10  # 최대 재연결 시도 횟수
        self.base_delay = 2  # 기본 재연결 대기 시간(초)
        self.max_delay = 300  # 최대 재연결 대기 시간(초)
        self.last_pong_time = None  # 마지막 PONG 수신 시간
        self.kiwoom_token = None  # 토큰 저장
        self.subscribed_groups = set()  # 현재 구독 중인 그룹 인덱스 저장
    
    async def connect(self, kiwoom_token: str):
        """웹소켓 연결 및 로그인"""
        try:
            # 토큰 저장
            self.kiwoom_token = kiwoom_token
            
            # 기존 연결이 있으면 종료
            if self.ws and not self.ws.closed:
                await self.close()
            
            # 웹소켓 연결 URL
            ws_url = f"{self.base_url}/api/dostk/websocket"
            
            # 웹소켓 연결
            if not self.session or self.session.closed:
                self.session = aiohttp.ClientSession()
            
            logger.info("웹소켓 서버 연결 시도 중...")
            self.ws = await self.session.ws_connect(
                ws_url,
                heartbeat=30.0,
                timeout=60.0
            )
            logger.info("웹소켓 서버 연결 성공, 로그인 시도 중...")
            
            # 로그인 메시지 전송
            login_msg = {
                "trnm": "LOGIN",
                "token": kiwoom_token
            }
            await self.ws.send_json(login_msg)
            
            # 로그인 응답 대기
            try:
                async with self.message_lock:
                    login_response = await asyncio.wait_for(
                        self.ws.receive_json(),
                        timeout=10.0  # 10초 타임아웃
                    )
                
                if login_response.get("trnm") == "LOGIN":
                    if login_response.get("return_code") == 0:
                        logger.info("로그인 성공")
                        self.is_logged_in = True
                        self.running = True
                        self.reconnect_attempts = 0  # 재연결 시도 횟수 초기화
                        self.last_pong_time = asyncio.get_event_loop().time()  # 현재 시간으로 초기화
                        
                        # 메시지 처리 루프 시작
                        if not self.message_task or self.message_task.done():
                            self.message_task = asyncio.create_task(self.message_loop())
                        
                        # 핑 태스크 시작
                        if not self.ping_task or self.ping_task.done():
                            self.ping_task = asyncio.create_task(self.ping_loop())
                        
                        # 헬스 체크 태스크 시작
                        if not self.health_check_task or self.health_check_task.done():
                            self.health_check_task = asyncio.create_task(self.health_check_loop())
                        
                        # 연결 성공 후 이전에 구독 중이던 그룹 복원
                        if self.subscribed_groups:
                            logger.info(f"이전 구독 그룹 복원 시도: {self.subscribed_groups}")
                            for group_idx in list(self.subscribed_groups):
                                await self.subscribe_group(group_idx, kiwoom_token)
                        
                        return True
                    else:
                        logger.error(f"로그인 실패: {login_response.get('return_msg')}")
                        await self.close()
                        # 로그인 실패 시 재연결 시도 (로그인 오류라면 토큰 문제일 수 있음)
                        await self.schedule_reconnect()
                        return False
                else:
                    logger.error("예상치 못한 응답을 받았습니다.")
                    await self.close()
                    await self.schedule_reconnect()
                    return False
            
            except asyncio.TimeoutError:
                logger.error("로그인 응답 대기 시간 초과")
                await self.close()
                await self.schedule_reconnect()
                return False
        
        except Exception as e:
            logger.error(f"웹소켓 연결 중 오류 발생: {str(e)}", exc_info=True)
            await self.close()
            await self.schedule_reconnect()
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
            logger.error(f"웹소켓 연결 또는 로그인 실패: {str(e)}")
            await self.close()
            await self.schedule_reconnect()
            return False
    
    async def schedule_reconnect(self):
        """지수 백오프 방식으로 재연결 스케줄링"""
        if self.reconnect_attempts >= self.max_reconnect_attempts:
            logger.error(f"최대 재연결 시도 횟수({self.max_reconnect_attempts}회)를 초과했습니다. 재연결 중단.")
            return False
        
        # 지수 백오프 + 지터 계산
        delay = min(self.base_delay * (2 ** self.reconnect_attempts), self.max_delay)
        jitter = random.uniform(0, delay * 0.1)  # 10% 지터 추가
        total_delay = delay + jitter
        
        logger.info(f"재연결 예정: {total_delay:.2f}초 후 (시도 {self.reconnect_attempts + 1}/{self.max_reconnect_attempts})")
        
        self.reconnect_attempts += 1
        
        # 재연결 태스크 생성
        if self.reconnect_task and not self.reconnect_task.done():
            self.reconnect_task.cancel()
            
        self.reconnect_task = asyncio.create_task(self._delayed_reconnect(total_delay))
        return True
    
    async def _delayed_reconnect(self, delay: float):
        """지연 후 재연결 시도"""
        try:
            await asyncio.sleep(delay)
            logger.info("재연결 시도 시작...")
            
            if self.kiwoom_token:
                success = await self.connect(self.kiwoom_token)
                if success:
                    logger.info("재연결 성공")
                    # 재연결 성공 후 로테이션 재시작
                    if self.running and (not self.rotation_task or self.rotation_task.done()):
                        # kiwoom_token 파라미터 전달하지 않음 - 클래스 속성 사용
                        self.rotation_task = asyncio.create_task(self.rotation_loop())
                    else:
                        logger.info("로테이션 태스크가 이미 실행 중입니다.")
                else:
                    logger.error("재연결 실패")
            else:
                logger.error("토큰이 없어 재연결할 수 없습니다.")
        
        except asyncio.CancelledError:
            logger.info("재연결 태스크 취소됨")
        except Exception as e:
            logger.error(f"재연결 중 오류: {str(e)}")
            # 재연결 중 오류 발생 시 다시 스케줄링
            await self.schedule_reconnect()
    
    async def health_check_loop(self):
        """연결 상태 확인 루프"""
        try:
            while self.running:
                await asyncio.sleep(15)  # 15초마다 상태 확인
                
                # 웹소켓 연결 확인
                if not self.ws or self.ws.closed:
                    logger.warning("웹소켓 연결이 끊어졌습니다. 재연결 시도 중...")
                    await self.schedule_reconnect()
                    continue
                
                # PONG 응답 확인 부분 제거됨
        
        except asyncio.CancelledError:
            logger.info("헬스 체크 루프 취소됨")
        except Exception as e:
            logger.error(f"헬스 체크 루프 오류: {str(e)}")
    
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
                        # PING 전송 실패는 연결 문제의 징후일 수 있음
                        await self.schedule_reconnect()
                        break
        
        except asyncio.CancelledError:
            logger.info("PING 루프 취소됨")
        except Exception as e:
            logger.error(f"PING 루프 오류: {str(e)}")
            await self.schedule_reconnect()
    
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
    
    async def subscribe_group(self, group_index: int, kiwoom_token: str = None):
        """특정 그룹 구독"""
        try:
            # 토큰이 전달되지 않았으면 저장된 토큰 사용
            if not kiwoom_token and self.kiwoom_token:
                kiwoom_token = self.kiwoom_token
            
            # 연결 확인 및 필요 시 재연결
            if not self.ws or not self.is_logged_in:
                if not kiwoom_token:
                    logger.error("구독을 위한 토큰이 없습니다.")
                    return False
                
                success = await self.connect(kiwoom_token)
                if not success:
                    logger.error("구독을 위한 연결 실패")
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
            
            # 구독 요청 전송 (3회까지 재시도)
            max_retries = 3
            for attempt in range(max_retries):
                try:
                    if self.ws and not self.ws.closed:
                        await self.ws.send_json(subscribe_msg)
                        
                        # 구독 성공으로 간주하고 구독 그룹 기록
                        self.subscribed_groups.add(group_index)
                        
                        # 구독된 종목 저장 - 원래 종목코드 사용 (접미사 없는)
                        for symbol in symbols:
                            self.stock_cache.add_subscribed_symbol(symbol)
                        
                        logger.info(f"그룹 {group_index} 구독 성공")
                        return True
                    else:
                        # 소켓이 닫힌 경우 재연결 시도
                        logger.warning(f"웹소켓이 닫혀있어 구독 실패 (시도 {attempt+1}/{max_retries}), 재연결 시도...")
                        if kiwoom_token:
                            await self.connect(kiwoom_token)
                        else:
                            logger.error("재연결에 필요한 토큰이 없습니다.")
                            return False
                        
                        if attempt == max_retries - 1:  # 마지막 시도에서 실패
                            return False
                
                except Exception as e:
                    if attempt < max_retries - 1:  # 마지막 시도가 아니면
                        logger.warning(f"그룹 구독 중 오류 (시도 {attempt+1}/{max_retries}): {str(e)}, 재시도...")
                        await asyncio.sleep(1 * (attempt + 1))  # 지연 후 재시도
                    else:
                        logger.error(f"그룹 구독 실패 (최대 시도 횟수 초과): {str(e)}")
                        return False
            
            # 모든 시도 실패
            return False
        
        except Exception as e:
            logger.error(f"그룹 구독 프로세스 중 오류: {str(e)}")
            return False
    
    async def unsubscribe_group(self, group_index: int, retry=True):
        """특정 그룹 구독 해제"""
        try:
            # 연결 상태 확인
            if not self.ws or self.ws.closed:
                logger.error("웹소켓 연결이 없습니다.")
                return False if not retry else await self.reconnect_and_unsubscribe(group_index)
            
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
            try:
                await self.ws.send_json(unsubscribe_msg)
                
                # 구독 그룹에서 제거
                if group_index in self.subscribed_groups:
                    self.subscribed_groups.remove(group_index)
                
                # 구독 해제된 종목 제거 - 원래 종목코드 사용 (접미사 없는)
                for symbol in symbols:
                    self.stock_cache.remove_subscribed_symbol(symbol)
                
                logger.info(f"그룹 {group_index} 구독 해제 성공")
                return True
            
            except Exception as e:
                logger.error(f"구독 해제 메시지 전송 중 오류: {str(e)}")
                return False if not retry else await self.reconnect_and_unsubscribe(group_index)
        
        except Exception as e:
            logger.error(f"그룹 구독 해제 중 오류: {str(e)}")
            # 연결 끊김 오류인 경우 연결 상태 업데이트
            if "Cannot write to closing transport" in str(e) or "Connection closed" in str(e):
                logger.warning("전송 중 연결이 닫힘, 연결 상태 업데이트")
                self.is_logged_in = False
                self.ws = None
            return False
    
    async def reconnect_and_unsubscribe(self, group_index: int):
        """재연결 후 구독 해제 시도"""
        if not self.kiwoom_token:
            logger.error("토큰이 없어 재연결할 수 없습니다.")
            return False
        
        # 재연결 시도
        success = await self.connect(self.kiwoom_token)
        if not success:
            logger.error("구독 해제를 위한 재연결 실패")
            return False
        
        # 재연결 성공 후 구독 해제 재시도 (재귀적으로 재시도하지 않음)
        return await self.unsubscribe_group(group_index, retry=False)
    
    async def start_rotating_subscriptions(self, callback: Callable = None, kiwoom_token: str = None):
        """구독 로테이션 시작"""
        if not self.subscription_groups:
            logger.error("구독 그룹이 준비되지 않았습니다.")
            return False
        
        self.callback = callback
        
        # 토큰 저장
        if kiwoom_token:
            self.kiwoom_token = kiwoom_token
        
        # 웹소켓 연결 및 로그인 확인
        if not self.is_logged_in:
            if not self.kiwoom_token:
                logger.error("연결에 필요한 토큰이 없습니다.")
                return False
                
            success = await self.connect(self.kiwoom_token)
            if not success:
                return False
        
        # 로테이션 태스크 시작
        if not self.rotation_task or self.rotation_task.done():
            if not self.rotation_task or self.rotation_task.done():
                # kiwoom_token 파라미터 전달 - 뒤에서 저장된 토큰을 사용할 수 있도록 구현
                self.rotation_task = asyncio.create_task(self.rotation_loop())
            
        logger.info("구독 로테이션 시작")
        return True
    
    async def rotation_loop(self, kiwoom_token=None):
        """구독 로테이션 루프"""
        try:
            consecutive_failures = 0  # 연속 실패 카운터
            
            # 토큰이 없으면 저장된 토큰 사용
            if not kiwoom_token and self.kiwoom_token:
                kiwoom_token = self.kiwoom_token
            
            while self.running:
                try:
                    # 이전 그룹 구독 해제
                    if len(self.subscription_groups) > 1:  # 그룹이 2개 이상인 경우만 로테이션
                        success = await self.unsubscribe_group(self.current_group_index)
                        if not success:
                            logger.warning(f"그룹 {self.current_group_index} 구독 해제 실패, 계속 진행합니다.")
                    
                    # 다음 그룹 인덱스로 이동
                    self.current_group_index = (self.current_group_index + 1) % len(self.subscription_groups)
                    
                    # 새 그룹 구독
                    success = await self.subscribe_group(self.current_group_index, kiwoom_token)
                    
                    if success:
                        consecutive_failures = 0  # 성공 시 실패 카운터 초기화
                    else:
                        consecutive_failures += 1
                        logger.warning(f"그룹 {self.current_group_index} 구독 실패 (연속 실패: {consecutive_failures})")
                        
                        # 연속 3회 이상 실패 시 재연결 시도
                        if consecutive_failures >= 3:
                            logger.error(f"연속 {consecutive_failures}회 구독 실패, 재연결 시도 중...")
                            await self.close()
                            await self.schedule_reconnect()
                            break
                    
                    # 각 그룹당 5초 동안 유지
                    await asyncio.sleep(5)
                
                except Exception as e:
                    logger.error(f"로테이션 단계 중 오류: {str(e)}")
                    consecutive_failures += 1
                    
                    # 연속 3회 이상 실패 시 재연결 시도
                    if consecutive_failures >= 3:
                        logger.error(f"로테이션 중 연속 {consecutive_failures}회 오류, 재연결 시도 중...")
                        await self.close()
                        await self.schedule_reconnect()
                        break
                    
                    await asyncio.sleep(1)  # 오류 발생 시 잠시 대기
        
        except asyncio.CancelledError:
            logger.info("구독 로테이션 루프 취소됨")
        except Exception as e:
            logger.error(f"구독 로테이션 루프 오류: {str(e)}")
            await self.schedule_reconnect()
    
    async def message_loop(self):
        """웹소켓 메시지 처리 루프"""
        try:
            # 메시지 카운터 초기화
            message_count = 0
            last_group_index = self.current_group_index
            error_count = 0  # 연속 오류 카운터
            
            # 웹소켓 연결 상태 로깅
            logger.info(f"메시지 루프 시작: 웹소켓 연결 상태 = {not self.ws.closed if self.ws else 'None'}")
            
            while self.running and self.ws and not self.ws.closed:
                try:
                    # 메시지 수신 (타임아웃 적용)
                    async with self.message_lock:
                        try:
                            msg = await asyncio.wait_for(
                                self.ws.receive(),
                                timeout=60.0  # 60초 타임아웃
                            )
                        except asyncio.TimeoutError:
                            logger.warning("메시지 수신 타임아웃, 연결 확인 중...")
                            if self.ws and not self.ws.closed:
                                # 핑 전송으로 연결 확인
                                try:
                                    await self.ws.send_json({"trnm": "PING"})
                                    logger.info("연결 확인 PING 전송 성공")
                                    error_count = 0  # 오류 카운터 초기화
                                    continue
                                except Exception:
                                    logger.error("연결 확인 PING 전송 실패, 재연결 필요")
                                    error_count += 1
                            
                            # 오류 카운터 증가, 3회 이상이면 재연결
                            if error_count >= 3:
                                logger.error(f"연속 {error_count}회 타임아웃, 재연결 필요")
                                await self.schedule_reconnect()
                                break
                            continue
                    
                    # 오류 카운터 초기화
                    error_count = 0
                    
                    # 연결 종료 확인
                    if msg.type == aiohttp.WSMsgType.CLOSED:
                        logger.warning("웹소켓 연결 닫힘, 재연결 필요")
                        await self.schedule_reconnect()
                        break
                    elif msg.type == aiohttp.WSMsgType.ERROR:
                        logger.error(f"웹소켓 오류: {self.ws.exception()}")
                        await self.schedule_reconnect()
                        break
                    
                    # 데이터 메시지 처리
                    if msg.type == aiohttp.WSMsgType.TEXT:
                        try:
                            data = json.loads(msg.data)
                            
                            # PING/PONG 메시지 응답
                            if data.get("trnm") == "PING":
                                logger.debug("PING 메시지 수신, 응답 전송")
                                await self.ws.send_json(data)  # 동일한 내용으로 응답
                                # 마지막 PONG 시간 업데이트
                                self.last_pong_time = asyncio.get_event_loop().time()
                                continue
                            
                            # PONG 응답 처리
                            if data.get("trnm") == "PONG" or (data.get("trnm") == "PING" and data.get("is_response")):
                                logger.debug("PONG 메시지 수신")
                                self.last_pong_time = asyncio.get_event_loop().time()
                                continue
                                
                            # 그룹이 변경되었을 때 이전 그룹의 메시지 통계 출력
                            if data.get("trnm") == "REG" and last_group_index != self.current_group_index:
                                # 이전 그룹에 대한 통계 출력
                                cache_size = len(self.stock_cache.price_cache) if hasattr(self.stock_cache, 'price_cache') else 0
                                logger.info(f"수신 메시지 {message_count}개, 현재 캐시 크기: {cache_size}개")
                                
                                # 카운터 초기화 및 현재 그룹 인덱스 저장
                                message_count = 0
                                last_group_index = self.current_group_index
                                continue
                            
                            # 실시간 데이터 처리 (trnm이 REAL인 경우)
                            if data.get("trnm") == "REAL":
                                real_data_list = data.get("data", [])
                                for real_data in real_data_list:
                                    try:
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
                                                       
                                                       # 콜백 함수 실행
                                                       if self.callback:
                                                           try:
                                                               await self.callback(symbol, price)
                                                           except Exception as callback_err:
                                                               logger.error(f"콜백 함수 실행 중 오류: {str(callback_err)}")
                                                except ValueError as ve:
                                                    logger.error(f"현재가 변환 오류: {price_str} - {str(ve)}")
                                            else:
                                                logger.warning(f"현재가 정보 없음: values={values}")
                                    except Exception as real_err:
                                        logger.error(f"실시간 데이터 처리 중 오류: {str(real_err)}")
                        
                        except json.JSONDecodeError as jde:
                            logger.error(f"JSON 디코딩 오류: {msg.data[:100]}... - {str(jde)}")
                        except Exception as msg_err:
                            logger.error(f"메시지 처리 중 오류: {str(msg_err)}")
                
                except asyncio.CancelledError:
                    logger.info("메시지 루프가 취소되었습니다.")
                    break
                except Exception as loop_err:
                    logger.error(f"메시지 처리 루프 내부 오류: {str(loop_err)}")
                    error_count += 1
                    
                    # 연속 5회 이상 오류 발생 시 재연결 시도
                    if error_count >= 5:
                        logger.error(f"연속 {error_count}회 오류 발생, 재연결 시도...")
                        await self.schedule_reconnect()
                        break
                        
                    # 오류 발생 후 잠시 대기
                    await asyncio.sleep(0.5)
        
        except asyncio.CancelledError:
            logger.info("메시지 처리 루프 취소됨")
        except Exception as e:
            logger.error(f"메시지 처리 루프 치명적 오류: {str(e)}")
            await self.schedule_reconnect()
        finally:
            logger.info("메시지 처리 루프 종료")
    
    async def close(self):
        """웹소켓 연결 종료"""
        logger.info("웹소켓 연결 종료 시작")
        self.running = False
        self.is_logged_in = False
        
        # 태스크 취소 (순서 중요)
        tasks = [
            (self.health_check_task, "헬스 체크"),
            (self.rotation_task, "로테이션"),
            (self.ping_task, "핑"),
            (self.message_task, "메시지 처리"),
            (self.reconnect_task, "재연결")
        ]
        
        for task, name in tasks:
            if task and not task.done():
                logger.info(f"{name} 태스크 취소 중...")
                task.cancel()
                try:
                    await asyncio.wait_for(asyncio.shield(task), timeout=2.0)
                    logger.info(f"{name} 태스크 취소 완료")
                except asyncio.TimeoutError:
                    logger.warning(f"{name} 태스크 취소 타임아웃")
                except asyncio.CancelledError:
                    logger.info(f"{name} 태스크 취소됨")
                except Exception as e:
                    logger.error(f"{name} 태스크 취소 중 오류: {str(e)}")
        
        # 속성 초기화
        self.health_check_task = None
        self.rotation_task = None
        self.ping_task = None
        self.message_task = None
        self.reconnect_task = None
        
        # 웹소켓 연결 종료
        if self.ws and not self.ws.closed:
            try:
                logger.info("웹소켓 연결 닫는 중...")
                await self.ws.close()
                logger.info("웹소켓 연결 닫힘")
            except Exception as e:
                logger.error(f"웹소켓 연결 종료 중 오류: {str(e)}")
            self.ws = None
        
        # 세션 종료
        if self.session and not self.session.closed:
            try:
                logger.info("aiohttp 세션 종료 중...")
                await self.session.close()
                logger.info("aiohttp 세션 종료됨")
            except Exception as e:
                logger.error(f"세션 종료 중 오류: {str(e)}")
            self.session = None
        
        # 구독 상태 초기화
        self.stock_cache.clear_subscribed_symbols()
        logger.info("웹소켓 연결 종료 완료")