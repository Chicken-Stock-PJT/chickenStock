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
        self.callback = None  # 실시간 데이터 처리 콜백
        self.running = False  # 실행 상태
        self.is_logged_in = False  # 로그인 상태
        self.message_lock = asyncio.Lock()  # 메시지 수신용 락
    
    async def connect(self, kiwoom_token: str):
        """웹소켓 연결 및 로그인"""
        try:
            # 기존 연결이 있으면 종료
            if self.ws:
                await self.close()
            
            # 웹소켓 연결 URL
            ws_url = f"{self.base_url}/api/dostk/websocket"
            
            # 웹소켓 연결
            self.session = aiohttp.ClientSession()
            self.ws = await self.session.ws_connect(ws_url)
            logger.info("웹소켓 서버 연결 성공, 로그인 시도 중...")
            
            # 로그인 메시지 전송
            login_msg = {
                "trnm": "LOGIN",
                "token": kiwoom_token
            }
            await self.ws.send_json(login_msg)
            
            # 로그인 응답 대기
            async with self.message_lock:
                login_response = await self.ws.receive_json()
            
            if login_response.get("trnm") == "LOGIN":
                if login_response.get("return_code") == 0:
                    logger.info("로그인 성공")
                    self.is_logged_in = True
                    self.running = True
                    
                    # 메시지 처리 루프 시작
                    self.message_task = asyncio.create_task(self.message_loop())
                    
                    # 핑 태스크 시작
                    self.ping_task = asyncio.create_task(self.ping_loop())
                    
                    return True
                else:
                    logger.error(f"로그인 실패: {login_response.get('return_msg')}")
                    await self.close()
                    return False
            else:
                logger.error("예상치 못한 응답을 받았습니다.")
                await self.close()
                return False
        
        except Exception as e:
            logger.error(f"웹소켓 연결 또는 로그인 실패: {str(e)}")
            await self.close()
            return False
    
    async def ping_loop(self):
        """PING 메시지 주기적 전송 루프"""
        try:
            while self.running and self.ws and not self.ws.closed:
                await asyncio.sleep(30)  # 30초마다 PING 메시지 전송
                
                if self.ws and not self.ws.closed:
                    try:
                        await self.ws.send_json({"trnm": "PING"})
                        logger.debug("PING 메시지 전송 완료")
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
            if not self.ws or not self.is_logged_in:
                success = await self.connect(kiwoom_token)
                if not success:
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
            
            # 구독된 종목 저장 - 원래 종목코드 사용 (접미사 없는)
            for symbol in symbols:
                self.stock_cache.add_subscribed_symbol(symbol)

            return True
        
        except Exception as e:
            logger.error(f"그룹 구독 중 오류: {str(e)}")
            return False
    
    async def unsubscribe_group(self, group_index: int):
        """특정 그룹 구독 해제"""
        try:
            if not self.ws or self.ws.closed:
                logger.error("웹소켓 연결이 없습니다.")
                return False
            
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
            
            # 구독 해제된 종목 제거 - 원래 종목코드 사용 (접미사 없는)
            for symbol in symbols:
                self.stock_cache.remove_subscribed_symbol(symbol)

            return True
        
        except Exception as e:
            logger.error(f"그룹 구독 해제 중 오류: {str(e)}")
            return False
    
    async def start_rotating_subscriptions(self, callback: Callable = None, kiwoom_token: str = None):
        """구독 로테이션 시작"""
        if not self.subscription_groups:
            logger.error("구독 그룹이 준비되지 않았습니다.")
            return False
        
        self.callback = callback
        
        # 웹소켓 연결 및 로그인 확인
        if not self.is_logged_in:
            success = await self.connect(kiwoom_token)
            if not success:
                return False
        
        # 로테이션 태스크 시작
        self.rotation_task = asyncio.create_task(self.rotation_loop(kiwoom_token))
        
        logger.info("구독 로테이션 시작")
        return True
    
    async def rotation_loop(self, kiwoom_token: str):
        """구독 로테이션 루프"""
        try:
            while self.running:
                # 이전 그룹 구독 해제
                if len(self.subscription_groups) > 1:  # 그룹이 2개 이상인 경우만 로테이션
                    await self.unsubscribe_group(self.current_group_index)
                
                # 다음 그룹 인덱스로 이동
                self.current_group_index = (self.current_group_index + 1) % len(self.subscription_groups)
                
                # 새 그룹 구독
                await self.subscribe_group(self.current_group_index, kiwoom_token)
                
                # 각 그룹당 5초 동안 유지
                await asyncio.sleep(5)
                
        
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
            
            while self.running and self.ws and not self.ws.closed:
                # 메시지 수신
                async with self.message_lock:
                    msg = await self.ws.receive()
                
                # 모든 메시지 타입 로깅 (디버깅 용도)
                logger.debug(f"웹소켓 메시지 수신: 타입={msg.type}, 데이터 길이={len(msg.data) if hasattr(msg, 'data') and msg.data else 0}")
                
                # 연결 종료 확인
                if msg.type == aiohttp.WSMsgType.CLOSED:
                    logger.warning("웹소켓 연결 닫힘")
                    break
                elif msg.type == aiohttp.WSMsgType.ERROR:
                    logger.error(f"웹소켓 오류: {self.ws.exception()}")
                    break
                
                # 데이터 메시지 처리
                if msg.type == aiohttp.WSMsgType.TEXT:
                    try:
                        data = json.loads(msg.data)
                        
                        # 모든 메시지 타입 로깅 (디버깅)
                        logger.debug(f"수신 메시지 타입: {data.get('trnm')}")
                        
                        # PING 메시지 응답
                        if data.get("trnm") == "PING":
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
        
        except asyncio.CancelledError:
            logger.info("메시지 처리 루프 취소됨")
        except Exception as e:
            logger.error(f"메시지 처리 루프 오류: {str(e)}")
    
    async def close(self):
        """웹소켓 연결 종료"""
        self.running = False
        self.is_logged_in = False
        
        # 로테이션 태스크 취소
        if self.rotation_task:
            self.rotation_task.cancel()
            try:
                await self.rotation_task
            except asyncio.CancelledError:
                pass
            self.rotation_task = None
        
        # 핑 태스크 취소
        if self.ping_task:
            self.ping_task.cancel()
            try:
                await self.ping_task
            except asyncio.CancelledError:
                pass
            self.ping_task = None
        
        # 메시지 태스크 취소
        if self.message_task:
            self.message_task.cancel()
            try:
                await self.message_task
            except asyncio.CancelledError:
                pass
            self.message_task = None
        
        # 웹소켓 연결 종료
        if self.ws and not self.ws.closed:
            await self.ws.close()
            self.ws = None
        
        # 세션 종료
        if self.session and not self.session.closed:
            await self.session.close()
            self.session = None
        
        # 구독 상태 초기화
        self.stock_cache.clear_subscribed_symbols()
        logger.info("웹소켓 연결 종료 완료")