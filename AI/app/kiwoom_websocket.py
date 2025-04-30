import logging
import asyncio
import json
from typing import Dict, List, Callable, Any
import aiohttp

from app.stock_cache import StockCache

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
    
    async def connect(self, access_token: str) -> bool:
        """웹소켓 연결 및 로그인"""
        try:
            # 웹소켓 연결 시도
            await self._start_websocket()
            
            # 웹소켓 로그인 시도
            login_success = await self._websocket_login(access_token)
            if not login_success:
                logger.warning("웹소켓 로그인 실패")
                return False
            
            # 연결 상태 업데이트
            self.connected = True
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

    async def _websocket_login(self, access_token: str):
        """웹소켓 로그인 처리"""
        if not self.websocket or self.websocket.closed:
            logger.error("웹소켓 연결이 없어 로그인할 수 없습니다.")
            return False
        
        try:
            # 로그인 메시지 생성 - 액세스 토큰 사용
            login_data = {
                "trnm": "LOGIN",
                "token": access_token
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
    
    async def start_rotating_subscriptions(self, callback: Callable = None, access_token: str = None):
        """3초마다 다른 그룹을 구독하는 로테이션 시작"""
        # 콜백 함수 설정
        if callback:
            self.real_data_callback = callback
        
        # 구독 태스크 시작
        if not self._subscription_task or self._subscription_task.done():
            self._subscription_task = asyncio.create_task(self._subscription_rotation_loop(access_token))
    
    async def _subscription_rotation_loop(self, access_token: str = None):
        """구독 로테이션 루프"""
        try:
            # 웹소켓 연결 및 로그인
            if not self.connected:
                ws_connected = await self.connect(access_token)
                if not ws_connected:
                    logger.error("웹소켓 연결 실패: 실시간 데이터 구독을 할 수 없습니다.")
                    return
            
            while self.connected:
                try:
                    # 현재 구독 중인 그룹 해제
                    if self._subscription_groups and self._current_group_index < len(self._subscription_groups):
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
                if self.session:
                    asyncio.create_task(self.connect(None))  # 액세스 토큰은 외부에서 다시 제공해야 함

    async def close(self):
        """웹소켓 연결 종료"""
        try:
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