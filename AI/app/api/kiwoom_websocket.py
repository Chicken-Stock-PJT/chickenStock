import logging
import asyncio
import aiohttp
from typing import List, Callable
from app.cache.stock_cache import StockCache

logger = logging.getLogger(__name__)

class KiwoomWebSocket:
    """키움 API 웹소켓 연결 및 데이터 구독"""
    
    def __init__(self, base_url: str, stock_cache: StockCache):
        self.base_url = base_url
        self.stock_cache = stock_cache
        self.ws = None
        self.subscription_groups = []  # 구독 그룹 목록
        self.current_group_index = 0  # 현재 구독 중인 그룹 인덱스
        self.rotation_task = None  # 로테이션 태스크
        self.callback = None  # 실시간 데이터 처리 콜백
        self.running = False  # 실행 상태
    
    async def connect(self, kiwoom_token: str):
        """웹소켓 연결"""
        try:
            if self.ws:
                await self.close()
            
            # 웹소켓 연결 URL
            ws_url = f"{self.base_url}/ws/v1/quotes/domestic/stocks?authorization={kiwoom_token}"
            
            # 웹소켓 연결
            self.ws = await aiohttp.ClientSession().ws_connect(ws_url)
            logger.info("웹소켓 연결 성공")
            return True
        
        except Exception as e:
            logger.error(f"웹소켓 연결 실패: {str(e)}")
            return False
    
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
            if not self.ws:
                await self.connect(kiwoom_token)
            
            if group_index >= len(self.subscription_groups):
                logger.error(f"유효하지 않은 그룹 인덱스: {group_index}")
                return False
            
            # 현재 그룹의 종목 코드 목록
            symbols = self.subscription_groups[group_index]
            
            # 구독 메시지 구성
            subscribe_msg = {
                "type": "subscribe",
                "codes": symbols,
                "isOnlyRealtime": True
            }
            
            # 구독 요청 전송
            await self.ws.send_json(subscribe_msg)
            
            # 구독된 종목 저장
            for symbol in symbols:
                self.stock_cache.add_subscribed_symbol(symbol)
            
            logger.info(f"그룹 {group_index} 구독 완료: {len(symbols)}개 종목")
            return True
        
        except Exception as e:
            logger.error(f"그룹 구독 중 오류: {str(e)}")
            return False
    
    async def unsubscribe_group(self, group_index: int):
        """특정 그룹 구독 해제"""
        try:
            if not self.ws:
                logger.error("웹소켓 연결이 없습니다.")
                return False
            
            if group_index >= len(self.subscription_groups):
                logger.error(f"유효하지 않은 그룹 인덱스: {group_index}")
                return False
            
            # 현재 그룹의 종목 코드 목록
            symbols = self.subscription_groups[group_index]
            
            # 구독 해제 메시지 구성
            unsubscribe_msg = {
                "type": "unsubscribe",
                "codes": symbols
            }
            
            # 구독 해제 요청 전송
            await self.ws.send_json(unsubscribe_msg)
            
            # 구독 해제된 종목 제거
            for symbol in symbols:
                self.stock_cache.remove_subscribed_symbol(symbol)
            
            logger.info(f"그룹 {group_index} 구독 해제 완료: {len(symbols)}개 종목")
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
        self.running = True
        
        # 메시지 처리 루프 시작
        asyncio.create_task(self.message_loop())
        
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
            while self.running and self.ws:
                # 메시지 수신
                msg = await self.ws.receive()
                
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
                        data = msg.json()
                        
                        # 실시간 시세 메시지 처리
                        if data.get("type") == "price":
                            symbol = data.get("code")
                            price = float(data.get("last", 0))
                            
                            # 현재가 캐싱
                            if symbol and price > 0:
                                self.stock_cache.update_price(symbol, price)
                                
                                # 콜백 함수 호출
                                if self.callback:
                                    await self.callback(symbol, price)
                    
                    except Exception as e:
                        logger.error(f"메시지 처리 중 오류: {str(e)}")
        
        except asyncio.CancelledError:
            logger.info("메시지 처리 루프 취소됨")
        except Exception as e:
            logger.error(f"메시지 처리 루프 오류: {str(e)}")
    
    async def close(self):
        """웹소켓 연결 종료"""
        self.running = False
        
        # 로테이션 태스크 취소
        if self.rotation_task:
            self.rotation_task.cancel()
            try:
                await self.rotation_task
            except asyncio.CancelledError:
                pass
        
        # 웹소켓 연결 종료
        if self.ws:
            await self.ws.close()
            self.ws = None
        
        # 구독 상태 초기화
        self.stock_cache.clear_subscribed_symbols()
        logger.info("웹소켓 연결 종료 완료")