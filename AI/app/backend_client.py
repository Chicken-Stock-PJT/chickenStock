import asyncio
import json
import logging
import websockets
from typing import Dict, Any, List, Optional
from datetime import datetime
from .config import settings
from .ai_trading import TradingModel, StockScreener
from .models import MarketData, TradeDecision

logger = logging.getLogger(__name__)

class BackendClient:
    def __init__(self):
        self.ws_url = settings.BACKEND_WS_URL
        self.connected = False
        self.websocket = None
        self.market_data_buffer = {}  # 심볼별 최신 시장 데이터
        self.screener = StockScreener()
        self.trading_model = TradingModel()
        self.active_symbols = set()  # 현재 모니터링 중인 종목
        
    async def connect(self) -> bool:
        """백엔드 서버와 웹소켓 연결 설정"""
        try:
            self.websocket = await websockets.connect(self.ws_url)
            self.connected = True
            logger.info(f"Backend websocket connected to {self.ws_url}")
            
            # 연결 후 시장 전체 데이터 요청
            await self.request_market_data()
            
            # 메시지 리스닝 시작
            asyncio.create_task(self.listen_messages())
            
            return True
        except Exception as e:
            logger.error(f"Failed to connect to backend: {str(e)}")
            self.connected = False
            return False
    
    async def listen_messages(self):
        """백엔드로부터 메시지 수신 및 처리"""
        if not self.connected or not self.websocket:
            logger.error("Cannot listen: Not connected to backend")
            return
                
        try:
            async for message in self.websocket:
                # 메시지 파싱
                try:
                    data = json.loads(message)
                    await self.process_message(data)
                except json.JSONDecodeError:
                    logger.error(f"Failed to parse message: {message}")
                    continue
                    
        except websockets.exceptions.ConnectionClosed:
            logger.warning("Backend websocket connection closed")
            self.connected = False
            # 재연결 시도
            await self.reconnect()
        except Exception as e:
            logger.error(f"Error in websocket listener: {str(e)}")
            self.connected = False
            await self.reconnect()
    
    async def reconnect(self, max_attempts=5, delay=5):
        """백엔드 서버에 재연결 시도"""
        attempts = 0
        while attempts < max_attempts and not self.connected:
            logger.info(f"Attempting to reconnect ({attempts+1}/{max_attempts})...")
            success = await self.connect()
            if success:
                logger.info("Reconnected successfully")
                return True
            
            attempts += 1
            await asyncio.sleep(delay)
        
        if not self.connected:
            logger.error(f"Failed to reconnect after {max_attempts} attempts")
        return self.connected
            
    async def process_message(self, data: Dict[str, Any]):
        """수신된 메시지 처리"""
        msg_type = data.get("type")
        
        if msg_type == "market_data":
            # 시장 데이터 처리
            await self.process_market_data(data["data"])
        elif msg_type == "trade_result":
            # 거래 결과 처리
            await self.process_trade_result(data["data"])
        elif msg_type == "market_snapshot":
            # 시장 전체 스냅샷 처리
            await self.process_market_snapshot(data["data"])
        else:
            logger.warning(f"Unknown message type: {msg_type}")
    
    async def process_market_data(self, data: Dict[str, Any]):
        """개별 종목 시장 데이터 처리"""
        try:
            symbol = data["symbol"]
            
            # 마켓 데이터 객체 생성
            market_data = MarketData(
                symbol=symbol,
                timestamp=datetime.fromtimestamp(data["timestamp"] / 1000),
                price=data["price"],
                volume=data["volume"],
                high=data.get("high"),
                low=data.get("low"),
                bid=data.get("bid"),
                ask=data.get("ask")
            )
            
            # 데이터 버퍼 업데이트
            if symbol not in self.market_data_buffer:
                self.market_data_buffer[symbol] = []
            self.market_data_buffer[symbol].append(market_data)
            
            # 버퍼 크기 제한
            max_buffer = 100
            if len(self.market_data_buffer[symbol]) > max_buffer:
                self.market_data_buffer[symbol] = self.market_data_buffer[symbol][-max_buffer:]
            
            # 스크리너 업데이트
            self.screener.update_market_data(symbol, market_data)
            
            # 활성 종목이면 트레이딩 모델로 분석
            if symbol in self.active_symbols:
                trade_decision = self.trading_model.process_data(symbol, market_data)
                
                # 신뢰도가 충분히 높은 경우에만 거래 결정 전송
                if trade_decision and trade_decision.confidence >= settings.MIN_CONFIDENCE:
                    await self.send_trade_decision(trade_decision)
        
        except Exception as e:
            logger.error(f"Error processing market data: {str(e)}")
    
    async def process_trade_result(self, data: Dict[str, Any]):
        """거래 결과 처리"""
        try:
            # 거래 결과를 트레이딩 모델에 피드백
            self.trading_model.update_trade_result(data)
            
            logger.info(f"Trade result received: {data['action']} {data['symbol']} "
                         f"at {data['price']}, profit: {data.get('profit', 'N/A')}")
        except Exception as e:
            logger.error(f"Error processing trade result: {str(e)}")
    
    async def process_market_snapshot(self, data: List[Dict[str, Any]]):
        """시장 전체 스냅샷 데이터 처리"""
        try:
            for item in data:
                symbol = item["symbol"]
                
                # 마켓 데이터 객체 생성
                market_data = MarketData(
                    symbol=symbol,
                    timestamp=datetime.fromtimestamp(item["timestamp"] / 1000),
                    price=item["price"],
                    volume=item["volume"],
                    high=item.get("high"),
                    low=item.get("low"),
                    bid=item.get("bid"),
                    ask=item.get("ask")
                )
                
                # 스크리너 업데이트
                self.screener.update_market_data(symbol, market_data)
            
            logger.info(f"Processed market snapshot with {len(data)} symbols")
        except Exception as e:
            logger.error(f"Error processing market snapshot: {str(e)}")
    
    async def send_trade_decision(self, decision: TradeDecision):
        """트레이딩 결정을 백엔드로 전송"""
        if not self.connected or not self.websocket:
            logger.error("Cannot send trade decision: Not connected to backend")
            return False
            
        try:
            message = {
                "type": "trade_decision",
                "data": decision.dict()
            }
            await self.websocket.send(json.dumps(message))
            logger.info(f"Sent trade decision: {decision.action} {decision.symbol} at {decision.price}")
            return True
        except Exception as e:
            logger.error(f"Failed to send trade decision: {str(e)}")
            self.connected = False
            await self.reconnect()
            return False
    
    async def request_market_data(self):
        """시장 전체 데이터 요청"""
        if not self.connected or not self.websocket:
            logger.error("Cannot request market data: Not connected to backend")
            return False
            
        try:
            message = {
                "type": "request",
                "data": {
                    "action": "market_snapshot"
                }
            }
            await self.websocket.send(json.dumps(message))
            logger.info("Requested market snapshot data")
            return True
        except Exception as e:
            logger.error(f"Failed to request market data: {str(e)}")
            self.connected = False
            await self.reconnect()
            return False
    
    async def update_active_symbols(self, symbols: List[str]):
        """활성 종목 목록 업데이트 및 백엔드에 알림"""
        if not self.connected or not self.websocket:
            logger.error("Cannot update active symbols: Not connected to backend")
            return False
        
        old_symbols = set(self.active_symbols)
        new_symbols = set(symbols)
        
        # 추가된 종목과 제거된 종목 확인
        added = new_symbols - old_symbols
        removed = old_symbols - new_symbols
        
        # 활성 종목 업데이트
        self.active_symbols = new_symbols
        
        try:
            # 백엔드에 구독 변경 알림
            message = {
                "type": "subscription",
                "data": {
                    "subscribe": list(added),
                    "unsubscribe": list(removed)
                }
            }
            await self.websocket.send(json.dumps(message))
            
            if added:
                logger.info(f"Added symbols for trading: {added}")
            if removed:
                logger.info(f"Removed symbols from trading: {removed}")
                
            return True
        except Exception as e:
            logger.error(f"Failed to update active symbols: {str(e)}")
            self.connected = False
            await self.reconnect()
            return False
    
    async def automated_trading_loop(self):
        """자동 종목 스크리닝 및 거래 루프"""
        while True:
            try:
                if not self.connected:
                    # 연결이 끊겼으면 재연결 시도
                    await self.reconnect()
                    if not self.connected:
                        await asyncio.sleep(60)  # 재연결 실패 시 1분 후 재시도
                        continue
                
                # 종목 스크리닝 수행
                logger.info("Performing stock screening")
                top_symbols = self.screener.screen_stocks(max_stocks=settings.MAX_ACTIVE_SYMBOLS)
                
                if top_symbols:
                    # 스크리닝 결과로 활성 종목 업데이트
                    symbols = [item["symbol"] for item in top_symbols]
                    await self.update_active_symbols(symbols)
                    
                    # 스크리닝 결과 로깅
                    logger.info(f"Selected {len(symbols)} symbols for trading: {symbols}")
                    logger.info(f"Screening scores: {[(s['symbol'], round(s['score'], 2)) for s in top_symbols]}")
                else:
                    logger.warning("No symbols selected from screening")
                
                # 다음 스크리닝까지 대기
                await asyncio.sleep(settings.SCREENING_INTERVAL)
                
            except Exception as e:
                logger.error(f"Error in automated trading loop: {str(e)}")
                await asyncio.sleep(300)  # 오류 발생 시 5분 후 재시도
    
    def get_active_symbols(self) -> List[str]:
        """현재 활성화된 종목 목록 반환"""
        return list(self.active_symbols)