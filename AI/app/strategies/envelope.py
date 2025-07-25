import logging
import asyncio
from typing import Dict, List, Any
from datetime import datetime
from app.strategies.base import BaseTradingModel

logger = logging.getLogger(__name__)

class EnvelopeTradingModel(BaseTradingModel):
    """Envelope 전략 기반 AI 트레이딩 모델"""
    
    def __init__(self, stock_cache=None):
        """Envelope 전략 트레이딩 모델 초기화"""
        super().__init__(stock_cache)
        
        # 매매 관련 설정
        self.max_positions = 7  # 최대 보유 종목 수
        self.trade_amount_per_stock = 14000000  # 종목당 매매 금액 (1000만원)

        # 매매 신호 저장 딕셔너리
        self.trading_signals = {}  # {symbol: {"signal": "lower", "price": price, "timestamp": datetime}}
        
        # 거래 이력 저장
        self.trade_history = {}  # {symbol: {"last_trade": datetime, "last_action": "buy"}}
        
        # 중복 메시지 필터링을 위한 변수
        self.last_processed_prices = {}  # {symbol: price}
        self.last_processed_times = {}   # {symbol: datetime}
        self.min_price_change_pct = 0.1  # 최소 가격 변동 비율 (0.1%)
        self.min_process_interval = 5    # 최소 처리 간격 (초)
        
        # 계좌 정보 관리 (독립적으로 관리)
        self.account_info = {}
        self.positions = {}
        self.cash_balance = 0
        
        logger.info("Envelope 트레이딩 모델 초기화 완료")
    
    async def start(self):
        """트레이딩 모델 시작"""
        if self.is_running:
            logger.warning("트레이딩 모델이 이미 실행 중입니다.")
            return
        
        self.is_running = True
        logger.info("Envelope 트레이딩 모델 시작")
        
        # 계좌 정보 초기 동기화
        if self.backend_client:
            account_info = await self.backend_client.request_account_info()
            if account_info:
                self.update_account_info(account_info)
                logger.info("계좌 정보 초기 동기화 완료")
        
        # 매매 신호 모니터링 시작
        asyncio.create_task(self.monitor_signals())
    
    async def stop(self):
        """트레이딩 모델 중지"""
        self.is_running = False
        logger.info("Envelope 트레이딩 모델 중지")
    
    async def refresh_indicators(self):
        """지표 갱신 (데이터 갱신 후 호출)"""
        logger.info("Envelope 지표 갱신")
        # 필요한 경우 캐시 비우기 또는 내부 상태 초기화
        # 현재는 구현이 필요하지 않음
    
    async def handle_realtime_price(self, symbol, price, indicators=None):
        """실시간 가격 데이터 처리 - 보유 종목에 대해서만 매도 신호 발생"""
        if not self.is_running:
            return
        
        try:
            # 중복 메시지 필터링 - 처리할 필요가 없으면 함수 종료
            if not self._should_process_price_update(symbol, price):
                return
            
            # Envelope 지표 가져오기 (캐시에서 직접 조회)
            envelope = None
            
            # 1. 전달받은 indicators 사용
            if indicators and 'envelope' in indicators:
                envelope = indicators['envelope']
            # 2. 또는 stock_cache에서 직접 조회
            elif self.stock_cache:
                envelope = self.stock_cache.get_envelope_indicators(symbol, price)
            
            if not envelope:
                logger.warning(f"종목 {symbol}에 대한 Envelope 지표가 없습니다")
                return
            
            # 밴드 정보 확인
            upper_band = envelope.get("upperBand", 0)
            middle_band = envelope.get("middleBand", 0)
            lower_band = envelope.get("lowerBand", 0)
            
            # 현재 시간
            now = datetime.now()
            
            # 이미 처리된 신호인지 확인
            last_trade = self.trade_history.get(symbol, {})
            last_action = last_trade.get("last_action", "")
            last_trade_time = last_trade.get("last_trade", None)
            
            # 계좌 정보에서 보유 종목 확인 - 매도 신호는 보유 종목에 대해서만 발생
            is_holding = symbol in self.positions
            
            # 상한선 (전량 매도 신호) - 보유 종목인 경우에만
            if price >= upper_band and is_holding:
                # 이미 상한선에서 전량 매도한 종목이면 신호 무시
                if last_action == "sell_all":
                    logger.debug(f"이미 상한선에서 매도한 종목: {symbol}, 신호 무시")
                    return
                
                self.trading_signals[symbol] = {
                    "signal": "upper",
                    "price": price,
                    "timestamp": now,
                    "bands": {
                        "upper": upper_band,
                        "middle": middle_band,
                        "lower": lower_band
                    }
                }
                logger.info(f"상한선 신호 발생 (보유 종목): {symbol}, 현재가: {price:.2f}, 상한선: {upper_band:.2f}")
            
            # 중앙선 (절반 매도 신호) - 이동평균선, 보유 종목인 경우에만
            elif price >= middle_band and price < upper_band and is_holding:
                # 이미 중앙선에서 절반 매도한 종목이거나 상한선에서 전량 매도한 종목이면 신호 무시
                if (last_action == "sell_half" or last_action == "sell_all"):
                    logger.debug(f"이미 중앙선 또는 상한선에서 매도한 종목: {symbol}, 신호 무시")
                    return
                
                self.trading_signals[symbol] = {
                    "signal": "middle",
                    "price": price,
                    "timestamp": now,
                    "bands": {
                        "upper": upper_band,
                        "middle": middle_band,
                        "lower": lower_band
                    }
                }
                logger.info(f"중앙선(이동평균선) 신호 발생 (보유 종목): {symbol}, 현재가: {price:.2f}, 중앙선: {middle_band:.2f}")
            
            # 하한선 (매수 신호) - 보유 여부 상관없음
            elif price <= lower_band:
                # 이미 보유 중인지 체크 - 중복 매수 방지
                if is_holding:
                    logger.debug(f"이미 보유 중인 종목: {symbol}, 매수 신호 무시")
                    return
                    
                # 이미 하한선에서 매수한 종목이면 신호 무시
                if last_action == "buy":
                    logger.debug(f"이미 하한선에서 매수한 종목: {symbol}, 신호 무시")
                    return
                
                self.trading_signals[symbol] = {
                    "signal": "lower",
                    "price": price,
                    "timestamp": now,
                    "bands": {
                        "upper": upper_band,
                        "middle": middle_band,
                        "lower": lower_band
                    }
                }
                logger.info(f"하한선 신호 발생: {symbol}, 현재가: {price:.2f}, 하한선: {lower_band:.2f}")
        except Exception as e:
            logger.error(f"실시간 가격 처리 중 오류: {str(e)}", exc_info=True)
    
    async def monitor_signals(self):
        """매매 신호 주기적 모니터링 및 처리"""
        logger.info("매매 신호 모니터링 시작")
        
        while self.is_running:
            try:
                # 1분마다 매매 신호 확인
                await asyncio.sleep(60)
                
                # 계좌 정보 동기화 (백엔드에서 가져옴)
                if self.backend_client:
                    account_info = await self.backend_client.request_account_info()
                    if account_info:
                        self.update_account_info(account_info)
                        logger.debug("계좌 정보 정기 동기화 완료")
                
                # 매매 신호 로깅
                if self.trading_signals:
                    signal_counts = {
                        "lower": sum(1 for v in self.trading_signals.values() if v["signal"] == "lower"),
                        "middle": sum(1 for v in self.trading_signals.values() if v["signal"] == "middle"),
                        "upper": sum(1 for v in self.trading_signals.values() if v["signal"] == "upper")
                    }
                    logger.info(f"현재 매매 신호: 하한선(매수)={signal_counts['lower']}개, " +
                              f"중앙선(절반매도)={signal_counts['middle']}개, " +
                              f"상한선(전량매도)={signal_counts['upper']}개")
                
                # 오래된 신호 제거 (10분 이상 경과)
                now = datetime.now()
                for symbol, signal_info in list(self.trading_signals.items()):
                    timestamp = signal_info["timestamp"]
                    if (now - timestamp).total_seconds() > 600:
                        del self.trading_signals[symbol]
                        logger.debug(f"종목 {symbol}의 오래된 신호 제거 (10분 경과)")
                
            except Exception as e:
                logger.error(f"매매 신호 모니터링 중 오류: {str(e)}", exc_info=True)
                await asyncio.sleep(30)
    
    async def get_trade_decisions(self, prices: Dict[str, float] = None) -> List[Dict[str, Any]]:
        """매매 의사결정 목록 반환"""
        if not self.is_running:
            logger.warning("트레이딩 모델이 실행 중이지 않습니다.")
            return []
        
        decisions = []
        try:
            # 계좌 정보 최신화 (백엔드에서 가져옴)
            if self.backend_client:
                account_info = await self.backend_client.request_account_info()
                if account_info:
                    self.update_account_info(account_info)
            
            # 계좌 정보 확인
            if not self.account_info:
                logger.warning("계좌 정보가 없습니다.")
                return []
            
            # 현재 시간
            now = datetime.now()
            
            # 매매 신호에 따른 거래 결정 처리
            for symbol, signal_info in list(self.trading_signals.items()):
                signal = signal_info["signal"]
                price = signal_info["price"]
                timestamp = signal_info["timestamp"]
                
                # 신호 유효 시간 (10분) 체크
                if (now - timestamp).total_seconds() > 600:
                    # 오래된 신호 제거
                    del self.trading_signals[symbol]
                    logger.debug(f"종목 {symbol}의 오래된 신호 제거 (10분 경과)")
                    continue
                
                # 1. 하한선 매수 신호 처리
                if signal == "lower":
                    # 현재 보유 종목 수 확인
                    if len(self.positions) >= self.max_positions:
                        logger.debug(f"최대 보유 종목 수({self.max_positions}) 도달, 매수 보류: {symbol}")
                        continue
                    
                    # 충분한 현금 확인
                    if self.cash_balance < self.trade_amount_per_stock:
                        logger.debug(f"현금 부족({self.cash_balance}), 매수 보류: {symbol}")
                        continue
                    
                    # 이미 보유 중인지 확인 (이중 체크)
                    if symbol in self.positions:
                        logger.debug(f"이미 보유 중인 종목 매수 신호 무시: {symbol}")
                        del self.trading_signals[symbol]
                        continue
                    
                    # 매수 수량 계산 (종목당 비중에 맞게)
                    current_price = price  # 직접 신호의 가격 사용
                    
                    if not current_price or current_price <= 0:
                        # 제공된 prices 딕셔너리에서 조회
                        if prices and symbol in prices:
                            current_price = prices[symbol]
                        # 또는 stock_cache에서 조회
                        elif self.stock_cache:
                            current_price = self.stock_cache.get_price(symbol)
                            
                        if not current_price or current_price <= 0:
                            logger.warning(f"종목 {symbol}의 가격 정보 없음, 매수 보류")
                            continue
                    
                    quantity = int(self.trade_amount_per_stock / current_price)
                    if quantity <= 0:
                        logger.warning(f"종목 {symbol}의 매수 수량이 0, 매수 보류")
                        continue
                    
                    # 매수 결정 추가
                    decision = {
                        "symbol": symbol,
                        "action": "buy",
                        "quantity": quantity,
                        "price": current_price,
                        "reason": "Envelope 하한선 터치 - 매수",
                        "timestamp": now.isoformat()
                    }
                    decisions.append(decision)
                    
                    # 거래 이력 업데이트
                    self.trade_history[symbol] = {
                        "last_trade": now,
                        "last_action": "buy"
                    }
                    
                    # 신호 제거
                    del self.trading_signals[symbol]
                    logger.info(f"매수 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}")
                
                # 2. 중앙선 절반 매도 신호 처리
                elif signal == "middle":
                    # 보유 중인 종목인지 확인 (이중 체크)
                    if symbol not in self.positions:
                        logger.debug(f"미보유 종목 매도 신호 무시: {symbol}")
                        del self.trading_signals[symbol]
                        continue
                    
                    position = self.positions[symbol]
                    total_quantity = position.get("quantity", 0)
                    
                    # 절반 수량 계산 (최소 1주)
                    sell_quantity = max(1, int(total_quantity / 2))
                    
                    if sell_quantity <= 0:
                        logger.warning(f"종목 {symbol}의 매도 수량이 0, 매도 보류")
                        continue
                    
                    # 매도 결정 추가
                    current_price = price  # 직접 신호의 가격 사용
                    
                    if not current_price or current_price <= 0:
                        # 제공된 prices 딕셔너리에서 조회
                        if prices and symbol in prices:
                            current_price = prices[symbol]
                        # 또는 stock_cache에서 조회
                        elif self.stock_cache:
                            current_price = self.stock_cache.get_price(symbol)
                            
                        if not current_price or current_price <= 0:
                            logger.warning(f"종목 {symbol}의 가격 정보 없음, 매도 보류")
                            continue
                            
                    decision = {
                        "symbol": symbol,
                        "action": "sell",
                        "quantity": sell_quantity,
                        "price": current_price,
                        "reason": "Envelope 중앙선 터치 - 절반 매도",
                        "timestamp": now.isoformat()
                    }
                    decisions.append(decision)
                    
                    # 거래 이력 업데이트
                    self.trade_history[symbol] = {
                        "last_trade": now,
                        "last_action": "sell_half"
                    }
                    
                    # 신호 제거
                    del self.trading_signals[symbol]
                    logger.info(f"절반 매도 결정: {symbol}, {sell_quantity}주, 가격: {current_price:.2f}")
                
                # 3. 상한선 전량 매도 신호 처리
                elif signal == "upper":
                    # 보유 중인 종목인지 확인 (이중 체크)
                    if symbol not in self.positions:
                        logger.debug(f"미보유 종목 매도 신호 무시: {symbol}")
                        del self.trading_signals[symbol]
                        continue
                    
                    position = self.positions[symbol]
                    total_quantity = position.get("quantity", 0)
                    
                    if total_quantity <= 0:
                        logger.warning(f"종목 {symbol}의 보유 수량이 0, 매도 보류")
                        continue
                    
                    # 매도 결정 추가 (전량)
                    current_price = price  # 직접 신호의 가격 사용
                    
                    if not current_price or current_price <= 0:
                        # 제공된 prices 딕셔너리에서 조회
                        if prices and symbol in prices:
                            current_price = prices[symbol]
                        # 또는 stock_cache에서 조회
                        elif self.stock_cache:
                            current_price = self.stock_cache.get_price(symbol)
                            
                        if not current_price or current_price <= 0:
                            logger.warning(f"종목 {symbol}의 가격 정보 없음, 매도 보류")
                            continue
                            
                    decision = {
                        "symbol": symbol,
                        "action": "sell",
                        "quantity": total_quantity,
                        "price": current_price,
                        "reason": "Envelope 상한선 터치 - 전량 매도",
                        "timestamp": now.isoformat()
                    }
                    decisions.append(decision)
                    
                    # 거래 이력 업데이트
                    self.trade_history[symbol] = {
                        "last_trade": now,
                        "last_action": "sell_all"
                    }
                    
                    # 신호 제거
                    del self.trading_signals[symbol]
                    logger.info(f"전량 매도 결정: {symbol}, {total_quantity}주, 가격: {current_price:.2f}")
            
            return decisions
        
        except Exception as e:
            logger.error(f"매매 의사결정 생성 중 오류: {str(e)}", exc_info=True)
            return []