import logging
import asyncio
from typing import Dict, List, Any
from datetime import datetime
from app.strategies.base import BaseTradingModel

logger = logging.getLogger(__name__)

class BollingerBandTradingModel(BaseTradingModel):
    """볼린저 밴드 전략 기반 AI 트레이딩 모델"""
    
    def __init__(self, stock_cache=None):
        """볼린저 밴드 전략 트레이딩 모델 초기화"""
        super().__init__(stock_cache)
        
        # 매매 관련 설정
        self.max_positions = 15  # 최대 보유 종목 수 (Envelope 모델보다 적게 설정)
        self.trade_amount_per_stock = 6000000  # 종목당 매매 금액 (600만원)
        self.min_holding_period = 1  # 최소 보유 기간 (일)
        
        # 볼린저 밴드 설정 (StockCache와 동일하게 유지)
        self.bb_period = 26  # 기간 (26일)
        self.bb_std_dev = 2.0  # 표준편차 승수 (2)
        
        # 실행 상태
        self.is_running = False
        
        # 매매 신호 저장 딕셔너리
        self.trading_signals = {}  # {symbol: {"signal": "buy", "price": price, "timestamp": datetime}}
        
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
        
        logger.info("볼린저 밴드 트레이딩 모델 초기화 완료")
    
    async def start(self):
        """트레이딩 모델 시작"""
        if self.is_running:
            logger.warning("볼린저 밴드 트레이딩 모델이 이미 실행 중입니다.")
            return
        
        self.is_running = True
        logger.info("볼린저 밴드 트레이딩 모델 시작")
        
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
        logger.info("볼린저 밴드 트레이딩 모델 중지")
    
    async def refresh_indicators(self):
        """지표 갱신 (데이터 갱신 후 호출)"""
        logger.info("볼린저 밴드 지표 갱신")
        # 필요한 경우 캐시 비우기 또는 내부 상태 초기화
        # 현재는 구현이 필요하지 않음
    
    async def handle_realtime_price(self, symbol, price, indicators=None):
        """실시간 가격 데이터 처리"""
        if not self.is_running:
            return
        
        try:
            # 중복 메시지 필터링 - 처리할 필요가 없으면 함수 종료
            if not self._should_process_price_update(symbol, price):
                return
            
            # 볼린저 밴드 지표 가져오기 (캐시에서 직접 조회)
            bb_indicators = None
            
            # 1. 전달받은 indicators 사용
            if indicators and 'bollinger_bands' in indicators:
                bb_indicators = indicators['bollinger_bands']
            # 2. 또는 stock_cache에서 직접 조회
            elif self.stock_cache:
                bb_indicators = self.stock_cache.get_bollinger_bands(symbol, price)
            
            if not bb_indicators:
                logger.warning(f"종목 {symbol}에 대한 볼린저 밴드 지표가 없습니다")
                return
            
            # 밴드 정보 확인
            upper_band = bb_indicators.get("upperBand", 0)
            middle_band = bb_indicators.get("middleBand", 0)
            lower_band = bb_indicators.get("lowerBand", 0)
            percent_b = bb_indicators.get("percentB", 0.5)
            used_period = bb_indicators.get("period", 0)
            
            # 충분한 데이터로 계산된 지표인지 검증 (최소 기간의 70% 이상 확보되었는지)
            if used_period < (self.bb_period * 0.7):
                logger.warning(f"종목 {symbol}의 볼린저 밴드 지표 데이터 부족: {used_period}/{self.bb_period}일 (70% 미만)")
                # 데이터가 충분하지 않으면 신호 생성하지 않음
                return
            
            # 현재 시간
            now = datetime.now()
            
            # 이미 처리된 신호인지 확인
            last_trade = self.trade_history.get(symbol, {})
            last_action = last_trade.get("last_action", "")
            last_trade_time = last_trade.get("last_trade", None)
            
            # 마지막 거래 이후 최소 보유 기간(일)이 지났는지 확인
            min_holding_passed = True
            if last_trade_time:
                seconds_passed = (now - last_trade_time).total_seconds()
                min_holding_passed = seconds_passed >= (self.min_holding_period * 86400)
            
            # 계좌 정보에서 보유 종목 확인 - 독립적인 positions 사용
            is_holding = symbol in self.positions
            
            # 볼린저 밴드 기반 매매 신호 로직
            # 1. 매수 신호: %B가 0.1 이하 (하단 밴드 아래거나 근처)
            if percent_b <= 0.1 and not is_holding:
                # 이미 매수한 종목이면 신호 무시
                if last_action == "buy" and not min_holding_passed:
                    logger.debug(f"이미 매수한 종목: {symbol}, 신호 무시")
                    return
                
                self.trading_signals[symbol] = {
                    "signal": "buy",
                    "price": price,
                    "timestamp": now,
                    "bands": {
                        "upper": upper_band,
                        "middle": middle_band,
                        "lower": lower_band,
                        "percentB": percent_b
                    }
                }
                logger.info(f"볼린저 밴드 매수 신호 발생: {symbol}, 현재가: {price:.2f}, %B: {percent_b:.2f}")
            
            # 2. 매도 신호: %B가 0.9 이상 (상단 밴드 위거나 근처)
            elif percent_b >= 0.9 and is_holding:
                # 이미 매도한 종목이면 신호 무시
                if last_action == "sell" and not min_holding_passed:
                    logger.debug(f"이미 매도한 종목: {symbol}, 신호 무시")
                    return
                
                self.trading_signals[symbol] = {
                    "signal": "sell",
                    "price": price,
                    "timestamp": now,
                    "bands": {
                        "upper": upper_band,
                        "middle": middle_band,
                        "lower": lower_band,
                        "percentB": percent_b
                    }
                }
                logger.info(f"볼린저 밴드 매도 신호 발생: {symbol}, 현재가: {price:.2f}, %B: {percent_b:.2f}")
            
            # 3. 손절 신호: 보유 중인 종목이 이동평균 아래로 15% 이상 하락
            elif is_holding and price < middle_band * 0.85:
                # 손절 신호는 최소 보유 기간과 상관없이 항상 발생
                position = self.positions[symbol]
                avg_price = position.get("avgPrice", 0)
                
                # 매수가 대비 10% 이상 손실인 경우에만 손절
                if avg_price > 0 and price < avg_price * 0.9:
                    self.trading_signals[symbol] = {
                        "signal": "stop_loss",
                        "price": price,
                        "timestamp": now,
                        "bands": {
                            "upper": upper_band,
                            "middle": middle_band,
                            "lower": lower_band,
                            "percentB": percent_b
                        }
                    }
                    logger.info(f"볼린저 밴드 손절 신호 발생: {symbol}, 현재가: {price:.2f}, 매수가: {avg_price:.2f}")
        
        except Exception as e:
            logger.error(f"실시간 가격 처리 중 오류: {str(e)}", exc_info=True)
    
    async def monitor_signals(self):
        """매매 신호 주기적 모니터링 및 처리"""
        logger.info("볼린저 밴드 매매 신호 모니터링 시작")
        
        last_recalc_time = datetime.now()
        
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
                        "buy": sum(1 for v in self.trading_signals.values() if v["signal"] == "buy"),
                        "sell": sum(1 for v in self.trading_signals.values() if v["signal"] == "sell"),
                        "stop_loss": sum(1 for v in self.trading_signals.values() if v["signal"] == "stop_loss")
                    }
                    logger.info(f"현재 볼린저 밴드 매매 신호: 매수={signal_counts['buy']}개, " +
                              f"매도={signal_counts['sell']}개, " +
                              f"손절={signal_counts['stop_loss']}개")
                
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
            logger.warning("볼린저 밴드 트레이딩 모델이 실행 중이지 않습니다.")
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
                
                # 1. 매수 신호 처리
                if signal == "buy":
                    # 현재 보유 종목 수 확인
                    if len(self.positions) >= self.max_positions:
                        logger.debug(f"최대 보유 종목 수({self.max_positions}) 도달, 매수 보류: {symbol}")
                        continue
                    
                    # 충분한 현금 확인
                    if self.cash_balance < self.trade_amount_per_stock:
                        logger.debug(f"현금 부족({self.cash_balance}), 매수 보류: {symbol}")
                        continue
                    
                    # 이미 보유 중인지 확인
                    if symbol in self.positions:
                        logger.debug(f"이미 보유 중인 종목 매수 신호 무시: {symbol}")
                        del self.trading_signals[symbol]
                        continue
                    
                    # 매수 수량 계산
                    current_price = price
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
                        "reason": "볼린저 밴드 하단 터치 - 매수",
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
                
                # 2. 매도 신호 처리
                elif signal in ["sell", "stop_loss"]:
                    # 보유 중인 종목인지 확인
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
                    current_price = price
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
                    
                    reason = "볼린저 밴드 상단 터치 - 매도" if signal == "sell" else "손절 매도"
                    
                    decision = {
                        "symbol": symbol,
                        "action": "sell",
                        "quantity": total_quantity,
                        "price": current_price,
                        "reason": reason,
                        "timestamp": now.isoformat()
                    }
                    decisions.append(decision)
                    
                    # 거래 이력 업데이트
                    self.trade_history[symbol] = {
                        "last_trade": now,
                        "last_action": "sell"
                    }
                    
                    # 신호 제거
                    del self.trading_signals[symbol]
                    logger.info(f"매도 결정: {symbol}, {total_quantity}주, 가격: {current_price:.2f}, 이유: {reason}")
            
            return decisions
        
        except Exception as e:
            logger.error(f"매매 의사결정 생성 중 오류: {str(e)}", exc_info=True)
            return []