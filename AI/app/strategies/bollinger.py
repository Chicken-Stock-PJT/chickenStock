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
        self.max_positions = 20  # 최대 보유 종목 수 (Envelope 모델보다 적게 설정)
        self.trade_amount_per_stock = 6000000  # 종목당 매매 금액 (600만원)
        
        # 분할 매수 설정
        self.split_purchase_count = 3  # 분할 매수 횟수
        self.split_purchase_percentages = [0.4, 0.3, 0.3]  # 분할 매수 비율
        self.split_purchase_tracking = {}  # 분할 매수 추적 {symbol: {"current_step": 1, "purchases": [{"price": price, "quantity": qty, "timestamp": datetime}]}}
        
        # 볼린저 밴드 설정 (StockCache와 동일하게 유지)
        self.bb_period = 26  # 기간 (26일)
        self.bb_std_dev = 2.0  # 표준편차 승수 (2)
        
        # 보수적 매수 신호 설정
        self.buy_percentB_threshold = 0.05  # 더 낮은 %B 기준값 (0.1 -> 0.05)
        self.min_confidence_threshold = 0.51  # 높은 신뢰도 기준 (0.5 -> 0.6)
        self.buy_cooldown_hours = 0  # 매수 신호 쿨다운 시간 (시간)
        
        # 실행 상태
        self.is_running = False
        
        # 매매 신호 저장 딕셔너리
        self.trading_signals = {}  # {symbol: {"signal": "buy", "price": price, "timestamp": datetime, "confidence": float}}
        
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
    
    def _calculate_signal_confidence(self, symbol, price, bb_indicators):
        """매매 신호의 신뢰도 계산 (거래량 제외한 간소화 버전)
        
        Parameters:
        - symbol: 종목 코드
        - price: 현재가
        - bb_indicators: 볼린저 밴드 지표 데이터
        
        Returns:
        - float: 0.0 ~ 1.0 사이의 신뢰도 점수
        """
        # 기본 신뢰도 점수
        confidence = 0.5
        
        # 1. 볼린저 밴드 관련 지표 확인
        upper_band = bb_indicators.get("upperBand", 0)
        middle_band = bb_indicators.get("middleBand", 0)
        lower_band = bb_indicators.get("lowerBand", 0)
        percent_b = bb_indicators.get("percentB", 0.5)
        used_period = bb_indicators.get("period", 0)
        band_width = (upper_band - lower_band) / middle_band if middle_band > 0 else 0  # 밴드 폭 계산
        
        # 2. 기본 신뢰도 요소: 사용된 데이터 기간의 충분성
        period_confidence = min(1.0, used_period / self.bb_period)
        confidence *= period_confidence
        
        # 3. %B 기반 신뢰도 계산 (매수/매도 신호에 따라 다름)
        signal_type = "buy" if percent_b <= self.buy_percentB_threshold else "sell" if percent_b >= 0.9 else "none"
        
        if signal_type == "buy":
            # 매수 신호: %B가 0에 가까울수록 신뢰도 증가 (더 엄격한 기준으로 계산)
            pb_confidence = 1.0 - (percent_b / self.buy_percentB_threshold)
            pb_confidence = max(0.0, min(1.0, pb_confidence))
            
            # 가격이 밴드 하단보다 얼마나 낮은지 확인 (밴드 하단보다 낮을수록 신뢰도↑)
            band_position = (lower_band - price) / lower_band if lower_band > 0 else 0
            band_position = max(0.0, min(0.2, band_position)) * 5  # 0~0.2 범위를 0~1로 정규화
            
            # 밴드 폭이 넓을수록 (변동성이 클수록) 신뢰도 가중
            band_width_factor = min(1.0, band_width / 0.05) if band_width > 0 else 0.5
            
            # 보수적인 매수 신호를 위한 추가 조건
            # 가격이 지속적으로 하락하고 있는지 확인 (밴드의 중앙값이 하락 추세인 경우 신뢰도 하향)
            trend_factor = 0.7  # 하락 추세시 신뢰도 감소 (기본값)
            
            # 최종 매수 신호 신뢰도 (단순 가중 평균)
            confidence = ((confidence * 0.2) + (pb_confidence * 0.5) + (band_position * 0.2) + (band_width_factor * 0.1)) * trend_factor
            
        elif signal_type == "sell":
            # 매도 신호: %B가 1에 가까울수록 신뢰도 증가 (0.9 기준)
            pb_confidence = (percent_b - 0.9) / 0.1
            pb_confidence = max(0.0, min(1.0, pb_confidence))
            
            # 가격이 밴드 상단보다 얼마나 높은지 확인 (밴드 상단보다 높을수록 신뢰도↑)
            band_position = (price - upper_band) / upper_band if upper_band > 0 else 0
            band_position = max(0.0, min(0.2, band_position)) * 5  # 0~0.2 범위를 0~1로 정규화
            
            # 밴드 폭이 넓을수록 (변동성이 클수록) 신뢰도 가중
            band_width_factor = min(1.0, band_width / 0.05) if band_width > 0 else 0.5
            
            # 최종 매도 신호 신뢰도 (단순 가중 평균)
            confidence = (confidence * 0.2) + (pb_confidence * 0.5) + (band_position * 0.2) + (band_width_factor * 0.1)
        
        # 최종 신뢰도 점수 반환 (0.0 ~ 1.0 범위로 클램핑)
        return max(0.0, min(1.0, confidence))
    
    def _should_process_price_update(self, symbol, price):
        """실시간 가격 업데이트를 처리할지 결정"""
        now = datetime.now()
        
        # 이전 처리 가격 및 시간
        last_price = self.last_processed_prices.get(symbol, 0)
        last_time = self.last_processed_times.get(symbol, datetime.min)
        
        # 최소 처리 간격 체크
        if (now - last_time).total_seconds() < self.min_process_interval:
            return False
        
        # 최소 가격 변동 체크 (가격이 0이 아닌 경우만)
        if last_price > 0 and price > 0:
            price_change_pct = abs(price - last_price) / last_price * 100
            if price_change_pct < self.min_price_change_pct:
                return False
        
        # 처리 결정: 가격 및 시간 업데이트
        self.last_processed_prices[symbol] = price
        self.last_processed_times[symbol] = now
        return True
    
    def _check_buy_cooldown(self, symbol):
        """매수 쿨다운 기간 체크 (True: 매수 가능, False: 쿨다운 중)"""
        now = datetime.now()
        last_trade = self.trade_history.get(symbol, {})
        last_trade_time = last_trade.get("last_trade", None)
        
        # 마지막 거래가 없으면 매수 가능
        if not last_trade_time:
            return True
        
        # 쿨다운 시간 체크
        elapsed_hours = (now - last_trade_time).total_seconds() / 3600
        return elapsed_hours >= self.buy_cooldown_hours
    
    def _can_make_split_purchase(self, symbol, price):
        """분할 매수 가능 여부 확인 - 가격 하락 조건 추가"""
        # 분할 매수 추적 정보가 없으면 첫 번째 매수 가능
        if symbol not in self.split_purchase_tracking:
            return True
        
        # 현재 분할 매수 단계가 최대 단계보다 작은지 확인
        tracking_info = self.split_purchase_tracking[symbol]
        if tracking_info["current_step"] >= self.split_purchase_count:
            return False
        
        # 이전 매수가 대비 하락률 확인
        last_purchase = tracking_info["purchases"][-1]
        last_price = last_purchase["price"]
        
        # 단계별 하락률 설정 (단계가 늘어날수록 요구 하락률 증가)
        required_drop_rates = [0.03, 0.03, 0.02]
        current_step = tracking_info["current_step"]
        required_drop = required_drop_rates[min(current_step-1, len(required_drop_rates)-1)]
        
        # 이전 매수가 대비 충분히 하락했는지 확인
        price_drop_rate = (last_price - price) / last_price
        return price_drop_rate >= required_drop
    
    def _calculate_split_purchase_amount(self, symbol):
        """다음 분할 매수 금액 계산"""
        # 분할 매수 추적 정보가 없으면 첫 번째 매수액 계산
        if symbol not in self.split_purchase_tracking:
            return self.trade_amount_per_stock * self.split_purchase_percentages[0]
        
        # 다음 매수 단계 계산
        tracking_info = self.split_purchase_tracking[symbol]
        next_step = tracking_info["current_step"]
        
        # 매수 단계가 유효한지 확인
        if next_step >= self.split_purchase_count:
            logger.warning(f"종목 {symbol}의 분할 매수 완료 (최대 {self.split_purchase_count}회)")
            return 0
        
        # 다음 매수 금액 계산
        return self.trade_amount_per_stock * self.split_purchase_percentages[next_step]
    
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
            
            # 계좌 정보에서 보유 종목 확인
            is_holding = symbol in self.positions
            
            # 분할 매수 가능 여부 확인
            can_make_split_purchase = self._can_make_split_purchase(symbol, price)
            
            # 매매 신호 신뢰도 계산
            signal_confidence = 0.0
            
            # 볼린저 밴드 기반 매매 신호 로직
            # 1. 매수 신호: %B가 임계값 이하 (기존 0.1 -> 0.05, 더 엄격한 조건)
            if percent_b <= self.buy_percentB_threshold and (is_holding and can_make_split_purchase or not is_holding):
                # 매수 쿨다운 체크
                if not self._check_buy_cooldown(symbol):
                    logger.debug(f"쿨다운 기간 중인 종목: {symbol}, 매수 신호 무시")
                    return
                
                # 매수 신호 신뢰도 계산 (보수적인 계산)
                signal_confidence = self._calculate_signal_confidence(symbol, price, bb_indicators)
                
                # 최소 신뢰도 이상인 경우만 신호 등록 (더 높은 임계값)
                if signal_confidence >= self.min_confidence_threshold:
                    self.trading_signals[symbol] = {
                        "signal": "buy",
                        "price": price,
                        "timestamp": now,
                        "bands": {
                            "upper": upper_band,
                            "middle": middle_band,
                            "lower": lower_band,
                            "percentB": percent_b
                        },
                        "confidence": signal_confidence,
                        "is_split_purchase": is_holding and can_make_split_purchase
                    }
                    logger.info(f"볼린저 밴드 매수 신호 발생: {symbol}, 현재가: {price:.2f}, %B: {percent_b:.2f}, 신뢰도: {signal_confidence:.2f}, 분할매수: {'예' if is_holding and can_make_split_purchase else '아니오'}")
                else:
                    logger.debug(f"볼린저 밴드 매수 신호 무시 (낮은 신뢰도): {symbol}, 신뢰도: {signal_confidence:.2f}")
            
            # 2. 매도 신호: %B가 0.9 이상 (상단 밴드 위거나 근처)
            elif percent_b >= 0.9 and is_holding:
                # 매도 신호 신뢰도 계산
                signal_confidence = self._calculate_signal_confidence(symbol, price, bb_indicators)
                
                # 최소 신뢰도 이상인 경우만 신호 등록 (매도는 좀 더 낮게 설정)
                if signal_confidence >= (self.min_confidence_threshold * 0.8):
                    self.trading_signals[symbol] = {
                        "signal": "sell",
                        "price": price,
                        "timestamp": now,
                        "bands": {
                            "upper": upper_band,
                            "middle": middle_band,
                            "lower": lower_band,
                            "percentB": percent_b
                        },
                        "confidence": signal_confidence
                    }
                    logger.info(f"볼린저 밴드 매도 신호 발생: {symbol}, 현재가: {price:.2f}, %B: {percent_b:.2f}, 신뢰도: {signal_confidence:.2f}")
                else:
                    logger.debug(f"볼린저 밴드 매도 신호 무시 (낮은 신뢰도): {symbol}, 신뢰도: {signal_confidence:.2f}")
            
            # 3. 손절 신호: 보유 중인 종목이 이동평균 아래로 15% 이상 하락
            elif is_holding and price < middle_band * 0.85:
                # 손절 신호는 최소 보유 기간과 상관없이 항상 발생
                position = self.positions[symbol]
                avg_price = position.get("avgPrice", 0)
                
                # 매수가 대비 10% 이상 손실인 경우에만 손절
                if avg_price > 0 and price < avg_price * 0.9:
                    # 손절 신호 신뢰도는 손실 크기에 비례
                    loss_pct = 1.0 - (price / avg_price)
                    signal_confidence = min(1.0, loss_pct * 5)  # 20% 손실이면 신뢰도 1.0
                    
                    self.trading_signals[symbol] = {
                        "signal": "stop_loss",
                        "price": price,
                        "timestamp": now,
                        "bands": {
                            "upper": upper_band,
                            "middle": middle_band,
                            "lower": lower_band,
                            "percentB": percent_b
                        },
                        "confidence": signal_confidence
                    }
                    logger.info(f"볼린저 밴드 손절 신호 발생: {symbol}, 현재가: {price:.2f}, 매수가: {avg_price:.2f}, 신뢰도: {signal_confidence:.2f}")
        
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
                    
                    # 신뢰도 평균 계산
                    avg_confidence = {
                        "buy": sum(v["confidence"] for v in self.trading_signals.values() if v["signal"] == "buy") / max(1, signal_counts["buy"]),
                        "sell": sum(v["confidence"] for v in self.trading_signals.values() if v["signal"] == "sell") / max(1, signal_counts["sell"]),
                        "stop_loss": sum(v["confidence"] for v in self.trading_signals.values() if v["signal"] == "stop_loss") / max(1, signal_counts["stop_loss"])
                    }
                    
                    logger.info(f"현재 볼린저 밴드 매매 신호: 매수={signal_counts['buy']}개(신뢰도 {avg_confidence['buy']:.2f}), " +
                              f"매도={signal_counts['sell']}개(신뢰도 {avg_confidence['sell']:.2f}), " +
                              f"손절={signal_counts['stop_loss']}개(신뢰도 {avg_confidence['stop_loss']:.2f})")
                
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
            
            # 매매 신호 목록 (신뢰도 기준 정렬)
            buy_signals = []
            sell_signals = []
            
            # 신호 분류 및 정렬 준비
            for symbol, signal_info in list(self.trading_signals.items()):
                signal = signal_info["signal"]
                
                # 신호 유효 시간 (10분) 체크
                timestamp = signal_info["timestamp"]
                if (now - timestamp).total_seconds() > 600:
                    # 오래된 신호 제거
                    del self.trading_signals[symbol]
                    logger.debug(f"종목 {symbol}의 오래된 신호 제거 (10분 경과)")
                    continue
                
                # 시그널 타입별로 분류
                if signal == "buy":
                    buy_signals.append((symbol, signal_info))
                elif signal in ["sell", "stop_loss"]:
                    sell_signals.append((symbol, signal_info))
            
            # 1. 매도/손절 신호 먼저 처리 (자본 확보를 위해)
            for symbol, signal_info in sell_signals:
                signal = signal_info["signal"]
                price = signal_info["price"]
                
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
                confidence = signal_info.get("confidence", 0.0)
                
                decision = {
                    "symbol": symbol,
                    "action": "sell",
                    "quantity": total_quantity,
                    "price": current_price,
                    "reason": f"{reason} (신뢰도: {confidence:.2f})",
                    "confidence": confidence,
                    "timestamp": now.isoformat()
                }
                decisions.append(decision)
                
                # 거래 이력 업데이트
                self.trade_history[symbol] = {
                    "last_trade": now,
                    "last_action": "sell"
                }
                
                # 분할 매수 추적 정보 초기화
                if symbol in self.split_purchase_tracking:
                    del self.split_purchase_tracking[symbol]
                
                # 신호 제거
                del self.trading_signals[symbol]
                logger.info(f"매도 결정: {symbol}, {total_quantity}주, 가격: {current_price:.2f}, 이유: {reason}, 신뢰도: {confidence:.2f}")
            
            # 2. 매수 신호 처리 (신뢰도 기준 정렬)
            # 신뢰도 기준 내림차순 정렬
            buy_signals.sort(key=lambda x: x[1].get("confidence", 0.0), reverse=True)
            
            for symbol, signal_info in buy_signals:
                # 분할 매수 여부 확인
                is_split_purchase = signal_info.get("is_split_purchase", False)
                
                # 현재 보유 종목 수 확인 (분할 매수는 이미 보유 중이므로 제외)
                if len(self.positions) >= self.max_positions and not is_split_purchase:
                    logger.debug(f"최대 보유 종목 수({self.max_positions}) 도달, 매수 보류: {symbol}")
                    continue
                
                # 매수 금액 계산 (분할 매수의 경우 다음 단계 금액 계산)
                purchase_amount = self._calculate_split_purchase_amount(symbol)
                if purchase_amount <= 0:
                    logger.debug(f"종목 {symbol}의 매수 금액이 0, 매수 보류")
                    continue
                
                # 충분한 현금 확인
                if self.cash_balance < purchase_amount:
                    logger.debug(f"현금 부족({self.cash_balance}), 매수 보류: {symbol}")
                    continue
                
                # 매수 수량 계산
                price = signal_info["price"]
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
                
                quantity = int(purchase_amount / current_price)
                if quantity <= 0:
                    logger.warning(f"종목 {symbol}의 매수 수량이 0, 매수 보류")
                    continue
                
                # 신뢰도 정보 가져오기
                confidence = signal_info.get("confidence", 0.0)
                
                # 분할 매수 추적 정보 업데이트
                next_step = 0
                if symbol in self.split_purchase_tracking:
                    tracking_info = self.split_purchase_tracking[symbol]
                    next_step = tracking_info["current_step"]
                    tracking_info["current_step"] += 1
                    tracking_info["purchases"].append({
                        "price": current_price,
                        "quantity": quantity,
                        "timestamp": now
                    })
                else:
                    self.split_purchase_tracking[symbol] = {
                        "current_step": 1,
                        "purchases": [{
                            "price": current_price,
                            "quantity": quantity,
                            "timestamp": now
                        }]
                    }
                
                # 분할 매수 단계에 따른 메시지
                purchase_msg = ""
                if is_split_purchase:
                    next_step += 1  # 표시용 (1부터 시작)
                    purchase_msg = f"분할매수 {next_step}/{self.split_purchase_count}"
                
                # 매수 결정 추가
                decision = {
                    "symbol": symbol,
                    "action": "buy",
                    "quantity": quantity,
                    "price": current_price,
                    "reason": f"볼린저 밴드 하단 터치 - 매수 {purchase_msg} (신뢰도: {confidence:.2f})",
                    "confidence": confidence,
                    "timestamp": now.isoformat(),
                    "is_split_purchase": is_split_purchase,
                    "split_step": next_step if is_split_purchase else 0
                }
                decisions.append(decision)
                
                # 거래 이력 업데이트
                self.trade_history[symbol] = {
                    "last_trade": now,
                    "last_action": "buy"
                }
                
                # 신호 제거
                del self.trading_signals[symbol]
                logger.info(f"매수 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}, {purchase_msg}, 신뢰도: {confidence:.2f}")
                
                # 자금 고갈 시 종료
                self.cash_balance -= (quantity * current_price)
                if self.cash_balance < self._calculate_split_purchase_amount("MIN_AMOUNT"):
                    logger.debug(f"가용 자금 소진, 매수 신호 처리 중단 (잔액: {self.cash_balance})")
                    break
            
            return decisions
        
        except Exception as e:
            logger.error(f"매매 의사결정 생성 중 오류: {str(e)}", exc_info=True)
            return []