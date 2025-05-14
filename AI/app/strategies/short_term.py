import asyncio
import logging
from typing import Dict, List, Any
from datetime import datetime, timedelta
import numpy as np
import pandas as pd

logger = logging.getLogger(__name__)

class ShortTermTradingModel:
    """
    단타 매매 알고리즘 모델
    거래량 증가와 눌림목 패턴을 활용한 단타 전략 구현
    """
    
    def __init__(self, stock_cache=None, backend_client=None):
        """단타 매매 모델 초기화"""
        self.stock_cache = stock_cache
        self.backend_client = backend_client
        self.is_running = False
        
        # 매매 관련 설정
        self.max_positions = 5  # 최대 보유 종목 수
        self.trade_amount_per_stock = 1000000  # 종목당 매매 금액 (100만원)
        self.min_holding_period = 0.5  # 최소 보유 기간 (일) - 반나절
        
        # 매매 신호 저장 딕셔너리
        self.trading_signals = {}  # {symbol: {"signal": "buy/sell", "price": price, "timestamp": datetime}}
        
        # 거래 이력 저장
        self.trade_history = {}  # {symbol: {"last_trade": datetime, "last_action": "buy"}}
        
        # 중복 메시지 필터링을 위한 변수
        self.last_processed_prices = {}  # {symbol: price}
        self.last_processed_times = {}   # {symbol: datetime}
        self.min_price_change_pct = 0.1  # 최소 가격 변동 비율 (0.1%)
        self.min_process_interval = 5    # 최소 처리 간격 (초)
        
        # 계좌 정보 관리
        self.account_info = {}
        self.positions = {}
        self.cash_balance = 0
        
        # 거래량 관련 매개변수
        self.volume_surge_threshold = 500  # 평균 거래량 대비 최소 증가율 (%)
        self.min_price_increase = 3.0     # 거래량 급증 시 최소 가격 상승률 (%)
        
        # 눌림목 패턴 관련 매개변수
        self.pullback_threshold = 0.5     # 눌림목 판단 기준 (%)
        
        # 손절 관련 매개변수
        self.stop_loss_pct = 2.0          # 손절 기준 (%)
        
        logger.info("단타 매매 모델 초기화 완료")
    
    async def start(self):
        """트레이딩 모델 시작"""
        if self.is_running:
            logger.warning("트레이딩 모델이 이미 실행 중입니다.")
            return
        
        self.is_running = True
        logger.info("단타 매매 모델 시작")
        
        # 계좌 정보 초기 동기화
        if self.backend_client:
            account_info = await self.backend_client.request_account_info()
            if account_info:
                self.update_account_info(account_info)
                logger.info("계좌 정보 초기 동기화 완료")
        
        # 매매 신호 모니터링 시작
        asyncio.create_task(self.monitor_signals())
        
        # 거래대금 상위 종목 모니터링 시작
        asyncio.create_task(self.monitor_top_volume_stocks())
    
    async def stop(self):
        """트레이딩 모델 중지"""
        self.is_running = False
        logger.info("단타 매매 모델 중지")
    
    async def refresh_indicators(self):
        """지표 갱신 (데이터 갱신 후 호출)"""
        logger.info("단타 매매 지표 갱신")
        refreshed_count = 0
        
        if not self.stock_cache:
            logger.warning("StockCache가 설정되지 않아 지표를 갱신할 수 없습니다.")
            return refreshed_count
        
        try:
            # 필터링된 종목 목록 가져오기
            filtered_stocks = self.get_filtered_stocks()
            if not filtered_stocks:
                logger.warning("필터링된 종목이 없어 지표를 갱신할 수 없습니다.")
                return refreshed_count
            
            # 각 종목에 대해 5분봉 데이터 캐시 갱신
            for symbol in filtered_stocks:
                try:
                    # 5분봉 데이터 가져오기
                    minute_candles = self._get_minute_candles(symbol)
                    if minute_candles and len(minute_candles) >= 10:
                        # 캐시에 저장
                        self.minute_candle_cache[symbol] = minute_candles
                        self.last_minute_candle_update[symbol] = datetime.now()
                        refreshed_count += 1
                except Exception as e:
                    logger.error(f"종목 {symbol}의 5분봉 데이터 갱신 중 오류: {str(e)}")
            
            logger.info(f"단타 매매 지표 갱신 완료: {refreshed_count}개 종목")
            return refreshed_count
            
        except Exception as e:
            logger.error(f"단타 매매 지표 갱신 중 오류: {str(e)}")
            return refreshed_count
    
    def get_filtered_stocks(self) -> List[str]:
        """필터링된 종목 목록 가져오기"""
        if self.stock_cache and hasattr(self.stock_cache, "get_filtered_stocks"):
            return self.stock_cache.get_filtered_stocks()
        elif self.stock_cache and hasattr(self.stock_cache, "filtered_stockcode_list"):
            return self.stock_cache.filtered_stockcode_list
        return []
    
    def _get_minute_candles(self, symbol: str) -> List[Dict]:
        """
        종목의 5분봉 데이터 가져오기
        StockCache 또는 BotStockCache에서 데이터를 가져온다
        """
        if not self.stock_cache:
            return []
        
        # 1. StockCache의 get_minute_chart_data 메서드 사용 (권장)
        if hasattr(self.stock_cache, "get_minute_chart_data"):
            return self.stock_cache.get_minute_chart_data(symbol, 5)
        
        # 2. BotStockCache의 get_minute_candles 메서드 사용 (대체)
        elif hasattr(self.stock_cache, "get_minute_candles"):
            return self.stock_cache.get_minute_candles(symbol, 5)
        
        # 3. 없으면 빈 리스트 반환
        return []
    
    async def handle_realtime_price(self, symbol: str, price: float, indicators=None):
        """
        실시간 가격 데이터 처리
        indicators: 전략 지표 (딕셔너리 형태)
        """
        if not self.is_running:
            return
        
        try:
            # 중복 메시지 필터링
            if not self._should_process_price_update(symbol, price):
                return
            
            # 단타 매매 지표 가져오기
            short_term_indicators = None
            if indicators and 'short_term' in indicators:
                short_term_indicators = indicators['short_term']
            
            # 분봉 데이터 확인
            candle_data = None
            
            # 1. 캐시에서 5분봉 데이터 가져오기
            if symbol in self.minute_candle_cache:
                last_update = self.last_minute_candle_update.get(symbol, datetime.min)
                if (datetime.now() - last_update).total_seconds() < 300:  # 5분 이내 갱신된 데이터면 사용
                    candle_data = self.minute_candle_cache[symbol]
            
            # 2. 캐시에 없으면 StockCache에서 가져오기
            if not candle_data or len(candle_data) < 10:
                candle_data = self._get_minute_candles(symbol)
                if candle_data and len(candle_data) >= 10:
                    # 캐시에 저장
                    self.minute_candle_cache[symbol] = candle_data
                    self.last_minute_candle_update[symbol] = datetime.now()
            
            # 분봉 데이터가 없으면 처리 중단
            if not candle_data or len(candle_data) < 10:
                return
            
            # 현재 시간
            now = datetime.now()
            
            # 이미 short_term_indicators가 전달되었으면 사용
            if short_term_indicators:
                signal = short_term_indicators.get("signal", "중립")
                
                if signal == "매수":
                    # 매수 신호 처리
                    self._handle_buy_signal(symbol, price, "단타 매매 지표 매수 신호", now)
                
                elif signal == "매도" and symbol in self.positions:
                    # 매도 신호 처리
                    self._handle_sell_signal(symbol, price, "단타 매매 지표 매도 신호", now)
                
                return
            
            # short_term_indicators가 없으면 직접 계산
            
            # 1. 거래량 급증 확인
            volume_surge, surge_candle = self._check_volume_surge(candle_data)
            
            # 2. 눌림목 패턴 확인
            pullback_pattern = False
            if volume_surge and surge_candle is not None:
                pullback_pattern = self._check_pullback_pattern(candle_data, surge_candle)
            
            # 3. 일목균형표 기준선 근처 확인
            ichimoku_signal = self._check_ichimoku_baseline(candle_data, price)
            
            # 매수 신호 생성 (거래량 급증 후 눌림목 패턴이 나타나고 일목균형표 기준선 근처인 경우)
            if volume_surge and pullback_pattern and ichimoku_signal:
                self._handle_buy_signal(symbol, price, "거래량 급증 후 눌림목 형성", now)
            
            # 매도 신호 생성 (보유 종목인 경우에만)
            elif symbol in self.positions:
                position = self.positions.get(symbol, {})
                avg_price = position.get("avg_price", 0)
                
                # 손절 조건 확인
                stop_loss_triggered = (avg_price > 0) and (price < avg_price * (1 - self.stop_loss_pct / 100))
                
                # 이익 실현 조건 확인
                take_profit = self._check_take_profit(candle_data, price, avg_price)
                
                if stop_loss_triggered:
                    # 손절 신호 처리
                    self._handle_sell_signal(symbol, price, "손절 조건 충족", now)
                
                elif take_profit:
                    # 이익 실현 신호 처리
                    self._handle_sell_signal(symbol, price, "이익 실현", now)
        
        except Exception as e:
            logger.error(f"실시간 가격 처리 중 오류: {str(e)}", exc_info=True)
    
    def _handle_buy_signal(self, symbol, price, reason, now):
        """매수 신호 처리"""
        # 이미 보유 중인지 확인
        if symbol in self.positions:
            logger.debug(f"이미 보유 중인 종목: {symbol}, 매수 신호 무시")
            return
        
        # 마지막 매매 이후 최소 보유 기간(일)이 지났는지 확인
        last_trade = self.trade_history.get(symbol, {})
        last_trade_time = last_trade.get("last_trade", None)
        
        if last_trade_time:
            hours_passed = (now - last_trade_time).total_seconds() / 3600
            if hours_passed < (self.min_holding_period * 24):
                logger.debug(f"최소 보유 기간이 지나지 않음: {symbol}, 경과 시간: {hours_passed:.1f}시간")
                return
        
        # 매수 신호 저장
        self.trading_signals[symbol] = {
            "signal": "buy",
            "price": price,
            "timestamp": now,
            "reason": reason
        }
        logger.info(f"매수 신호 발생: {symbol}, 현재가: {price:.2f}, 이유: {reason}")
    
    def _handle_sell_signal(self, symbol, price, reason, now):
        """매도 신호 처리"""
        # 매도 신호 저장
        self.trading_signals[symbol] = {
            "signal": "sell",
            "price": price,
            "timestamp": now,
            "reason": reason,
            "quantity": "all"  # 전량 매도
        }
        
        position = self.positions.get(symbol, {})
        avg_price = position.get("avg_price", 0)
        logger.info(f"매도({reason}) 신호 발생: {symbol}, 현재가: {price:.2f}, 평균단가: {avg_price:.2f}")
    
    def _check_volume_surge(self, candle_data):
        """
        거래량 급증 확인
        - 최근 분봉의 거래량이 평균 대비 volume_surge_threshold% 이상 증가했는지 확인
        - 동시에 가격이 min_price_increase% 이상 상승했는지 확인
        """
        try:
            # 캔들 데이터를 DataFrame으로 변환
            df = pd.DataFrame(candle_data)
            
            # 최근 10개 분봉의 평균 거래량 계산
            avg_volume = df['volume'].iloc[-11:-1].mean()  # 최근 분봉 제외한 이전 10개
            
            # 최근 캔들들에 대해 검사
            for i in range(min(5, len(df))):
                idx = len(df) - 1 - i  # 최근 캔들부터 역순으로
                if idx < 0:
                    continue
                
                current_candle = df.iloc[idx]
                current_volume = current_candle['volume']
                
                # 거래량이 평균 대비 설정 비율 이상 증가했는지 확인
                if current_volume > avg_volume * (1 + self.volume_surge_threshold / 100):
                    # 해당 캔들의 시가와 종가 확인
                    open_price = current_candle['open']
                    close_price = current_candle['close']
                    
                    # 가격이 설정 비율 이상 상승했는지 확인
                    price_change_pct = (close_price - open_price) / open_price * 100
                    if price_change_pct >= self.min_price_increase:
                        return True, current_candle
            
            return False, None
        except Exception as e:
            logger.error(f"거래량 급증 확인 중 오류: {str(e)}", exc_info=True)
            return False, None
    
    def _check_pullback_pattern(self, candle_data, surge_candle):
        """
        눌림목 패턴 확인
        - 거래량 급증 후 가격이 pullback_threshold% 이상 조정받았는지 확인
        - 조정 시 거래량이 급증 시보다 적어야 함
        """
        if surge_candle is None:
            return False
        
        try:
            df = pd.DataFrame(candle_data)
            
            # 급증 캔들 이후 캔들들 확인
            surge_idx = df[df['time'] == surge_candle['time']].index[0]
            
            # 급증 캔들의 고가
            surge_high = surge_candle['high']
            surge_close = surge_candle['close']
            surge_volume = surge_candle['volume']
            
            # 급증 캔들 이후 캔들들 중 눌림목 패턴이 있는지 확인
            for i in range(surge_idx + 1, len(df)):
                current_candle = df.iloc[i]
                
                # 현재 캔들의 저가가 급증 캔들의 고가보다 pullback_threshold% 이상 하락했는지 확인
                pullback_pct = (surge_high - current_candle['low']) / surge_high * 100
                
                # 현재 캔들의 거래량이 급증 캔들보다 적은지 확인
                lower_volume = current_candle['volume'] < surge_volume
                
                if pullback_pct >= self.pullback_threshold and lower_volume:
                    # 눌림목 패턴 확인됨
                    return True
            
            return False
        except Exception as e:
            logger.error(f"눌림목 패턴 확인 중 오류: {str(e)}", exc_info=True)
            return False
    
    def _check_ichimoku_baseline(self, candle_data, current_price):
        """
        일목균형표 기준선 근처 확인
        - 현재 가격이 기준선(26일선)의 위아래 0.5% 이내인지 확인
        """
        try:
            df = pd.DataFrame(candle_data)
            
            # 일목균형표 기준선 계산 (9일 + 26일 고가/저가 평균의 평균)
            # 분봉 데이터로는 정확한 계산이 어려우므로 단순화하여 구현
            # 실제로는 일봉 데이터로 계산하는 것이 정확함
            
            # 단순화된 기준선: 최근 26개 분봉의 (고가+저가)/2 평균
            baseline = ((df['high'] + df['low']) / 2).rolling(window=26).mean().iloc[-1]
            
            # 현재 가격이 기준선 근처인지 확인 (위아래 0.5%)
            price_diff_pct = abs(current_price - baseline) / baseline * 100
            
            return price_diff_pct <= 0.5
        except Exception as e:
            logger.error(f"일목균형표 기준선 확인 중 오류: {str(e)}", exc_info=True)
            return False
    
    def _check_take_profit(self, candle_data, current_price, avg_price):
        """
        이익 실현 조건 확인
        - 수익률이 5% 이상이고, 최근 고점에서 2% 이상 하락했을 때 이익 실현
        """
        try:
            # 현재 수익률
            profit_pct = (current_price - avg_price) / avg_price * 100
            
            # 수익률이 5% 이상인 경우에만 고려
            if profit_pct < 5:
                return False
            
            df = pd.DataFrame(candle_data)
            
            # 최근 10개 캔들의 최고가
            recent_high = df['high'].iloc[-10:].max()
            
            # 최고가 대비 현재가 하락률
            drop_pct = (recent_high - current_price) / recent_high * 100
            
            # 하락률이 2% 이상이면 이익 실현
            return drop_pct >= 2
        except Exception as e:
            logger.error(f"이익 실현 조건 확인 중 오류: {str(e)}", exc_info=True)
            return False
    
    async def monitor_signals(self):
        """매매 신호 주기적 모니터링 및 처리"""
        logger.info("매매 신호 모니터링 시작")
        
        while self.is_running:
            try:
                # 30초마다 매매 신호 확인
                await asyncio.sleep(30)
                
                # 계좌 정보 동기화
                if self.backend_client:
                    account_info = await self.backend_client.request_account_info()
                    if account_info:
                        self.update_account_info(account_info)
                
                # 매매 신호 로깅
                if self.trading_signals:
                    signal_counts = {
                        "buy": sum(1 for v in self.trading_signals.values() if v["signal"] == "buy"),
                        "sell": sum(1 for v in self.trading_signals.values() if v["signal"] == "sell")
                    }
                    logger.info(f"현재 매매 신호: 매수={signal_counts['buy']}개, 매도={signal_counts['sell']}개")
                
                # 오래된 신호 제거 (5분 이상 경과)
                now = datetime.now()
                for symbol, signal_info in list(self.trading_signals.items()):
                    timestamp = signal_info["timestamp"]
                    if (now - timestamp).total_seconds() > 300:
                        del self.trading_signals[symbol]
                        logger.debug(f"종목 {symbol}의 오래된 신호 제거 (5분 경과)")
                
            except Exception as e:
                logger.error(f"매매 신호 모니터링 중 오류: {str(e)}", exc_info=True)
                await asyncio.sleep(30)
    
    async def monitor_top_volume_stocks(self):
        """거래대금 상위 종목 모니터링"""
        logger.info("거래대금 상위 종목 모니터링 시작")
        
        while self.is_running:
            try:
                # 5분마다 확인
                await asyncio.sleep(300)
                
                # 거래대금 상위 종목 가져오기
                top_volume_stocks = await self._get_top_volume_stocks()
                
                if top_volume_stocks:
                    logger.info(f"거래대금 상위 종목 리스트 업데이트: {len(top_volume_stocks)}개")
                    
                    # 각 종목에 대해 분석 실행
                    for stock_info in top_volume_stocks:
                        # 종목 코드와 현재가 추출
                        symbol = None
                        price = 0
                        
                        # stock_info가 딕셔너리인 경우
                        if isinstance(stock_info, dict):
                            symbol = stock_info.get("symbol") or stock_info.get("code")
                            price = stock_info.get("price") or stock_info.get("close") or 0
                        
                        if symbol and price > 0:
                            # 분봉 데이터 가져오기
                            candle_data = self._get_minute_candles(symbol)
                            
                            # 실시간 가격 처리 함수 호출
                            await self.handle_realtime_price(symbol, price)
            
            except Exception as e:
                logger.error(f"거래대금 상위 종목 모니터링 중 오류: {str(e)}", exc_info=True)
                await asyncio.sleep(60)
    
    async def _get_top_volume_stocks(self, limit: int = 20) -> List[Dict]:
        """
        거래대금 상위 종목 가져오기
        다양한 StockCache 구현에 대응
        """
        if not self.stock_cache:
            return []
            
        # 1. get_top_volume_stocks 메서드 사용 (우선)
        if hasattr(self.stock_cache, "get_top_volume_stocks"):
            # 동기 메서드인 경우
            if callable(self.stock_cache.get_top_volume_stocks):
                try:
                    return self.stock_cache.get_top_volume_stocks(limit)
                except Exception:
                    pass
            
        # 2. 필터링된 종목 중에서 거래량 기준으로 정렬 (대체)
        filtered_stocks = self.get_filtered_stocks()
        if not filtered_stocks:
            return []
            
        result = []
        for symbol in filtered_stocks[:100]:  # 처음 100개 종목만 확인 (성능 최적화)
            try:
                price = self.stock_cache.get_price(symbol)
                if price > 0:
                    # 일봉 데이터 가져와서 거래량 확인
                    chart_data = None
                    if hasattr(self.stock_cache, "get_chart_data"):
                        chart_data = self.stock_cache.get_chart_data(symbol)
                    
                    volume = 0
                    if chart_data and len(chart_data) > 0:
                        # 가장 최근 거래량
                        first_item = chart_data[0]
                        if isinstance(first_item, dict) and 'volume' in first_item:
                            volume = float(first_item['volume'])
                        elif isinstance(first_item, list) and len(first_item) >= 6:
                            volume = float(first_item[5])  # 거래량이 6번째 위치라고 가정
                    
                    result.append({
                        "symbol": symbol,
                        "price": price,
                        "volume": volume
                    })
            except Exception as e:
                logger.error(f"종목 {symbol}의 거래량 정보 조회 중 오류: {str(e)}")
        
        # 거래량 기준으로 정렬하여 상위 limit개 반환
        result.sort(key=lambda x: x.get("volume", 0), reverse=True)
        return result[:limit]
    
    async def get_current_prices(self) -> Dict[str, float]:
        """현재 가격 정보 조회 - 기존 코드와 호환되는 인터페이스"""
        if not self.stock_cache:
            return {}
            
        prices = {}
        try:
            # 필터링된 종목들의 현재가 조회
            filtered_stocks = self.get_filtered_stocks()
            for symbol in filtered_stocks:
                if hasattr(self.stock_cache, "get_price"):
                    price = self.stock_cache.get_price(symbol)
                    if price and price > 0:
                        prices[symbol] = price
        except Exception as e:
            logger.error(f"현재가 정보 조회 중 오류: {str(e)}")
            
        return prices
    
    async def get_trade_decisions(self, prices: Dict[str, float] = None) -> List[Dict[str, Any]]:
        """매매 의사결정 목록 반환"""
        if not self.is_running:
            logger.warning("트레이딩 모델이 실행 중이지 않습니다.")
            return []
        
        decisions = []
        try:
            # 계좌 정보 최신화
            if self.backend_client:
                account_info = await self.backend_client.request_account_info()
                if account_info:
                    self.update_account_info(account_info)
            
            # 현재 시간
            now = datetime.now()
            
            # 매매 신호에 따른 거래 결정 처리
            for symbol, signal_info in list(self.trading_signals.items()):
                signal = signal_info["signal"]
                price = signal_info["price"]
                timestamp = signal_info["timestamp"]
                reason = signal_info.get("reason", "")
                
                # 신호 유효 시간 (5분) 체크
                if (now - timestamp).total_seconds() > 300:
                    del self.trading_signals[symbol]
                    continue
                
                # 전달받은 현재가 사용 (최신 가격)
                current_price = price
                if prices and symbol in prices:
                    current_price = prices[symbol]
                
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
                        "reason": reason,
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
                    logger.info(f"매수 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}, 이유: {reason}")
                
                # 2. 매도 신호 처리
                elif signal == "sell":
                    # 보유 중인 종목인지 확인
                    if symbol not in self.positions:
                        logger.debug(f"미보유 종목 매도 신호 무시: {symbol}")
                        del self.trading_signals[symbol]
                        continue
                    
                    position = self.positions.get(symbol, {})
                    total_quantity = position.get("quantity", 0)
                    
                    if total_quantity <= 0:
                        logger.warning(f"종목 {symbol}의 보유 수량이 0, 매도 보류")
                        continue
                    # 매도 수량 (전량)
                    quantity = total_quantity
                    
                    # 매도 결정 추가
                    decision = {
                        "symbol": symbol,
                        "action": "sell",
                        "quantity": quantity,
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
                    logger.info(f"매도 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}, 이유: {reason}")
            
            return decisions
        
        except Exception as e:
            logger.error(f"매매 의사결정 생성 중 오류: {str(e)}", exc_info=True)
            return []
                    
    # 기타 유틸리티 메서드들
    def _should_process_price_update(self, symbol: str, price: float) -> bool:
        """중복 가격 업데이트 필터링 (너무 잦은 가격 변동 무시)"""
        now = datetime.now()
        
        # 이전 처리 시간과 가격 가져오기
        last_time = self.last_processed_times.get(symbol, datetime.min)
        last_price = self.last_processed_prices.get(symbol, 0)
        
        # 처리 간격이 너무 짧으면 무시
        if (now - last_time).total_seconds() < self.min_process_interval:
            return False
        
        # 가격 변동이 너무 작으면 무시
        if last_price > 0:
            price_change_pct = abs((price - last_price) / last_price * 100)
            if price_change_pct < self.min_price_change_pct:
                return False
        
        # 처리할 경우 정보 업데이트
        self.last_processed_times[symbol] = now
        self.last_processed_prices[symbol] = price
        return True
    
    def set_backend_client(self, backend_client):
        """백엔드 클라이언트 설정"""
        self.backend_client = backend_client
        logger.info("백엔드 클라이언트 설정 완료")
        