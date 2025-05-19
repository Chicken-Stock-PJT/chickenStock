"""
BotStockCache - 봇별 주식 지표 캐시 래퍼 클래스
"""
import logging
from typing import Dict, Optional
from datetime import datetime
import numpy as np
import pandas as pd

from app.models.trade_models import TradingStrategy

logger = logging.getLogger(__name__)

class BotStockCache:
    """개별 봇을 위한 StockCache 래퍼 클래스"""
    
    def __init__(self, shared_stock_cache, strategy: TradingStrategy):
        """
        초기화 함수
        
        :param shared_stock_cache: 공유 StockCache 인스턴스 (모든 봇이 공유하는 실시간 가격 데이터)
        :param strategy: 봇의 거래 전략 (볼린저 밴드 또는 Envelope)
        """
        self.shared_cache = shared_stock_cache
        self.strategy = strategy
        
        # 봇별 전략 지표 저장을 위한 딕셔너리
        self.envelope_cache = {}
        self.bollinger_cache = {}
        self.short_term_cache = {}
        
        # 볼린저 밴드 설정
        self.bb_period = 26  # 기간 (26일)
        self.bb_std_dev = 2.0  # 표준편차 승수 (2)
        
        logger.info(f"봇별 StockCache 래퍼 초기화 완료 (전략: {strategy})")
    
    # 기본 데이터는 공유 cache에서 가져옴
    def get_price(self, symbol):
        """현재가 조회 (공유 캐시에서 가져옴)"""
        return self.shared_cache.get_price(symbol)
    
    def get_stock_name(self, symbol):
        """종목명 조회 (공유 캐시에서 가져옴)"""
        return self.shared_cache.get_stock_name(symbol)
    
    def get_change(self, symbol):
        """변동폭 조회 (공유 캐시에서 가져옴)"""
        return self.shared_cache.get_change(symbol)
    
    def get_change_percent(self, symbol):
        """변동률 조회 (공유 캐시에서 가져옴)"""
        return self.shared_cache.get_change_percent(symbol)
    
    def get_chart_data(self, symbol):
        """차트 데이터 조회 (공유 캐시에서 가져옴)"""
        return self.shared_cache.get_chart_data(symbol)
    
    def get_minute_chart_data(self, symbol, time_interval=5):
        """분봉 차트 데이터 조회 (공유 캐시에서 가져옴)"""
        if hasattr(self.shared_cache, 'get_minute_chart_data'):
            return self.shared_cache.get_minute_chart_data(symbol, time_interval)
        return []
    
    def get_filtered_stocks(self):
        """필터링된 종목 리스트 조회 (공유 캐시에서 가져옴)"""
        return self.shared_cache.filtered_stockcode_list
    
    def calculate_strategy_indicators(self):
        """봇의 전략에 따라 지표 계산"""
        strategy_enum = self.strategy
        if isinstance(self.strategy, str):
            try:
                strategy_enum = TradingStrategy[self.strategy.upper()]
            except KeyError:
                logger.error(f"알 수 없는 전략 문자열: {self.strategy}")
                return 0
        
        if strategy_enum == TradingStrategy.ENVELOPE:
            return self.calculate_envelope_indicators()
        elif strategy_enum == TradingStrategy.BOLLINGER:
            return self.calculate_bollinger_bands()
        elif strategy_enum == TradingStrategy.SHORT_TERM:
            return self.calculate_short_term_indicators()
        elif strategy_enum == TradingStrategy.DRL_UTRANS:
            return self.calculate_drl_utrans_indicators()
        else:
            logger.warning(f"지원되지 않는 전략: {self.strategy}")
            return 0
    
    def calculate_envelope_indicators(self):
        """Envelope 지표 계산 - 봇에 특화된 버전"""
        # 성공 카운트 초기화
        success_count = 0
        
        # Envelope 설정
        envelope_percentage = 0.2  # 20%
        
        # 필터링된 종목 목록 가져오기
        filtered_stocks = self.get_filtered_stocks()
        
        logger.info(f"봇 전용 Envelope 지표 계산 시작: {len(filtered_stocks)}개 종목")
        
        # 모든 필터링된 종목에 대해 계산
        for symbol in filtered_stocks:
            try:
                # 차트 데이터 가져오기
                chart_data = self.get_chart_data(symbol)
                
                # 차트 데이터가 없으면 건너뛰기
                if not chart_data or len(chart_data) < 20:
                    logger.warning(f"종목 {symbol} 차트 데이터 부족: {len(chart_data) if chart_data else 0}개")
                    continue
                
                # 종가 추출
                closing_prices = []
                for item in chart_data:
                    if isinstance(item, dict) and 'close' in item:
                        closing_prices.append(float(item['close']))
                    elif isinstance(item, list) and len(item) >= 5:
                        closing_prices.append(float(item[4]))
                
                if len(closing_prices) < 20:
                    logger.warning(f"종목 {symbol} 종가 데이터 부족: {len(closing_prices)}개")
                    continue
                
                # 20일 이동평균 계산
                ma20 = sum(closing_prices[:20]) / 20
                
                # Envelope 상/하한 계산
                upper_band = ma20 * (1 + envelope_percentage)
                lower_band = ma20 * (1 - envelope_percentage)
                
                # 현재가 (마지막 종가)
                last_close = closing_prices[0]
                
                # 매수/매도 신호
                signal = "중립"
                if last_close >= upper_band:
                    signal = "매도"
                elif last_close <= lower_band:
                    signal = "매수"
                
                # 캐시에 저장 - middleBand를 명시적으로 설정
                self.envelope_cache[symbol] = {
                    "middleBand": float(ma20),  # 이동평균선을 중앙선으로 설정
                    "upperBand": float(upper_band),
                    "lowerBand": float(lower_band),
                    "currentPrice": float(last_close),
                    "signal": signal,
                    "lastUpdate": datetime.now().isoformat()
                }
                
                success_count += 1
                
            except Exception as e:
                logger.error(f"종목 {symbol} Envelope 지표 계산 오류: {str(e)}")
        
        logger.info(f"봇 전용 Envelope 지표 계산 완료: {success_count}/{len(filtered_stocks)}개 성공")
        
        return success_count
    
    def calculate_bollinger_bands(self):
        """볼린저 밴드 지표 계산 - 봇에 특화된 버전"""
        # 성공 카운트 초기화
        success_count = 0
        
        # 필터링된 종목 목록 가져오기
        filtered_stocks = self.get_filtered_stocks()
        
        logger.info(f"봇 전용 볼린저 밴드 지표 계산 시작: {len(filtered_stocks)}개 종목")
        
        # 모든 필터링된 종목에 대해 계산
        for symbol in filtered_stocks:
            try:
                # 차트 데이터 접근 (캐시 활용)
                chart_data = self.get_chart_data(symbol)
                
                # 차트 데이터가 없으면 건너뛰기
                if not chart_data:
                    logger.warning(f"종목 {symbol} 차트 데이터 없음")
                    continue
                
                # 사용 가능한 데이터로 최대한 계산 (차트 데이터 길이 확인)
                actual_period = min(self.bb_period, len(chart_data))
                if actual_period < 10:  # 최소 10일 데이터는 필요
                    logger.warning(f"종목 {symbol} 차트 데이터 부족: {len(chart_data)}개 (최소 10일 필요)")
                    continue
                
                # 종가 추출
                closing_prices = []
                for item in chart_data:
                    if isinstance(item, dict) and 'close' in item:
                        closing_prices.append(float(item['close']))
                    elif isinstance(item, list) and len(item) >= 5:
                        closing_prices.append(float(item[4]))
                
                if len(closing_prices) < self.bb_period:
                    logger.warning(f"종목 {symbol} 종가 데이터 부족: {len(closing_prices)}개")
                    continue
                
                # 사용 가능한 데이터로 최대한 계산
                actual_period = min(self.bb_period, len(closing_prices))
                prices = closing_prices[:actual_period]
                
                # 볼린저 밴드 계산
                sma = np.mean(prices)  # 단순 이동평균
                std_dev = np.std(prices)  # 표준편차
                
                # 실제 사용된 기간 저장
                used_period = actual_period
                
                upper_band = sma + (self.bb_std_dev * std_dev)
                lower_band = sma - (self.bb_std_dev * std_dev)
                
                # 현재가 (마지막 종가)
                last_close = closing_prices[0]
                
                # 매수/매도 신호 판단 (기본 볼린저 밴드 전략)
                signal = "중립"
                if last_close <= lower_band:
                    signal = "매수"
                elif last_close >= upper_band:
                    signal = "매도"
                
                # %B 계산 (주가가 밴드 내에서 어디에 위치하는지 0~1 사이 값)
                percent_b = (last_close - lower_band) / (upper_band - lower_band) if (upper_band != lower_band) else 0.5
                
                # 밴드폭(Bandwidth) 계산 (밴드의 변동성)
                bandwidth = (upper_band - lower_band) / sma
                
                # 캐시에 저장
                self.bollinger_cache[symbol] = {
                    "middleBand": float(sma),
                    "upperBand": float(upper_band),
                    "lowerBand": float(lower_band),
                    "currentPrice": float(last_close),
                    "percentB": float(percent_b),
                    "bandwidth": float(bandwidth),
                    "signal": signal,
                    "period": used_period,  # 실제 사용된 기간 저장
                    "lastUpdate": datetime.now().isoformat()
                }
                
                success_count += 1
                
            except Exception as e:
                logger.error(f"종목 {symbol} 볼린저 밴드 지표 계산 오류: {str(e)}")
        
        logger.info(f"봇 전용 볼린저 밴드 지표 계산 완료: {success_count}/{len(filtered_stocks)}개 성공")
        return success_count
    
    def calculate_short_term_indicators(self):
        """단타 매매용 지표 계산"""
        # 성공 카운트 초기화
        success_count = 0
        
        # 필터링된 종목 목록 가져오기
        filtered_stocks = self.get_filtered_stocks()
        
        logger.info(f"봇 전용 단타 매매 지표 계산 시작: {len(filtered_stocks)}개 종목")
        
        # 모든 필터링된 종목에 대해 계산
        for symbol in filtered_stocks:
            try:
                # 분봉 차트 데이터 가져오기
                minute_data = self.get_minute_chart_data(symbol, 5)
                
                # 분봉 데이터가 없으면 건너뛰기
                if not minute_data or len(minute_data) < 20:
                    logger.warning(f"종목 {symbol} 분봉 데이터 부족: {len(minute_data) if minute_data else 0}개")
                    continue
                
                # 데이터를 DataFrame으로 변환
                df = pd.DataFrame(minute_data)
                
                # 1. 거래량 급증 확인
                volume_surge, surge_candle = self._check_volume_surge_indicator(df)
                
                # 2. 일목균형표 기준선 계산
                baseline = self._calculate_ichimoku_baseline(df)
                
                # 현재가 (마지막 분봉의 종가)
                last_close = df['close'].iloc[-1] if len(df) > 0 else 0
                
                # 지표 저장
                self.short_term_cache[symbol] = {
                    "volume_surge": volume_surge,
                    "surge_candle": surge_candle.to_dict() if volume_surge and surge_candle is not None else {},
                    "ichimoku_baseline": baseline,
                    "currentPrice": float(last_close),
                    "lastUpdate": datetime.now().isoformat()
                }
                
                success_count += 1
                
            except Exception as e:
                logger.error(f"종목 {symbol} 단타 매매 지표 계산 오류: {str(e)}")
        
        logger.info(f"봇 전용 단타 매매 지표 계산 완료: {success_count}/{len(filtered_stocks)}개 성공")
        
        return success_count
    
    def _check_volume_surge_indicator(self, df):
        """거래량 급증 지표 계산 (DataFrame 사용)"""
        try:
            if len(df) < 10:
                return False, None
            
            # 최근 10개 분봉의 평균 거래량 계산
            avg_volume = df['volume'].iloc[-11:-1].mean()  # 최근 분봉 제외한 이전 10개
            
            # 최근 캔들들에 대해 검사
            for i in range(min(5, len(df))):
                idx = len(df) - 1 - i  # 최근 캔들부터 역순으로
                if idx < 0:
                    continue
                
                current_candle = df.iloc[idx]
                current_volume = current_candle['volume']
                
                # 거래량이 평균 대비 500% 이상 증가했는지 확인
                if current_volume > avg_volume * 6:  # 500% 증가 = 기존 대비 6배
                    # 해당 캔들의 시가와 종가 확인
                    open_price = current_candle['open']
                    close_price = current_candle['close']
                    
                    # 가격이 3% 이상 상승했는지 확인
                    price_change_pct = (close_price - open_price) / open_price * 100
                    if price_change_pct >= 3:
                        return True, current_candle
            
            return False, None
        except Exception as e:
            logger.error(f"거래량 급증 지표 계산 중 오류: {str(e)}")
            return False, None
    
    def _calculate_ichimoku_baseline(self, df):
        """일목균형표 기준선 계산 (DataFrame 사용)"""
        try:
            if len(df) < 26:
                return 0
            
            # 기준선 (26일 고가와 저가의 평균)
            high_26 = df['high'].iloc[-26:].max()
            low_26 = df['low'].iloc[-26:].min()
            baseline = (high_26 + low_26) / 2
            
            return baseline
        except Exception as e:
            logger.error(f"일목균형표 기준선 계산 중 오류: {str(e)}")
            return 0
        
    def get_envelope_indicators(self, symbol, current_price=None):
        """종목의 Envelope 지표 조회 - 봇 전용 캐시에서 조회"""
        if symbol in self.envelope_cache:
            indicators = self.envelope_cache[symbol].copy()
            
            # 실시간 가격 업데이트
            if current_price is not None:
                indicators["currentPrice"] = current_price
                
                # 신호 업데이트
                signal = "중립"
                if current_price >= indicators["upperBand"]:
                    signal = "매도"
                elif current_price <= indicators["lowerBand"]:
                    signal = "매수"
                indicators["signal"] = signal
            
            return indicators
        
        return None
    
    def get_bollinger_bands(self, symbol, current_price=None):
        """종목의 볼린저 밴드 지표 조회 - 봇 전용 캐시에서 조회"""
        if symbol in self.bollinger_cache:
            indicators = self.bollinger_cache[symbol].copy()
            
            # 실시간 가격 업데이트
            if current_price is not None:
                indicators["currentPrice"] = current_price
                
                # 신호 업데이트
                signal = "중립"
                if current_price <= indicators["lowerBand"]:
                    signal = "매수"
                elif current_price >= indicators["upperBand"]:
                    signal = "매도"
                
                # %B 재계산
                upper_band = indicators["upperBand"]
                lower_band = indicators["lowerBand"]
                percent_b = (current_price - lower_band) / (upper_band - lower_band) if (upper_band != lower_band) else 0.5
                indicators["percentB"] = percent_b
                indicators["signal"] = signal
            
            return indicators
        
        return None
    
    def get_short_term_indicators(self, symbol, current_price=None):
        if symbol in self.short_term_cache:
            indicators = self.short_term_cache[symbol].copy()
            
            # 실시간 가격 업데이트
            if current_price is not None:
                indicators["currentPrice"] = current_price
                
                # 1. 거래량 급증 신호 확인 (직전 캐싱된 값에서)
                volume_surge = indicators.get("volume_surge", False)
                
                # 2. 눌림목 패턴 확인
                pullback_low = indicators.get("pullback_low", 0)
                surge_high = indicators.get("surge_high", 0)
                
                # 3. 일목균형표 기준선 확인
                baseline = indicators.get("ichimoku_baseline", 0)
                
                # 4. 매수/매도 신호 업데이트
                signal = "중립"
                
                # 매수 신호: 눌림목 패턴에서 회복 시작 (일목균형표 기준선 근처)
                if volume_surge and pullback_low > 0 and surge_high > 0:
                    # 눌림목에서 반등 시작 (현재가가 저점보다 높고 고점보다 낮음)
                    if current_price > pullback_low and current_price < surge_high:
                        # 일목균형표 기준선 근처인지 확인
                        if baseline > 0 and abs(current_price - baseline) / baseline <= 0.005:  # 0.5% 이내
                            signal = "매수"
                
                # 매도 신호: 손절 조건 또는 이익실현 조건
                avg_price = indicators.get("avg_purchase_price", 0)
                if avg_price > 0:  # 보유 중인 경우
                    # 손절 조건: 평균 단가 대비 2% 이상 하락
                    stop_loss_pct = indicators.get("stop_loss_pct", 2.0)
                    if current_price < avg_price * (1 - stop_loss_pct / 100):
                        signal = "매도"
                    
                    # 이익실현 조건: 5% 이상 수익 & 최근 고점에서 2% 이상 하락
                    recent_high = indicators.get("recent_high", 0)
                    profit_pct = ((current_price / avg_price) - 1) * 100
                    
                    if profit_pct >= 5 and recent_high > 0:
                        drop_pct = ((recent_high - current_price) / recent_high) * 100
                        if drop_pct >= 2:
                            signal = "매도"
                
                # 신호 업데이트
                indicators["signal"] = signal
                
                # 타임스탬프 업데이트
                indicators["lastUpdate"] = datetime.now().isoformat()
            
            return indicators
        
        return None
    
    def calculate_strategy_indicators(self):
        """봇의 전략에 따라 지표 계산"""
        strategy_enum = self.strategy
        if isinstance(self.strategy, str):
            try:
                strategy_enum = TradingStrategy[self.strategy.upper()]
            except KeyError:
                logger.error(f"알 수 없는 전략 문자열: {self.strategy}")
                return 0
        
        if strategy_enum == TradingStrategy.ENVELOPE:
            return self.calculate_envelope_indicators()
        elif strategy_enum == TradingStrategy.BOLLINGER:
            return self.calculate_bollinger_bands()
        elif strategy_enum == TradingStrategy.SHORT_TERM:
            return self.calculate_short_term_indicators()
        elif strategy_enum == TradingStrategy.DRL_UTRANS:
            return self.calculate_drl_utrans_indicators()
        else:
            logger.warning(f"지원되지 않는 전략: {self.strategy}")
            return 0
    
    def refresh_indicators(self):
        """봇 전략에 따른 지표 새로고침"""
        return self.calculate_strategy_indicators()