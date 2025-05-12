"""
종목 정보 및 지표 캐시
"""
import logging
from typing import Dict, List, Optional
from datetime import datetime
import numpy as np

logger = logging.getLogger(__name__)

class StockCache:
    """종목 정보, 차트 데이터 및 지표를 캐싱하는 통합 클래스 (메모리 전용)"""
    
    def __init__(self):
        """캐시 초기화"""
        # 종목 정보 캐시
        self.stock_info_cache = {}
        
        # 필터링된 종목 리스트
        self.filtered_stockcode_list = []
        
        # 차트 데이터 캐시
        self.chart_cache = {}
        
        # Envelope 지표 캐시
        self.envelope_cache = {}
        
        # 볼린저 밴드 지표 캐시
        self.bollinger_cache = {}
        
        # 볼린저 밴드 설정
        self.bb_period = 26  # 기간 (26일)
        self.bb_std_dev = 2.0  # 표준편차 승수 (2)
        
        # 현재가 시세 캐시
        self.price_cache = {}
        
        # 코스피, 코스닥 종목 리스트
        self.kospi_symbols = []
        self.kosdaq_symbols = []
        
        # 구독 관리
        self.subscribed_symbols = set()
        
        logger.info("메모리 전용 StockCache 초기화 완료")
    
    # 구독 관리 메서드
    def add_subscribed_symbol(self, code: str):
        """구독 종목 추가"""
        self.subscribed_symbols.add(code)
    
    def remove_subscribed_symbol(self, code: str):
        """구독 종목 제거"""
        if code in self.subscribed_symbols:
            self.subscribed_symbols.remove(code)
    
    def clear_subscribed_symbols(self):
        """모든 구독 종목 제거"""
        self.subscribed_symbols.clear()
    
    def get_subscribed_symbols(self):
        """현재 구독 중인 종목 목록 반환"""
        return list(self.subscribed_symbols)
    
    # 종목 정보 관련 메서드
    def init_stock_info(self, stock_list: List[Dict]):
        """종목 정보 초기화 - 전달받은 종목을 모두 캐싱"""
        try:
            # 종목 정보 캐시 초기화
            self.stock_info_cache.clear()
            self.kospi_symbols.clear()
            self.kosdaq_symbols.clear()
            
            logger.info(f"종목 정보 초기화 시작: {len(stock_list)}개 종목")
            
            # 종목 정보 캐싱
            for stock in stock_list:
                code = stock.get("shortCode")
                if not code:
                    continue
                
                # 필요한 정보만 저장
                self.stock_info_cache[code] = {
                    "shortCode": code,
                    "name": stock.get("shortName", ""),
                    "market": stock.get("market", "")
                }
                
                # 코스피/코스닥 분류
                market = stock.get("market", "").upper()
                if "KOSPI" in market:
                    self.kospi_symbols.append(code)
                elif "KOSDAQ" in market:
                    self.kosdaq_symbols.append(code)
            
            logger.info(f"종목 정보 캐시 초기화 완료: 총 {len(self.stock_info_cache)}개 종목")
            logger.info(f"코스피: {len(self.kospi_symbols)}개, 코스닥: {len(self.kosdaq_symbols)}개")
            
            return True
                
        except Exception as e:
            logger.error(f"종목 정보 캐시 초기화 중 오류: {str(e)}")
            return False
    
    # 필터링된 종목 관련 메서드
    def set_filtered_stocks(self, symbols: List[str]):
        """필터링된 종목 리스트 설정"""
        self.filtered_stockcode_list = symbols
        logger.info(f"필터링된 종목 리스트 설정: {len(symbols)}개 종목")
    
    # 차트 데이터 관련 메서드
    def add_chart_data(self, symbol: str, chart_data: List[Dict]):
        """차트 데이터 추가/업데이트"""
        self.chart_cache[symbol] = chart_data
    
    def get_chart_data(self, symbol: str):
        """차트 데이터 조회"""
        return self.chart_cache.get(symbol, [])
    
    # Envelope 지표 관련 메서드
    def calculate_envelope_indicators(self):
        """모든 필터링된 종목의 Envelope 지표 계산"""
        logger.info(f"Envelope 지표 계산 시작: {len(self.filtered_stockcode_list)}개 종목")
        self.envelope_cache = {}

        success_count = 0
        
        # Envelope 설정
        envelope_percentage = 0.2  # 20%
        
        # 모든 필터링된 종목에 대해 계산
        for symbol in self.filtered_stockcode_list:
            try:
                # 차트 데이터 접근
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
                
                # 디버깅 로그 추가
                logger.debug(f"종목 {symbol} Envelope 계산: MA20={ma20:.2f}, 상한={upper_band:.2f}, 하한={lower_band:.2f}, 현재가={last_close:.2f}")
                
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
        
        logger.info(f"Envelope 지표 계산 완료: {success_count}/{len(self.filtered_stockcode_list)}개 성공")
        
        return success_count
    
    def get_envelope_indicators(self, symbol: str, current_price: float = None):
        """종목의 Envelope 지표 조회"""
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
    
    # 볼린저 밴드 지표 관련 메서드
    def calculate_bollinger_bands(self):
        """모든 필터링된 종목의 볼린저 밴드 지표 계산"""
        logger.info(f"볼린저 밴드 지표 계산 시작: {len(self.filtered_stockcode_list)}개 종목")
        self.bollinger_cache = {}

        success_count = 0
        
        # 모든 필터링된 종목에 대해 계산
        for symbol in self.filtered_stockcode_list:
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
                
                logger.debug(f"종목 {symbol} 볼린저 밴드 계산: {actual_period}일 기간으로 계산 (요청 기간: {self.bb_period}일)")
                
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
                
                logger.debug(f"종목 {symbol} 볼린저 밴드 계산: SMA={sma:.2f}, 상한={upper_band:.2f}, 하한={lower_band:.2f}, 현재가={last_close:.2f}")
                
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
        
        logger.info(f"볼린저 밴드 지표 계산 완료: {success_count}/{len(self.filtered_stockcode_list)}개 성공")
        return success_count
    
    def get_bollinger_bands(self, symbol: str, current_price: float = None):
        """종목의 볼린저 밴드 지표 조회"""
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
    
    # 현재가 관련 메서드
    def update_price(self, code: str, price: float):
        """현재가 정보 업데이트"""
        self.price_cache[code] = price
    
    def get_price(self, code: str) -> Optional[float]:
        """캐시된 현재가 조회"""
        return self.price_cache.get(code)