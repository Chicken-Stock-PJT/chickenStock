import logging
import os
import json
from typing import Dict, List, Optional, Any
from datetime import datetime

logger = logging.getLogger(__name__)

class StockCache:
    """종목 정보, 차트 데이터 및 지표를 캐싱하는 통합 클래스"""
    
    def __init__(self, kiwoom_api=None):
        """캐시 초기화"""
        self.kiwoom_api = kiwoom_api

        # 종목 정보 캐시
        self.stock_info_cache = {}  # {code: {shortCode, shortName, market, stockType, faceValue}}
        
        # 필터링된 종목 리스트 (시가총액 상위 - 코스피 450, 코스닥 150)
        self.filtered_stockcode_list = []
        
        # 차트 데이터 캐시
        self.chart_cache = {}  # {code: [chart_data]}
        
        # Envelope 지표 캐시
        self.envelope_cache = {}  # {code: {MA20, upperBand, lowerBand, signal}}
        
        # 현재가 시세 캐시
        self.price_cache = {}  # {code: price}
        
        # 코스피, 코스닥 종목 리스트
        self.kospi_symbols = []
        self.kosdaq_symbols = []
        
        # 캐시 파일 경로
        self.cache_dir = os.path.join(os.getcwd(), "cache")
        os.makedirs(self.cache_dir, exist_ok=True)
        
        # 캐시 파일명
        self.stock_cache_file = os.path.join(self.cache_dir, "stock_info.json")
        self.filtered_cache_file = os.path.join(self.cache_dir, "filtered_stocks.json")
        self.chart_cache_file = os.path.join(self.cache_dir, "chart_data.json")
        self.envelope_cache_file = os.path.join(self.cache_dir, "envelope_indicators.json")
        
        # 캐시 로드
        self._load_cache()
    
    def _load_cache(self):
        """저장된 캐시 파일 로드"""
        # 1. 종목 정보 캐시 로드
        self._load_stock_info_cache()
        
        # 2. 필터링된 종목 리스트 로드
        self._load_filtered_stocks_cache()
        
        # 3. 차트 데이터 캐시 로드
        self._load_chart_cache()
        
        # 4. Envelope 지표 캐시 로드
        self._load_envelope_cache()

        # 5. 구독 관리 - 추가된 부분
        self.subscribed_symbols = set()

    # 구독 관리 메서드 추가
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
    
    def _load_stock_info_cache(self):
        """종목 정보 캐시 로드"""
        try:
            if os.path.exists(self.stock_cache_file):
                with open(self.stock_cache_file, 'r', encoding='utf-8') as f:
                    cache_data = json.load(f)
                    self.stock_info_cache = cache_data.get('stock_info', {})
                    self.kospi_symbols = cache_data.get('kospi_symbols', [])
                    self.kosdaq_symbols = cache_data.get('kosdaq_symbols', [])
                    
                    logger.info(f"종목 정보 캐시 로드 완료: {len(self.stock_info_cache)}개 종목")
                    logger.info(f"코스피: {len(self.kospi_symbols)}개, 코스닥: {len(self.kosdaq_symbols)}개")
        except Exception as e:
            logger.error(f"종목 정보 캐시 로드 중 오류: {str(e)}")
            self.stock_info_cache = {}
            self.kospi_symbols = []
            self.kosdaq_symbols = []
    
    def _load_filtered_stocks_cache(self):
        """필터링된 종목 리스트 로드"""
        try:
            if os.path.exists(self.filtered_cache_file):
                with open(self.filtered_cache_file, 'r', encoding='utf-8') as f:
                    cache_data = json.load(f)
                    self.filtered_stockcode_list = cache_data.get('filtered_stocks', [])
                    logger.info(f"필터링된 종목 캐시 로드 완료: {len(self.filtered_stockcode_list)}개 종목")
        except Exception as e:
            logger.error(f"필터링된 종목 캐시 로드 중 오류: {str(e)}")
            self.filtered_stockcode_list = []
    
    def _load_chart_cache(self):
        """차트 데이터 캐시 로드"""
        try:
            if os.path.exists(self.chart_cache_file):
                with open(self.chart_cache_file, 'r', encoding='utf-8') as f:
                    cache_data = json.load(f)
                    self.chart_cache = cache_data.get('chart_data', {})
                    logger.info(f"차트 데이터 캐시 로드 완료: {len(self.chart_cache)}개 종목")
        except Exception as e:
            logger.error(f"차트 데이터 캐시 로드 중 오류: {str(e)}")
            self.chart_cache = {}
    
    def _load_envelope_cache(self):
        """Envelope 지표 캐시 로드"""
        try:
            if os.path.exists(self.envelope_cache_file):
                with open(self.envelope_cache_file, 'r', encoding='utf-8') as f:
                    cache_data = json.load(f)
                    self.envelope_cache = cache_data.get('indicators', {})
                    logger.info(f"Envelope 지표 캐시 로드 완료: {len(self.envelope_cache)}개 종목")
        except Exception as e:
            logger.error(f"Envelope 지표 캐시 로드 중 오류: {str(e)}")
            self.envelope_cache = {}
    
    def _save_stock_info_cache(self):
        """종목 정보 캐시 저장"""
        try:
            cache_data = {
                'stock_info': self.stock_info_cache,
                'kospi_symbols': self.kospi_symbols,
                'kosdaq_symbols': self.kosdaq_symbols,
                'last_update': datetime.now().isoformat()
            }
            
            with open(self.stock_cache_file, 'w', encoding='utf-8') as f:
                json.dump(cache_data, f)
            
            logger.info(f"종목 정보 캐시 저장 완료: {len(self.stock_info_cache)}개 종목")
        except Exception as e:
            logger.error(f"종목 정보 캐시 저장 중 오류: {str(e)}")
    
    def _save_filtered_stocks_cache(self):
        """필터링된 종목 리스트 저장"""
        try:
            cache_data = {
                'filtered_stocks': self.filtered_stockcode_list,
                'last_update': datetime.now().isoformat()
            }
            
            with open(self.filtered_cache_file, 'w', encoding='utf-8') as f:
                json.dump(cache_data, f)
            
            logger.info(f"필터링된 종목 캐시 저장 완료: {len(self.filtered_stockcode_list)}개 종목")
        except Exception as e:
            logger.error(f"필터링된 종목 캐시 저장 중 오류: {str(e)}")
    
    def _save_chart_cache(self):
        """차트 데이터 캐시 저장"""
        try:
            cache_data = {
                'chart_data': self.chart_cache,
                'last_update': datetime.now().isoformat()
            }
            
            with open(self.chart_cache_file, 'w', encoding='utf-8') as f:
                json.dump(cache_data, f)
            
            logger.info(f"차트 데이터 캐시 저장 완료: {len(self.chart_cache)}개 종목")
        except Exception as e:
            logger.error(f"차트 데이터 캐시 저장 중 오류: {str(e)}")
    
    def _save_envelope_cache(self):
        """Envelope 지표 캐시 저장"""
        try:
            cache_data = {
                'indicators': self.envelope_cache,
                'last_update': datetime.now().isoformat()
            }
            
            with open(self.envelope_cache_file, 'w', encoding='utf-8') as f:
                json.dump(cache_data, f)
            
            logger.info(f"Envelope 지표 캐시 저장 완료: {len(self.envelope_cache)}개 종목")
        except Exception as e:
            logger.error(f"Envelope 지표 캐시 저장 중 오류: {str(e)}")
    
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
                    logger.warning(f"종목 코드가 누락된 데이터 건너뜀: {stock}")
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
            
            # 캐시 저장
            self._save_stock_info_cache()
            
            return True
                
        except Exception as e:
            logger.error(f"종목 정보 캐시 초기화 중 오류: {str(e)}")
            return False
    
    # 필터링된 종목 관련 메서드
    def set_filtered_stocks(self, symbols: List[str]):
        """필터링된 종목 리스트 설정 (시가총액 상위 600개)"""
        self.filtered_stockcode_list = symbols
        logger.info(f"필터링된 종목 리스트 설정: {len(symbols)}개 종목")
        self._save_filtered_stocks_cache()
    
    # 차트 데이터 관련 메서드
    def add_chart_data(self, symbol: str, chart_data: List[Dict]):
        """차트 데이터 추가/업데이트"""
        self.chart_cache[symbol] = chart_data
        # 차트 데이터는 양이 많으므로 주기적으로만 저장
    
    def save_chart_cache(self):
        """차트 데이터 캐시 저장 (주기적으로 호출)"""
        self._save_chart_cache()
    
    def get_chart_data(self, symbol: str):
        """차트 데이터 조회"""
        return self.chart_cache.get(symbol, [])
    
    # Envelope 지표 관련 메서드
    def calculate_envelope_indicators(self):
        """모든 필터링된 종목의 Envelope 지표 계산"""
        logger.info(f"Envelope 지표 계산 시작: {len(self.filtered_stockcode_list)}개 종목")
        self.envelope_cache = {}

        success_count = 0
        failed_symbols = []
        
        # Envelope 설정
        envelope_percentage = 0.2  # 20%
        
        # 모든 필터링된 종목에 대해 계산
        for symbol in self.filtered_stockcode_list:
            try:
                # 차트 데이터 접근 - KiwoomAPI 참조를 통해
                chart_data = None
                if self.kiwoom_api:
                    # 모든 가능한 캐시 키 시도
                    possible_keys = [
                        f"daily_{symbol}___{120}",
                        f"daily_{symbol}__{120}",
                        f"daily_{symbol}",
                        symbol
                    ]
                    
                    # 키움 API의 chart_cache에서 직접 가져오기
                    for key in possible_keys:
                        if key in self.kiwoom_api.chart_cache:
                            chart_data = self.kiwoom_api.chart_cache[key]
                            logger.debug(f"종목 {symbol}의 차트 데이터를 키 '{key}'로 찾았습니다")
                            break
                
                # 차트 데이터가 없으면 다른 방법으로 시도 (모든 키 조회)
                if not chart_data and self.kiwoom_api and hasattr(self.kiwoom_api, 'chart_cache'):
                    # 모든 키를 순회하며 symbol을 포함하는 키 찾기
                    for key, data in self.kiwoom_api.chart_cache.items():
                        if symbol in key:
                            chart_data = data
                            logger.debug(f"종목 {symbol}의 차트 데이터를 포함 키 '{key}'로 찾았습니다")
                            break
                
                # 차트 데이터가 없으면 로깅 후 건너뛰기
                if not chart_data or len(chart_data) < 20:
                    logger.warning(f"종목 {symbol}의 차트 데이터 부족: {len(chart_data) if chart_data else 0}개")
                    
                    # 디버깅 정보 추가 - 차트 캐시 키 출력
                    if self.kiwoom_api and hasattr(self.kiwoom_api, 'chart_cache'):
                        cache_keys = list(self.kiwoom_api.chart_cache.keys())
                        if cache_keys:
                            logger.debug(f"차트 캐시 키 샘플: {cache_keys[:5]}")
                            
                    failed_symbols.append(symbol)
                    continue
                
                # 종가 추출
                closing_prices = []
                for item in chart_data:
                    if isinstance(item, dict) and 'close' in item:
                        closing_prices.append(float(item['close']))
                    elif isinstance(item, list) and len(item) >= 5:
                        closing_prices.append(float(item[4]))
                
                if len(closing_prices) < 20:
                    logger.warning(f"종목 {symbol}의 종가 데이터 부족: {len(closing_prices)}개")
                    failed_symbols.append(symbol)
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
                
                # 캐시에 저장
                self.envelope_cache[symbol] = {
                    "MA20": float(ma20),
                    "upperBand": float(upper_band),
                    "lowerBand": float(lower_band),
                    "currentPrice": float(last_close),
                    "signal": signal,
                    "lastUpdate": datetime.now().isoformat()
                }
                
                success_count += 1
                
                # 일부 종목만 로깅 (매 5번째나 마지막)
                if success_count % 5 == 0 or success_count == len(self.filtered_stockcode_list):
                    logger.info(f"Envelope 지표 계산 진행: {success_count}/{len(self.filtered_stockcode_list)}")
                
            except Exception as e:
                logger.error(f"종목 {symbol} Envelope 지표 계산 오류: {str(e)}")
                failed_symbols.append(symbol)
        
        # 캐시 저장
        self._save_envelope_cache()
        
        logger.info(f"Envelope 지표 계산 완료: {success_count}/{len(self.filtered_stockcode_list)}개 성공")
        
        # 실패한 종목 로깅
        if failed_symbols:
            logger.warning(f"Envelope 지표 계산 실패 종목: {len(failed_symbols)}개")
            logger.warning(f"실패 종목 샘플: {failed_symbols[:10]}")
        
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
        
        logger.warning(f"종목 {symbol}의 Envelope 지표가 캐시에 없음")
        return None
    
    # 현재가 관련 메서드
    def update_price(self, code: str, price: int):
        """현재가 정보 업데이트"""
        self.price_cache[code] = price
    
    def get_price(self, code: str) -> Optional[int]:
        """캐시된 현재가 조회"""
        return self.price_cache.get(code)