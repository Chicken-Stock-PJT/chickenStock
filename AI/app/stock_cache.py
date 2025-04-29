import logging
from typing import Dict, List, Optional
from datetime import datetime

logger = logging.getLogger(__name__)

class StockCache:
    """종목 정보와 시세 데이터를 캐싱하는 클래스"""
    
    def __init__(self):
        """캐시 초기화"""
        # 종목 정보 캐시
        self.stock_info_cache = {}  # {code: {shortCode, shortName, market, stockType, faceValue}}
        
        # 현재가 시세 캐시
        self.price_cache = {}  # {code: {price, timestamp}}
        
        # 구독 관리
        self.subscribed_symbols = set()
        
        # 코스피, 코스닥 종목 리스트
        self.kospi_symbols = []
        self.kosdaq_symbols = []
    
    def init_stock_info(self, stock_list: List[Dict]):
        """종목 정보 초기화"""
        try:
            # 종목 정보 캐시 초기화
            self.stock_info_cache.clear()
            self.kospi_symbols.clear()
            self.kosdaq_symbols.clear()
            
            # 종목 정보 캐싱
            for stock in stock_list:
                code = stock.get("shortCode")
                if not code:
                    continue
                
                self.stock_info_cache[code] = {
                    "shortCode": code,
                    "shortName": stock.get("shortName", ""),
                    "market": stock.get("market", ""),
                    "stockType": stock.get("stockType", ""),
                    "faceValue": stock.get("faceValue", "0")
                }
                
                # 코스피/코스닥 분류
                if stock.get("market") == "KOSPI":
                    self.kospi_symbols.append(code)
                elif stock.get("market") == "KOSDAQ":
                    self.kosdaq_symbols.append(code)
            
            logger.info(f"종목 정보 캐시 초기화 완료: 총 {len(self.stock_info_cache)}개 종목")
            logger.info(f"코스피 종목 수: {len(self.kospi_symbols)}, 코스닥 종목 수: {len(self.kosdaq_symbols)}")
            
            return True
        
        except Exception as e:
            logger.error(f"종목 정보 캐시 초기화 중 오류: {str(e)}")
            return False
    
    def get_stock_name(self, code: str) -> str:
        """종목 코드로 종목명 조회"""
        stock = self.stock_info_cache.get(code, {})
        return stock.get("shortName", "")
    
    def get_market_type(self, code: str) -> str:
        """종목 코드로 시장 구분 조회"""
        stock = self.stock_info_cache.get(code, {})
        return stock.get("market", "")
    
    def update_price(self, code: str, price: int):
        """현재가 정보 업데이트"""
        self.price_cache[code] = {
            "price": price,
            "timestamp": datetime.now()
        }
    
    def get_price(self, code: str) -> Optional[int]:
        """캐시된 현재가 조회"""
        cache_data = self.price_cache.get(code)
        if cache_data:
            return cache_data["price"]
        return None
    
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
    
    def get_all_symbols(self) -> List[str]:
        """모든 종목 코드 반환"""
        return list(self.stock_info_cache.keys())
    
    def get_filtered_symbols(self, top_kospi: int = 450, top_kosdaq: int = 150) -> List[str]:
        """필터링된 종목 코드 반환"""
        kospi_filtered = self.kospi_symbols[:top_kospi] if len(self.kospi_symbols) > top_kospi else self.kospi_symbols
        kosdaq_filtered = self.kosdaq_symbols[:top_kosdaq] if len(self.kosdaq_symbols) > top_kosdaq else self.kosdaq_symbols
        
        return kospi_filtered + kosdaq_filtered