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
        """종목 정보 초기화 - 전달받은 종목을 모두 캐싱"""
        try:
            # 종목 정보 캐시 초기화
            self.stock_info_cache.clear()
            self.kospi_symbols.clear()
            self.kosdaq_symbols.clear()
            
            skipped_count = 0
            logger.info(f"종목 정보 초기화 시작: {len(stock_list)}개 종목")
            
            # 종목 정보 캐싱 - 누락된 종목 코드만 건너뛰고 나머지는 모두 저장
            for stock in stock_list:
                code = stock.get("shortCode")
                if not code:
                    skipped_count += 1
                    logger.warning(f"종목 코드가 누락된 데이터 건너뜀: {stock}")
                    continue  # 종목 코드가 없는 경우만 건너뜀
                
                # 나머지 정보는 있는 그대로 저장 (기본값 필요 없음)
                self.stock_info_cache[code] = {
                    "shortCode": code,
                    "name": stock.get("shortName", ""),
                    "market": stock.get("market", ""),
                    "stockType": stock.get("stockType", ""),
                    "faceValue": stock.get("faceValue", "0")
                }
                
                # 코스피/코스닥 분류 - 대소문자 구분 없이 확인
                market = stock.get("market", "").upper()
                if "KOSPI" in market:
                    self.kospi_symbols.append(code)
                elif "KOSDAQ" in market:
                    self.kosdaq_symbols.append(code)
                else:
                    logger.warning(f"알 수 없는 시장 유형: {code} - {market}")
            
            unknown_market_count = len(self.stock_info_cache) - len(self.kospi_symbols) - len(self.kosdaq_symbols)
            
            logger.info(f"종목 정보 캐시 초기화 완료: 총 {len(self.stock_info_cache)}개 종목")
            logger.info(f"코스피 종목 수: {len(self.kospi_symbols)}, 코스닥 종목 수: {len(self.kosdaq_symbols)}, 기타: {unknown_market_count}개")
            
            if skipped_count > 0:
                logger.warning(f"종목 코드 누락으로 건너뛴 데이터: {skipped_count}개")
            
            return True
                
        except Exception as e:
            logger.error(f"종목 정보 캐시 초기화 중 오류: {str(e)}")
            return False
    
    def get_stock_name(self, code: str) -> str:
        """종목 코드로 종목명 조회"""
        stock = self.stock_info_cache.get(code, {})
        return stock.get("name", "")
    
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