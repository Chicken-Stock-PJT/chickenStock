import pandas as pd
import numpy as np
import logging
import asyncio
import os
import json
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional

logger = logging.getLogger(__name__)

class ChartDataProcessor:
    def __init__(self):
        """차트 데이터 처리기 초기화"""
        # Envelope 전략 파라미터
        self.envelope_percentage = 0.2  # Envelope 상/하한 범위 (20%)
        
        # 차트 데이터 캐시 (종목 코드 -> 지표 데이터)
        self.indicators_cache = {}  # 종목 코드 -> Envelope 지표 (중앙선, 상한선, 하한선)
        self.last_cache_update = None  # 마지막 캐시 업데이트 시간
        
        # 캐시 저장 파일 경로
        self.cache_dir = os.path.join(os.getcwd(), "cache")
        self.cache_file = os.path.join(self.cache_dir, "envelope_indicators.json")
        
        # 캐시 디렉토리 생성
        os.makedirs(self.cache_dir, exist_ok=True)
        
        # 저장된 캐시 로드
        self._load_cache()
    
    def _load_cache(self):
        """저장된 캐시 파일 로드"""
        try:
            if os.path.exists(self.cache_file):
                with open(self.cache_file, 'r', encoding='utf-8') as f:
                    cache_data = json.load(f)
                    
                    # 캐시 데이터 복원
                    self.indicators_cache = cache_data.get('indicators', {})
                    
                    # 마지막 업데이트 시간 복원
                    last_update_str = cache_data.get('last_update')
                    if last_update_str:
                        self.last_cache_update = datetime.fromisoformat(last_update_str)
                    
                    logger.info(f"Loaded envelope indicators cache for {len(self.indicators_cache)} symbols")
        except Exception as e:
            logger.error(f"Error loading cache: {str(e)}")
            # 오류 발생 시 새 캐시 시작
            self.indicators_cache = {}
            self.last_cache_update = None
    
    def _save_cache(self):
        """캐시 데이터 파일로 저장"""
        try:
            cache_data = {
                'indicators': self.indicators_cache,
                'last_update': self.last_cache_update.isoformat() if self.last_cache_update else None
            }
            
            with open(self.cache_file, 'w', encoding='utf-8') as f:
                json.dump(cache_data, f)
            
            logger.info(f"Saved envelope indicators cache for {len(self.indicators_cache)} symbols")
        except Exception as e:
            logger.error(f"Error saving cache: {str(e)}")
    
    def is_cache_valid(self):
        """캐시가 유효한지 확인 (당일 데이터인지)"""
        if self.last_cache_update is None:
            return False
            
        now = datetime.now()
        # 같은 날짜인지 확인 (일봉 기준으로 캐시는 하루 단위로 유효)
        return now.date() == self.last_cache_update.date()
    
    async def preload_envelope_indicators(self, symbols: List[str], kiwoom_api):
        """여러 종목의 Envelope 지표를 한 번에 미리 계산하여 캐싱"""
        logger.info(f"Preloading envelope indicators for {len(symbols)} symbols")
        self.last_cache_update = datetime.now()
        
        # 캐시가 이미 유효하면 스킵
        if self.is_cache_valid() and len(self.indicators_cache) > 0:
            logger.info("Using existing valid cache for envelope indicators")
            return len(self.indicators_cache)
        
        # 배치 처리를 위한 설정
        batch_size = 30
        success_count = 0
        total_batches = (len(symbols) + batch_size - 1) // batch_size
        
        for batch_idx in range(total_batches):
            start_idx = batch_idx * batch_size
            end_idx = min(start_idx + batch_size, len(symbols))
            current_batch = symbols[start_idx:end_idx]
            
            # 배치 내 모든 종목 병렬 처리
            tasks = []
            for symbol in current_batch:
                task = self._calculate_indicator_for_symbol(symbol, kiwoom_api)
                tasks.append(task)
            
            # 모든 태스크 완료 대기
            batch_results = await asyncio.gather(*tasks, return_exceptions=True)
            
            # 결과 처리
            for i, result in enumerate(batch_results):
                if isinstance(result, Exception):
                    logger.error(f"Error calculating envelope for {current_batch[i]}: {str(result)}")
                elif result:
                    success_count += 1
            
            # 진행 상황 로깅
            logger.info(f"Preloaded envelope indicators: batch {batch_idx+1}/{total_batches}, progress: {success_count}/{len(symbols)}")
            
            # API 부하 방지를 위한 짧은 대기
            await asyncio.sleep(0.5)
        
        # 캐시 저장
        self._save_cache()
            
        logger.info(f"Completed preloading envelope indicators. Cached {success_count}/{len(symbols)} symbols")
        return success_count
    
    async def _calculate_indicator_for_symbol(self, symbol: str, kiwoom_api) -> bool:
        """REST API를 사용하여 종목의 일봉 데이터 요청 및 Envelope 지표 계산"""
        try:
            # 일봉 데이터 요청 - 처음 호출 시에만 force_reload=True, 이후에는 캐시 사용
            # KiwoomAPI의 차트 데이터 캐시를 활용
            is_first_load = not self.is_cache_valid() or symbol not in self.indicators_cache
            chart_data = await kiwoom_api.get_daily_chart_data(
                symbol, 
                period=60, 
                force_reload=is_first_load
            )
            
            if not chart_data or len(chart_data) < 20:  # 최소 20일치 데이터 필요
                logger.warning(f"Insufficient data for {symbol}: {len(chart_data) if chart_data else 0} days")
                return False
            
            # 데이터프레임 생성
            df = pd.DataFrame(chart_data)
            
            # 20일 이동평균선 계산
            df['ma20'] = df['close'].rolling(window=20).mean()
            
            # 최신 데이터의 MA20 가져오기
            ma20 = df['ma20'].iloc[0]  # 내림차순 정렬된 데이터에서 첫 번째가 최신
            
            # Envelope 상하한 계산 (20%)
            upper_band = ma20 * (1 + self.envelope_percentage)
            lower_band = ma20 * (1 - self.envelope_percentage)
            
            # 최신 날짜와 종가
            latest_date = df['date'].iloc[0]
            last_close = df['close'].iloc[0]
            
            # 캐시에 저장
            self.indicators_cache[symbol] = {
                "MA20": ma20,
                "upperBand": upper_band,
                "lowerBand": lower_band,
                "date": latest_date,
                "lastPrice": last_close,
                "currentPrice": last_close  # 초기값은 차트의 마지막 가격
            }
            
            logger.debug(f"Calculated envelope for {symbol}: MA20={ma20:.2f}, Upper={upper_band:.2f}, Lower={lower_band:.2f}")
            return True
            
        except Exception as e:
            logger.error(f"Error calculating envelope for {symbol}: {str(e)}")
            return False
    
    def get_envelope_indicators(self, symbol: str, current_price: float = None) -> Dict[str, Any]:
        """종목의 Envelope 지표 조회 (캐시 사용)"""
        # 캐시에 데이터가 있는지 확인
        if symbol in self.indicators_cache:
            # 캐시된 데이터 복사
            indicators = self.indicators_cache[symbol].copy()
            
            # 실시간 가격 업데이트 (현재 API에서 받아온 실시간 가격)
            if current_price is not None:
                indicators["currentPrice"] = current_price
            
            return indicators
        
        logger.warning(f"No cached envelope indicators for {symbol}")
        return None
    
    def clear_cache(self):
        """캐시 데이터 초기화"""
        self.indicators_cache.clear()
        self.last_cache_update = None
        logger.info("Envelope indicators cache cleared")