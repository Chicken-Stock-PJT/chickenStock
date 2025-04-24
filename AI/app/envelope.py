import numpy as np
import pandas as pd
from typing import Dict, Any, List, Optional
import logging
import asyncio
from datetime import datetime, timedelta

logger = logging.getLogger(__name__)

class ChartDataProcessor:
    def __init__(self):
        """차트 데이터 처리기 초기화"""
        # Envelope 전략 파라미터
        self.envelope_percentage = 0.2  # Envelope 상/하한 범위 (20%)
        
        # 차트 데이터 캐시 (종목 코드 -> 지표 데이터)
        self.envelope_cache = {}  # 종목 코드 -> Envelope 지표 (중앙선, 상한선, 하한선)
        self.last_cache_update = None  # 마지막 캐시 업데이트 시간
        
    async def preload_envelope_indicators(self, symbols: List[str], kiwoom_api):
        """여러 종목의 Envelope 지표를 한 번에 미리 계산하여 캐싱"""
        logger.info(f"Preloading envelope indicators for {len(symbols)} symbols")
        self.last_cache_update = datetime.now()
        
        # 30개씩 배치 처리
        batch_size = 30
        success_count = 0
        
        for i in range(0, len(symbols), batch_size):
            batch = symbols[i:i+batch_size]
            
            # 배치 내 각 종목에 대해 처리
            for symbol in batch:
                try:
                    # 일봉 데이터 요청 및 지표 계산
                    result = await self._calculate_indicator_for_symbol(symbol, kiwoom_api)
                    if result:
                        success_count += 1
                except Exception as e:
                    logger.error(f"Error loading envelope indicator for {symbol}: {str(e)}")
            
            logger.info(f"Preloaded envelope indicators batch {i//batch_size + 1}/{(len(symbols)-1)//batch_size + 1}")
            await asyncio.sleep(1)  # API 부하 방지를 위한 지연
            
        logger.info(f"Completed preloading envelope indicators. Cached {success_count}/{len(symbols)} symbols")
        return success_count
    
    async def _calculate_indicator_for_symbol(self, symbol: str, kiwoom_api) -> bool:
        """키움 API를 사용하여 종목의 일봉 데이터 요청 및 Envelope 지표 계산"""
        try:
            # 일봉 데이터 요청 (TR: opt10081 - 주식일봉차트조회요청)
            df = await self._get_daily_chart_data(symbol, kiwoom_api)
            
            if df is None or len(df) < 20:  # 최소 20일치 데이터 필요
                logger.warning(f"Insufficient data for {symbol}")
                return False
            
            # 20일 이동평균선 계산
            df['ma20'] = df['close'].rolling(window=20).mean()
            
            # 최신 데이터의 MA20 가져오기
            ma20 = df['ma20'].iloc[-1]
            
            # Envelope 상하한 계산 (20%)
            upper_band = ma20 * (1 + self.envelope_percentage)
            lower_band = ma20 * (1 - self.envelope_percentage)
            
            # 최신 날짜와 종가
            latest_date = df.index[-1].strftime("%Y-%m-%d")
            last_close = df['close'].iloc[-1]
            
            # 캐시에 저장
            self.envelope_cache[symbol] = {
                "MA20": ma20,
                "upperBand": upper_band,
                "lowerBand": lower_band,
                "date": latest_date,
                "chart_last_price": last_close,
                "currentPrice": last_close  # 초기값은 차트의 마지막 가격
            }
            
            logger.debug(f"Calculated envelope for {symbol}: MA20={ma20:.2f}, Upper={upper_band:.2f}, Lower={lower_band:.2f}")
            return True
            
        except Exception as e:
            logger.error(f"Error calculating envelope for {symbol}: {str(e)}")
            return False
    
    async def _get_daily_chart_data(self, symbol: str, kiwoom_api):
        """키움 API를 사용하여 일봉 데이터 요청"""
        try:
            # TR 요청 준비
            kiwoom_api.kiwoom.dynamicCall("SetInputValue(QString, QString)", "종목코드", symbol)
            kiwoom_api.kiwoom.dynamicCall("SetInputValue(QString, QString)", "기준일자", datetime.now().strftime("%Y%m%d"))
            kiwoom_api.kiwoom.dynamicCall("SetInputValue(QString, QString)", "수정주가구분", "1")  # 수정주가 사용
            
            # TR 요청
            rq_name = "일봉데이터조회"
            tr_code = "opt10081"
            screen_no = "0101"
            
            # 요청 및 응답 대기를 위한 콜백 설정
            chart_data = []
            
            def on_receive_tr_data(scrno, rqname, trcode, recordname, prev_next, datalength, errorcode, message, splmmsg):
                nonlocal chart_data
                
                if rqname == "일봉데이터조회":
                    count = kiwoom_api.kiwoom.dynamicCall("GetRepeatCnt(QString, QString)", trcode, rqname)
                    
                    for i in range(count):
                        date = kiwoom_api.kiwoom.dynamicCall("GetCommData(QString, QString, int, QString)", 
                                                           trcode, rqname, i, "일자").strip()
                        open_price = int(kiwoom_api.kiwoom.dynamicCall("GetCommData(QString, QString, int, QString)", 
                                                              trcode, rqname, i, "시가").strip())
                        high_price = int(kiwoom_api.kiwoom.dynamicCall("GetCommData(QString, QString, int, QString)", 
                                                              trcode, rqname, i, "고가").strip())
                        low_price = int(kiwoom_api.kiwoom.dynamicCall("GetCommData(QString, QString, int, QString)", 
                                                             trcode, rqname, i, "저가").strip())
                        close_price = int(kiwoom_api.kiwoom.dynamicCall("GetCommData(QString, QString, int, QString)", 
                                                               trcode, rqname, i, "현재가").strip())
                        volume = int(kiwoom_api.kiwoom.dynamicCall("GetCommData(QString, QString, int, QString)", 
                                                         trcode, rqname, i, "거래량").strip())
                        
                        chart_data.append({
                            'date': date,
                            'open': open_price,
                            'high': high_price,
                            'low': low_price,
                            'close': abs(close_price),  # 음수 값이 올 수 있으므로 절대값 처리
                            'volume': volume
                        })
                
                # 이벤트 루프 종료
                kiwoom_api.loop.exit()
            
            # 콜백 연결
            prev_handler = kiwoom_api.kiwoom.OnReceiveTrData.disconnect()
            kiwoom_api.kiwoom.OnReceiveTrData.connect(on_receive_tr_data)
            
            # 요청 전송
            ret = kiwoom_api.kiwoom.dynamicCall("CommRqData(QString, QString, int, QString)", 
                                              [rq_name, tr_code, 0, screen_no])
            
            if ret != 0:
                logger.error(f"일봉 데이터 요청 실패: {ret}")
                kiwoom_api.kiwoom.OnReceiveTrData.disconnect()
                if prev_handler:
                    kiwoom_api.kiwoom.OnReceiveTrData.connect(prev_handler)
                return None
            
            # 응답 대기
            kiwoom_api.loop.exec_()
            
            # 콜백 연결 복원
            kiwoom_api.kiwoom.OnReceiveTrData.disconnect()
            if prev_handler:
                kiwoom_api.kiwoom.OnReceiveTrData.connect(prev_handler)
            
            # 데이터 확인
            if not chart_data:
                logger.warning(f"No daily chart data received for {symbol}")
                return None
            
            # 데이터프레임 생성
            df = pd.DataFrame(chart_data)
            df['date'] = pd.to_datetime(df['date'], format='%Y%m%d')
            df = df.set_index('date')
            df = df.sort_index()  # 날짜 순으로 정렬
            
            return df
            
        except Exception as e:
            logger.error(f"Error getting daily chart data for {symbol}: {str(e)}")
            return None
    
    def get_envelope_indicators(self, symbol: str, current_price: float = None) -> Dict[str, Any]:
        """종목의 Envelope 지표 조회 (캐시 사용)"""
        # 캐시에 데이터가 있는지 확인
        if symbol in self.envelope_cache:
            # 캐시된 데이터 복사
            indicators = self.envelope_cache[symbol].copy()
            
            # 실시간 가격 업데이트 (현재 API에서 받아온 실시간 가격)
            if current_price is not None:
                indicators["currentPrice"] = current_price
            
            return indicators
        
        logger.warning(f"No cached envelope indicators for {symbol}")
        return None
    
    def is_cache_valid(self):
        """캐시가 유효한지 확인 (당일 데이터인지)"""
        if self.last_cache_update is None:
            return False
            
        now = datetime.now()
        # 같은 날짜인지 확인 (일봉 기준으로 캐시는 하루 단위로 유효)
        return now.date() == self.last_cache_update.date()
    
    def clear_cache(self):
        """캐시 데이터 초기화"""
        self.envelope_cache.clear()
        self.last_cache_update = None
        logger.info("Envelope indicators cache cleared")