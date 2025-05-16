import asyncio
import logging
from typing import Dict, List, Any
from datetime import datetime, timedelta
import numpy as np
import pandas as pd
from app.auth.token_manager import TokenManager
from app.api.kiwoom_api import KiwoomAPI

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
        self.token_manager = TokenManager()
        self.kiwoom_api = KiwoomAPI(self.token_manager)
        
        # 매매 관련 설정
        self.max_positions = 5  # 최대 보유 종목 수
        self.trade_amount_per_stock = 5000000  # 종목당 매매 금액 (100만원)
        self.min_holding_period = 0.5  # 최소 보유 기간 (일) - 반나절
        
        # 분할 매매 관련 설정
        self.buy_division_count = 2  # 매수 분할 횟수
        self.sell_division_count = 2  # 매도 분할 횟수
        self.division_interval = 1500  # 분할 매매 간격 (초)
        
        # 분할 매매 진행 상태 추적
        self.division_status = {}  # {symbol: {"buy_count": 0, "sell_count": 0, "last_division": datetime}}
        
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
        
        # 분봉 캐시 초기화
        self.minute_candle_cache = {}     # {symbol: [candle_data]}
        self.last_minute_candle_update = {}  # {symbol: datetime}
        
        # 거래 대금 상위 종목 캐시
        self.top_trading_amount_stocks = []  # 거래 대금 상위 종목 리스트
        self.last_top_amount_update = datetime.min  # 마지막 업데이트 시간
        self.top_amount_update_interval = 3600  # 업데이트 간격 (초) - 1시간으로 변경
        
        # 교차 검증된 매매 대상 종목 리스트
        self.potential_targets = []  # 실제 매매 대상 후보 종목
        self.verified_targets = {}  # {symbol: rank} - 교차 검증된 종목과 순위
        
        logger.info("단타 매매 모델 초기화 완료 (분할 매매 전략 적용)")
        
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
        
        # 서버 시작 시 초기 데이터 로드 - 추가된 부분
        await self.initial_data_load()
        
        # 매매 신호 모니터링 시작
        asyncio.create_task(self.monitor_signals())
        
        # 거래대금 상위 종목 모니터링 시작
        asyncio.create_task(self.monitor_top_volume_stocks())
        
        # 거래 대금 상위 종목 모니터링 시작 (1시간 주기로 변경)
        asyncio.create_task(self.monitor_top_trading_amount_stocks())
    
    async def initial_data_load(self):
        """서버 시작 시 초기 데이터 로드 - 새로 추가된 메서드"""
        logger.info("서버 시작 시 초기 데이터 로드 시작")
        
        try:
            # 거래 대금 상위 종목 초기 로드
            if self.kiwoom_api and hasattr(self.kiwoom_api, "get_all_top_trading_amount"):
                top_stocks = await self.kiwoom_api.get_all_top_trading_amount(limit=100)
                
                if top_stocks and len(top_stocks) > 0:
                    self.top_trading_amount_stocks = top_stocks
                    self.last_top_amount_update = datetime.now()
                    
                    # 교차 검증 실행
                    await self.cross_verify_target_stocks()
                    
                    logger.info(f"초기 거래 대금 상위 종목 로드 완료: {len(top_stocks)}개")
                    logger.info(f"초기 교차 검증된 매매 대상 종목: {len(self.potential_targets)}개")
            
            # 필요한 경우 여기에 추가 초기 데이터 로드 코드 추가
            
        except Exception as e:
            logger.error(f"초기 데이터 로드 중 오류: {str(e)}", exc_info=True)
    
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
            # 교차 검증된 매매 대상 종목 가져오기 (기존 필터링 대신 변경)
            target_stocks = await self.get_cross_verified_target_stocks()
            if not target_stocks:
                logger.warning("교차 검증된 매매 대상 종목이 없어 지표를 갱신할 수 없습니다.")
                return refreshed_count
            
            # 각 종목에 대해 5분봉 데이터 캐시 갱신
            for symbol in target_stocks:
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
    
    async def monitor_top_trading_amount_stocks(self):
        """거래 대금 상위 종목 모니터링 (1시간 주기로 변경)"""
        logger.info("거래 대금 상위 종목 모니터링 시작 (1시간 주기)")
        
        while self.is_running:
            try:
                now = datetime.now()
                
                # 업데이트 필요 여부 확인 (1시간 간격으로 변경)
                if (now - self.last_top_amount_update).total_seconds() >= self.top_amount_update_interval:
                    # 거래 대금 상위 종목 가져오기
                    if self.kiwoom_api and hasattr(self.kiwoom_api, "get_all_top_trading_amount"):
                        top_stocks = await self.kiwoom_api.get_all_top_trading_amount(limit=100)
                        
                        if top_stocks and len(top_stocks) > 0:
                            self.top_trading_amount_stocks = top_stocks
                            self.last_top_amount_update = now
                            
                            # 교차 검증 실행
                            await self.cross_verify_target_stocks()
                            
                            logger.info(f"거래 대금 상위 종목 업데이트 완료: {len(top_stocks)}개")
                            logger.info(f"교차 검증된 매매 대상 종목: {len(self.potential_targets)}개")
                
                # 10분 단위로 체크 (1시간마다 업데이트하므로 자주 확인할 필요 없음)
                await asyncio.sleep(600)
                
            except Exception as e:
                logger.error(f"거래 대금 상위 종목 모니터링 중 오류: {str(e)}", exc_info=True)
                await asyncio.sleep(600)  # 오류 시 10분 후 재시도
    
    async def cross_verify_target_stocks(self):
      try:
          # 1. 필터링된 종목 목록 가져오기 (코스피 450, 코스닥 150)
          filtered_stocks = self.get_filtered_stocks()
          filtered_set = set(filtered_stocks)
          
          if not filtered_stocks:
              logger.warning("필터링된 종목 목록을 가져올 수 없습니다.")
              return
                  
          # 2. 거래 대금 상위 종목 중 필터링된 종목에 포함된 것만 선택
          verified_targets = {}
          potential_targets = []
          
          for rank, stock in enumerate(self.top_trading_amount_stocks, 1):
              symbol = stock.get("code", "")
              
              # '_AL' 접미사가 있으면 제거
              if symbol.endswith('_AL'):
                  symbol = symbol[:-3]  # 마지막 3글자('_AL') 제거
              
              # 필터링된 종목 집합에 있는지 확인
              if symbol in filtered_set:
                  # 교차 검증 통과한 종목 추가
                  potential_targets.append(symbol)
                  verified_targets[symbol] = {
                      "rank": rank,  # 순위
                      "trading_amount": stock.get("trading_amount", 0),  # 거래대금
                      "price": stock.get("price", 0),  # 현재가
                      "change_rate": stock.get("change_rate", 0),  # 등락률
                      "market_type": stock.get("market_type", "")  # 시장 구분
                  }
          
          # 3. 결과 저장
          self.potential_targets = potential_targets
          self.verified_targets = verified_targets
          
          logger.info(f"교차 검증 완료: 거래 대금 상위 {len(self.top_trading_amount_stocks)}개 종목 중 " 
                    f"필터링된 종목({len(filtered_stocks)}개)과 교차 검증 결과 {len(potential_targets)}개 선택")
          
          return potential_targets
              
      except Exception as e:
          logger.error(f"종목 교차 검증 중 오류: {str(e)}", exc_info=True)
          return []
    
    async def get_cross_verified_target_stocks(self, limit=30):
        # 현재 시간 확인
        now = datetime.now()
        
        # 마지막 업데이트로부터 1시간 이상 지났으면 재검증
        if (now - self.last_top_amount_update).total_seconds() >= self.top_amount_update_interval:
            await self.cross_verify_target_stocks()
        
        # 상위 N개만 반환
        return self.potential_targets[:limit]
            
    def get_filtered_stocks(self) -> List[str]:
        """필터링된 종목 목록 가져오기"""
        if self.stock_cache and hasattr(self.stock_cache, "get_filtered_stocks"):
            return self.stock_cache.get_filtered_stocks()
        elif self.stock_cache and hasattr(self.stock_cache, "filtered_stockcode_list"):
            return self.stock_cache.filtered_stockcode_list
        return []
    
    def _get_minute_candles(self, symbol: str) -> List[Dict]:
      if not self.stock_cache:
          return []
      
      # StockCache의 get_minute_chart_data 메서드 사용
      if hasattr(self.stock_cache, "get_minute_chart_data"):
          minute_data = self.stock_cache.get_minute_chart_data(symbol, 5)
          # 최대 50개만 반환
          return minute_data[:50] if len(minute_data) > 50 else minute_data
      
      # 없으면 빈 리스트 반환
      return []
    
    async def monitor_top_volume_stocks(self):
        """거래대금 상위 종목 모니터링 (기존 메서드 수정)"""
        logger.info("거래대금 상위 종목 모니터링 시작")
        
        while self.is_running:
            try:
                # 5분마다 확인
                await asyncio.sleep(300)
                
                # 교차 검증된 매매 대상 종목 가져오기 (기존 top_volume_stocks 대신 변경)
                target_stocks = await self.get_cross_verified_target_stocks()
                
                if target_stocks:
                    logger.info(f"교차 검증된 매매 대상 종목 리스트: {len(target_stocks)}개")
                    
                    # 각 종목에 대해 분석 실행
                    for symbol in target_stocks:
                        # 현재가 가져오기
                        price = 0
                        if self.stock_cache and hasattr(self.stock_cache, "get_price"):
                            price = self.stock_cache.get_price(symbol)
                        
                        # verified_targets에서 종목 정보 가져오기
                        if symbol in self.verified_targets:
                            rank_info = self.verified_targets[symbol]
                            price = rank_info.get("price", price)
                        
                        if price > 0:
                            # 실시간 가격 처리 함수 호출
                            await self.handle_realtime_price(symbol, price)
            
            except Exception as e:
                logger.error(f"거래대금 상위 종목 모니터링 중 오류: {str(e)}", exc_info=True)
                await asyncio.sleep(60)
    
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
                  # 최대 50개로 제한하고 캐시에 저장
                  if len(candle_data) > 50:
                      candle_data = candle_data[:50]
                  
                  # 캐시에 저장
                  self.minute_candle_cache[symbol] = candle_data
                  self.last_minute_candle_update[symbol] = datetime.now()
            
            # 분봉 데이터가 없으면 처리 중단
            if not candle_data or len(candle_data) < 10:
                return
            
            # 현재 시간
            now = datetime.now()
            
            # 교차 검증 정보 - 거래 대금 상위 종목 순위 정보 활용 (추가)
            rank_info = self.verified_targets.get(symbol, {})
            rank = rank_info.get("rank", 0)
            
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
            
            # 4. 거래대금 순위 확인 (추가) - 상위 30위 이내인지 확인
            high_trading_amount = rank > 0 and rank <= 30
            
            # 매수 신호 생성 조건에 거래대금 순위 조건 추가
            if volume_surge and pullback_pattern and ichimoku_signal and high_trading_amount:
                reason = f"거래량 급증 후 눌림목 형성 (거래대금 순위: {rank}위)"
                self._handle_buy_signal(symbol, price, reason, now)
            
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
        """매수 신호 처리 - 분할 매수 적용"""
        # 이미 보유 중인지 확인
        if symbol in self.positions:
            # 분할 매수 진행 중인지 확인
            if symbol in self.division_status:
                division_info = self.division_status[symbol]
                buy_count = division_info.get("buy_count", 0)
                last_division = division_info.get("last_division", datetime.min)
                
                # 최대 분할 횟수에 도달했는지 확인
                if buy_count >= self.buy_division_count:
                    logger.debug(f"종목 {symbol}의 분할 매수 완료 (총 {buy_count}회)")
                    return
                
                # 분할 매수 간격 확인
                seconds_since_last = (now - last_division).total_seconds()
                if seconds_since_last < self.division_interval:
                    logger.debug(f"종목 {symbol}의 다음 분할 매수까지 {self.division_interval - seconds_since_last:.0f}초 남음")
                    return
                
                # 추가 분할 매수 신호 생성
                self.trading_signals[symbol] = {
                    "signal": "additional_buy",
                    "price": price,
                    "timestamp": now,
                    "reason": f"분할 매수 ({buy_count + 1}/{self.buy_division_count}): {reason}",
                    "division": buy_count + 1
                }
                
                logger.info(f"분할 매수 신호 발생: {symbol}, 현재가: {price:.2f}, 회차: {buy_count + 1}/{self.buy_division_count}")
                return
            else:
                # 아직 분할 매수 정보가 없으면 초기화
                logger.debug(f"이미 보유 중인 종목: {symbol}, 분할 매수 상태 초기화")
                self.division_status[symbol] = {
                    "buy_count": 1,  # 이미 1회 매수 완료로 간주
                    "sell_count": 0,
                    "last_division": now
                }
                return
        
        # 마지막 매매 이후 최소 보유 기간(일)이 지났는지 확인
        last_trade = self.trade_history.get(symbol, {})
        last_trade_time = last_trade.get("last_trade", None)
        
        if last_trade_time:
            hours_passed = (now - last_trade_time).total_seconds() / 3600
            if hours_passed < (self.min_holding_period * 24):
                logger.debug(f"최소 보유 기간이 지나지 않음: {symbol}, 경과 시간: {hours_passed:.1f}시간")
                return
        
        # 첫 매수 신호 저장
        self.trading_signals[symbol] = {
            "signal": "buy",
            "price": price,
            "timestamp": now,
            "reason": f"첫 매수 (1/{self.buy_division_count}): {reason}",
            "division": 1
        }
        
        # 분할 매수 상태 초기화
        self.division_status[symbol] = {
            "buy_count": 1,
            "sell_count": 0,
            "last_division": now
        }
        
        logger.info(f"첫 매수 신호 발생: {symbol}, 현재가: {price:.2f}, 이유: {reason}")
    
    def _handle_sell_signal(self, symbol, price, reason, now):
        """매도 신호 처리 - 분할 매도 적용"""
        # 매도 가능한지 확인 (보유 중인 종목인지)
        if symbol not in self.positions:
            logger.debug(f"보유하지 않은 종목 매도 신호 무시: {symbol}")
            return
        
        # 분할 매도 진행 중인지 확인
        if symbol in self.division_status:
            division_info = self.division_status[symbol]
            sell_count = division_info.get("sell_count", 0)
            last_division = division_info.get("last_division", datetime.min)
            buy_count = division_info.get("buy_count", 0)
            
            # 이미 모두 매도했는지 확인
            if sell_count >= self.sell_division_count:
                logger.debug(f"종목 {symbol}의 분할 매도 완료 (총 {sell_count}회)")
                return
            
            # 분할 매도 간격 확인
            seconds_since_last = (now - last_division).total_seconds()
            if seconds_since_last < self.division_interval and sell_count > 0:
                logger.debug(f"종목 {symbol}의 다음 분할 매도까지 {self.division_interval - seconds_since_last:.0f}초 남음")
                return
            
            # 손절 조건인 경우 전량 매도
            position = self.positions.get(symbol, {})
            avg_price = position.get("avg_price", 0)
            
            is_stop_loss = False
            if avg_price > 0 and price < avg_price * (1 - self.stop_loss_pct / 100):
                is_stop_loss = True
                sell_signal = "stop_loss"
                sell_reason = f"손절 조건 충족: -{self.stop_loss_pct}% (전량 매도)"
                quantity = "all"
            else:
                # 일반 분할 매도
                sell_signal = "partial_sell"
                sell_reason = f"분할 매도 ({sell_count + 1}/{self.sell_division_count}): {reason}"
                
                # 마지막 분할이면 잔량 전부, 아니면 보유량의 일정 비율
                if sell_count == self.sell_division_count - 1:
                    quantity = "all"  # 마지막 매도는 전량
                else:
                    # 보유 수량 계산
                    total_quantity = position.get("quantity", 0)
                    # 분할 매도 수량: 총 보유 수량을 분할 횟수로 나눔 (반올림)
                    quantity = round(total_quantity / self.sell_division_count)
                    
                    # 최소 1주 이상
                    if quantity < 1:
                        quantity = 1
            
            # 매도 신호 저장
            self.trading_signals[symbol] = {
                "signal": sell_signal,
                "price": price,
                "timestamp": now,
                "reason": sell_reason,
                "quantity": quantity,
                "division": sell_count + 1
            }
            
            # 분할 매도 상태 업데이트
            self.division_status[symbol]["sell_count"] = sell_count + 1
            self.division_status[symbol]["last_division"] = now
            
            if is_stop_loss:
                logger.info(f"손절 매도 신호 발생: {symbol}, 현재가: {price:.2f}, 평균단가: {avg_price:.2f}, 전량 매도")
            else:
                logger.info(f"분할 매도 신호 발생: {symbol}, 현재가: {price:.2f}, 회차: {sell_count + 1}/{self.sell_division_count}")
        else:
            # 분할 매도 정보가 없으면 초기화 (첫 매도)
            self.division_status[symbol] = {
                "buy_count": 1,  # 이미 매수한 상태로 가정
                "sell_count": 1,
                "last_division": now
            }
            
            # 첫 매도 신호 저장
            self.trading_signals[symbol] = {
                "signal": "sell",
                "price": price,
                "timestamp": now,
                "reason": f"첫 매도 (1/{self.sell_division_count}): {reason}",
                "quantity": "half",  # 첫 매도는 절반
                "division": 1
            }
            
            logger.info(f"첫 매도 신호 발생: {symbol}, 현재가: {price:.2f}, 이유: {reason}, 절반 매도")

    async def monitor_signals(self):
      """매매 신호 주기적 모니터링 및 처리"""
      logger.info("단타 매매 신호 모니터링 시작")
      
      while self.is_running:
          try:
              # 1분마다 매매 신호 확인
              await asyncio.sleep(30)
              
              # 계좌 정보 동기화 (백엔드에서 가져옴)
              if self.backend_client:
                  account_info = await self.backend_client.request_account_info()
                  if account_info:
                      self.update_account_info(account_info)
                      logger.debug("계좌 정보 정기 동기화 완료")
              
              # 매매 신호 로깅
              if self.trading_signals:
                  buy_signals = sum(1 for v in self.trading_signals.values() if v["signal"] == "buy")
                  sell_signals = sum(1 for v in self.trading_signals.values() if v["signal"] == "sell")
                  logger.info(f"현재 매매 신호: 매수={buy_signals}개, 매도={sell_signals}개")
              
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
        """매매 의사결정 목록 반환 - 분할 매매 전략 적용"""
        if not self.is_running:
            logger.warning("트레이딩 모델이 실행 중이지 않습니다.")
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
                reason = signal_info.get("reason", "단타 매매 신호")
                division = signal_info.get("division", 1)
                
                # 신호 유효 시간 (10분) 체크
                if (now - timestamp).total_seconds() > 600:
                    # 오래된 신호 제거
                    del self.trading_signals[symbol]
                    logger.debug(f"종목 {symbol}의 오래된 신호 제거 (10분 경과)")
                    continue
                
                # 매수 신호 처리 (첫 매수)
                if signal == "buy":
                    # 현재 보유 종목 수 확인
                    if len(self.positions) >= self.max_positions:
                        logger.debug(f"최대 보유 종목 수({self.max_positions}) 도달, 매수 보류: {symbol}")
                        continue
                    
                    # 충분한 현금 확인
                    division_amount = self.trade_amount_per_stock / self.buy_division_count
                    if self.cash_balance < division_amount:
                        logger.debug(f"현금 부족({self.cash_balance}), 매수 보류: {symbol}")
                        continue
                    
                    # 이미 보유 중인지 확인 (이중 체크)
                    if symbol in self.positions:
                        logger.debug(f"이미 보유 중인 종목 매수 신호 무시: {symbol}")
                        del self.trading_signals[symbol]
                        continue
                    
                    # 매수 수량 계산 (분할 매수 기준)
                    current_price = price
                    
                    if not current_price or current_price <= 0:
                        if prices and symbol in prices:
                            current_price = prices[symbol]
                        elif self.stock_cache:
                            current_price = self.stock_cache.get_price(symbol)
                            
                        if not current_price or current_price <= 0:
                            logger.warning(f"종목 {symbol}의 가격 정보 없음, 매수 보류")
                            continue
                    
                    # 분할 매수를 위한 수량 계산 (종목당 비중 / 분할 횟수)
                    quantity = int(division_amount / current_price)
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
                        "division": division,
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
                    logger.info(f"매수 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}, 이유: {reason}, 회차: {division}/{self.buy_division_count}")
                
                # 추가 분할 매수 신호 처리
                elif signal == "additional_buy":
                    # 현재 보유 중인지 확인
                    if symbol not in self.positions:
                        logger.debug(f"보유하지 않은 종목 추가 매수 신호 무시: {symbol}")
                        del self.trading_signals[symbol]
                        continue
                    
                    # 충분한 현금 확인
                    division_amount = self.trade_amount_per_stock / self.buy_division_count
                    if self.cash_balance < division_amount:
                        logger.debug(f"현금 부족({self.cash_balance}), 추가 매수 보류: {symbol}")
                        continue
                    
                    # 매수 수량 계산 (분할 매수 기준)
                    current_price = price
                    
                    if not current_price or current_price <= 0:
                        if prices and symbol in prices:
                            current_price = prices[symbol]
                        elif self.stock_cache:
                            current_price = self.stock_cache.get_price(symbol)
                            
                        if not current_price or current_price <= 0:
                            logger.warning(f"종목 {symbol}의 가격 정보 없음, 추가 매수 보류")
                            continue
                    
                    # 분할 매수를 위한 수량 계산
                    quantity = int(division_amount / current_price)
                    if quantity <= 0:
                        logger.warning(f"종목 {symbol}의 추가 매수 수량이 0, 매수 보류")
                        continue
                    
                    # 매수 결정 추가
                    decision = {
                        "symbol": symbol,
                        "action": "buy",
                        "quantity": quantity,
                        "price": current_price,
                        "reason": reason,
                        "division": division,
                        "timestamp": now.isoformat()
                    }
                    decisions.append(decision)
                    
                    # 거래 이력 업데이트
                    self.trade_history[symbol] = {
                        "last_trade": now,
                        "last_action": "additional_buy"
                    }
                    
                    # 신호 제거
                    del self.trading_signals[symbol]
                    logger.info(f"추가 매수 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}, 이유: {reason}, 회차: {division}/{self.buy_division_count}")
                
                # 매도 신호 처리 (분할 매도 및 손절 포함)
                elif signal in ["sell", "partial_sell", "stop_loss"]:
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
                    
                    # 매도 수량
                    sell_quantity = signal_info.get("quantity", "half")
                    
                    if sell_quantity == "all":
                        # 전량 매도
                        sell_quantity = total_quantity
                    elif sell_quantity == "half":
                        # 절반 매도 (첫 분할)
                        sell_quantity = max(1, round(total_quantity / 2))
                    
                    if sell_quantity <= 0:
                        logger.warning(f"종목 {symbol}의 매도 수량이 0, 매도 보류")
                        continue
                    
                    # 실제 보유량보다 크면 전량으로 조정
                    if sell_quantity > total_quantity:
                        sell_quantity = total_quantity
                    
                    # 매도 결정 추가
                    current_price = price
                    
                    if not current_price or current_price <= 0:
                        if prices and symbol in prices:
                            current_price = prices[symbol]
                        elif self.stock_cache:
                            current_price = self.stock_cache.get_price(symbol)
                            
                        if not current_price or current_price <= 0:
                            logger.warning(f"종목 {symbol}의 가격 정보 없음, 매도 보류")
                            continue
                    
                    decision = {
                        "symbol": symbol,
                        "action": "sell",
                        "quantity": sell_quantity,
                        "price": current_price,
                        "reason": reason,
                        "division": division,
                        "timestamp": now.isoformat()
                    }
                    decisions.append(decision)
                    
                    # 거래 이력 업데이트
                    last_action = "sell_all" if sell_quantity == total_quantity else "partial_sell"
                    self.trade_history[symbol] = {
                        "last_trade": now,
                        "last_action": last_action
                    }
                    
                    # 신호 제거
                    del self.trading_signals[symbol]
                    
                    # 매도 완료 로깅
                    if signal == "stop_loss":
                        logger.info(f"손절 매도 결정: {symbol}, {sell_quantity}주, 가격: {current_price:.2f}, 이유: {reason}")
                    else:
                        logger.info(f"분할 매도 결정: {symbol}, {sell_quantity}주, 가격: {current_price:.2f}, 이유: {reason}, 회차: {division}/{self.sell_division_count}")
            
            return decisions
        
        except Exception as e:
            logger.error(f"매매 의사결정 생성 중 오류: {str(e)}", exc_info=True)
            return []

    def update_account_info(self, account_info):
        """계좌 정보 업데이트"""
        if not account_info:
            logger.warning("업데이트할 계좌 정보가 없습니다.")
            return False
        
        try:
            # 현금 잔고 업데이트
            self.cash_balance = account_info.get("cash_balance", 0)
            
            # 보유 종목 업데이트
            self.positions = account_info.get("positions", {})
            
            # 계좌 정보 전체 저장
            self.account_info = account_info
            
            return True
        except Exception as e:
            logger.error(f"계좌 정보 업데이트 중 오류: {str(e)}")
            return False
        
    def _should_process_price_update(self, symbol: str, price: float) -> bool:
      """중복 메시지 필터링 - 최소 가격 변동 비율 또는 최소 처리 간격 이내면 처리하지 않음"""
      now = datetime.now()
      
      # 이전 처리 가격 및 시간 가져오기
      last_price = self.last_processed_prices.get(symbol, 0)
      last_time = self.last_processed_times.get(symbol, datetime.min)
      
      # 최소 처리 간격 확인 (초 단위)
      if (now - last_time).total_seconds() < self.min_process_interval:
          return False
      
      # 최소 가격 변동 비율 확인 (백분율)
      if last_price > 0:
          price_change_pct = abs(price - last_price) / last_price * 100
          if price_change_pct < self.min_price_change_pct:
              return False
      
      # 처리 가격 및 시간 업데이트
      self.last_processed_prices[symbol] = price
      self.last_processed_times[symbol] = now
      
      return True

    def set_backend_client(self, backend_client):
        """백엔드 클라이언트 설정"""
        self.backend_client = backend_client
        logger.info("단타 매매 모델 백엔드 클라이언트 설정 완료")

    def _check_volume_surge(self, candle_data):
      """거래량 급증 확인"""
      try:
          if not candle_data or len(candle_data) < 5:
              return False, None
          
          # 캔들 데이터 형식 확인 및 변환
          recent_candles = candle_data[:5]  # 최근 5개 캔들
          previous_candles = candle_data[5:15]  # 이전 10개 캔들 (평균 계산용)
          
          # 거래량 데이터 추출 (데이터 형식에 따라 처리)
          recent_volumes = []
          for candle in recent_candles:
              if isinstance(candle, dict) and 'volume' in candle:
                  recent_volumes.append(candle['volume'])
              elif isinstance(candle, list) and len(candle) > 5:
                  recent_volumes.append(candle[5])  # 거래량 인덱스
          
          previous_volumes = []
          for candle in previous_candles:
              if isinstance(candle, dict) and 'volume' in candle:
                  previous_volumes.append(candle['volume'])
              elif isinstance(candle, list) and len(candle) > 5:
                  previous_volumes.append(candle[5])  # 거래량 인덱스
          
          # 이전 10개 캔들의 평균 거래량 계산
          if not previous_volumes:
              return False, None
          
          avg_volume = sum(previous_volumes) / len(previous_volumes)
          
          # 최근 5개 캔들 중 급증 여부 확인
          for i, volume in enumerate(recent_volumes):
              # 최소 5배 이상 급증
              if volume > avg_volume * 5:
                  # 가격 상승 확인
                  candle = recent_candles[i]
                  open_price = 0
                  close_price = 0
                  
                  if isinstance(candle, dict):
                      open_price = float(candle.get('open', 0))
                      close_price = float(candle.get('close', 0))
                  elif isinstance(candle, list) and len(candle) > 4:
                      open_price = float(candle[1])  # open 인덱스
                      close_price = float(candle[4])  # close 인덱스
                  
                  # 가격 상승 여부 확인
                  if close_price > open_price:
                      price_increase_pct = ((close_price / open_price) - 1) * 100
                      # 최소 1% 이상 상승했는지 확인
                      if price_increase_pct >= 1.0:
                          return True, candle
          
          return False, None
      except Exception as e:
          logger.error(f"거래량 급증 확인 중 오류: {str(e)}")
          return False, None

    def _check_pullback_pattern(self, candle_data, surge_candle):
      """눌림목 패턴 확인"""
      try:
          if not candle_data or len(candle_data) < 3 or not surge_candle:
              return False
          
          # 급등 캔들 이후의 캔들 추출
          surge_index = -1
          for i, candle in enumerate(candle_data):
              if candle == surge_candle:
                  surge_index = i
                  break
          
          if surge_index < 0 or surge_index >= len(candle_data) - 1:
              return False
          
          # 급등 캔들의 정보
          surge_high = 0
          surge_close = 0
          
          if isinstance(surge_candle, dict):
              surge_high = float(surge_candle.get('high', 0))
              surge_close = float(surge_candle.get('close', 0))
          elif isinstance(surge_candle, list) and len(surge_candle) > 4:
              surge_high = float(surge_candle[2])  # high 인덱스
              surge_close = float(surge_candle[4])  # close 인덱스
          
          # 이후 캔들들의 정보
          after_candles = candle_data[surge_index + 1:surge_index + 3]  # 2개 캔들 확인
          
          lowest_after = float('inf')
          for candle in after_candles:
              low_price = 0
              
              if isinstance(candle, dict):
                  low_price = float(candle.get('low', 0))
              elif isinstance(candle, list) and len(candle) > 3:
                  low_price = float(candle[3])  # low 인덱스
              
              lowest_after = min(lowest_after, low_price)
          
          # 눌림목 패턴: 이후 캔들에서 최저가가 급등 캔들의 고가보다 낮고, 
          # 급등 캔들 종가의 0.5%~3% 이내로 내려왔는지 확인
          if lowest_after < surge_high:
              price_decrease_pct = ((surge_close - lowest_after) / surge_close) * 100
              return 0.5 <= price_decrease_pct <= 3.0
          
          return False
      except Exception as e:
          logger.error(f"눌림목 패턴 확인 중 오류: {str(e)}")
          return False

    def _check_ichimoku_baseline(self, candle_data, current_price):
      """일목균형표 기준선 확인"""
      try:
          if not candle_data or len(candle_data) < 26:
              return False
          
          # 26일 기준선 계산 데이터 추출
          period_candles = candle_data[:26]
          
          # 고가와 저가 추출
          highs = []
          lows = []
          
          for candle in period_candles:
              high_price = 0
              low_price = 0
              
              if isinstance(candle, dict):
                  high_price = float(candle.get('high', 0))
                  low_price = float(candle.get('low', 0))
              elif isinstance(candle, list) and len(candle) > 3:
                  high_price = float(candle[2])  # high 인덱스
                  low_price = float(candle[3])  # low 인덱스
              
              highs.append(high_price)
              lows.append(low_price)
          
          # 기준선 계산 (26일 고가와 저가의 평균)
          highest = max(highs)
          lowest = min(lows)
          
          kijun_sen = (highest + lowest) / 2
          
          # 현재가가 기준선 근처인지 확인 (±0.5%)
          price_diff_pct = abs((current_price - kijun_sen) / kijun_sen) * 100
          
          return price_diff_pct <= 0.5
      except Exception as e:
          logger.error(f"일목균형표 기준선 확인 중 오류: {str(e)}")
          return False

    def _check_take_profit(self, candle_data, current_price, avg_price):
      """이익 실현 조건 확인"""
      try:
          if not candle_data or len(candle_data) < 5 or avg_price <= 0:
              return False
          
          # 수익률 계산
          profit_pct = ((current_price / avg_price) - 1) * 100
          
          # 최소 5% 이상 수익 발생 확인
          if profit_pct < 5.0:
              return False
          
          # 최근 고점 확인
          recent_candles = candle_data[:5]
          highest = 0
          
          for candle in recent_candles:
              high_price = 0
              
              if isinstance(candle, dict):
                  high_price = float(candle.get('high', 0))
              elif isinstance(candle, list) and len(candle) > 2:
                  high_price = float(candle[2])  # high 인덱스
              
              highest = max(highest, high_price)
          
          # 고점에서 2% 이상 하락 확인
          if highest > current_price:
              drop_pct = ((highest - current_price) / highest) * 100
              return drop_pct >= 2.0
          
          return False
      except Exception as e:
          logger.error(f"이익 실현 조건 확인 중 오류: {str(e)}")
          return False