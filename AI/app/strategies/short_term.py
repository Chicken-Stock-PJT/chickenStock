import asyncio
import logging
from typing import Dict, List, Any, Optional, Tuple, Union, Set
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
        # 기본 컴포넌트 설정
        self.stock_cache = stock_cache
        self.backend_client = backend_client
        self.is_running = False
        self.token_manager = TokenManager()
        self.kiwoom_api = KiwoomAPI(self.token_manager)
        
        # 매매 전략 설정
        self._init_trading_settings()
        
        # 상태 저장 컨테이너 초기화
        self._init_state_containers()
        
        logger.info("단타 매매 모델 초기화 완료 (분할 매매 전략 적용)")
    
    def _init_trading_settings(self) -> None:
        """매매 관련 설정 초기화"""
        # 매매 기본 설정
        self.max_positions = 7  # 최대 보유 종목 수
        self.trade_amount_per_stock = 14000000  # 종목당 매매 금액 (500만원)
        
        # 분할 매매 관련 설정
        self.buy_division_count = 2  # 매수 분할 횟수
        self.sell_division_count = 2  # 매도 분할 횟수
        self.division_interval = 600  # 분할 매매 간격 (초)
        
        # 중복 메시지 필터링 설정
        self.min_price_change_pct = 0.1  # 최소 가격 변동 비율 (0.1%)
        self.min_process_interval = 5    # 최소 처리 간격 (초)
        
        # 기술적 지표 매개변수
        self.volume_surge_threshold = 500  # 평균 거래량 대비 최소 증가율 (%)
        self.min_price_increase = 3.0     # 거래량 급증 시 최소 가격 상승률 (%)
        self.pullback_threshold = 0.5     # 눌림목 판단 기준 (%)
        self.stop_loss_pct = 3.0          # 손절 기준 (%)
        self.additional_buy_drop_pct = 2.0  # 추가 매수 기준 하락률 (%)
        
        # 데이터 업데이트 간격 설정
        self.top_amount_update_interval = 3600  # 거래대금 상위 종목 업데이트 간격 (초) - 1시간
    
    def _init_state_containers(self) -> None:
        """상태 저장 컨테이너 초기화"""
        # 분할 매매 상태 추적
        self.division_status: Dict[str, Dict[str, Any]] = {}  # {symbol: {"buy_count": 0, "sell_count": 0, "last_division": datetime}}
        
        # 매매 신호 및 이력 저장
        self.trading_signals: Dict[str, Dict[str, Any]] = {}  # {symbol: {"signal": "buy/sell", "price": price, "timestamp": datetime}}
        self.trade_history: Dict[str, Dict[str, Any]] = {}  # {symbol: {"last_trade": datetime, "last_action": "buy"}}
        
        # 중복 처리 방지 변수
        self.last_processed_prices: Dict[str, float] = {}  # {symbol: price}
        self.last_processed_times: Dict[str, datetime] = {}   # {symbol: datetime}
        
        # 계좌 정보 관리
        self.account_info: Dict[str, Any] = {}
        self.positions: Dict[str, Dict[str, Any]] = {}
        self.cash_balance: float = 0
        
        # 데이터 캐시 초기화
        self.minute_candle_cache: Dict[str, List[Any]] = {}     # {symbol: [candle_data]}
        self.last_minute_candle_update: Dict[str, datetime] = {}  # {symbol: datetime}
        
        # 거래 대금 상위 종목 캐시
        self.top_trading_amount_stocks: List[Dict[str, Any]] = []  # 거래 대금 상위 종목 리스트
        self.last_top_amount_update: datetime = datetime.min  # 마지막 업데이트 시간
        
        # 교차 검증된 매매 대상 종목
        self.potential_targets: List[str] = []  # 실제 매매 대상 후보 종목
        self.verified_targets: Dict[str, Dict[str, Any]] = {}  # {symbol: {"rank": rank, ...}}
    
    async def start(self) -> None:
        """트레이딩 모델 시작"""
        if self.is_running:
            logger.warning("트레이딩 모델이 이미 실행 중입니다.")
            return
        
        self.is_running = True
        logger.info("단타 매매 모델 시작")
        
        try:
            # 계좌 정보 초기 동기화
            await self._sync_account_info()
            
            # 서버 시작 시 초기 데이터 로드
            await self.initial_data_load()
            
            # 모니터링 태스크 시작
            asyncio.create_task(self.monitor_signals())
            asyncio.create_task(self.monitor_top_volume_stocks())
            asyncio.create_task(self.monitor_top_trading_amount_stocks())
            
        except Exception as e:
            logger.error(f"트레이딩 모델 시작 중 오류: {str(e)}", exc_info=True)
            self.is_running = False
    
    async def _sync_account_info(self) -> None:
        """계좌 정보 동기화"""
        if self.backend_client:
            account_info = await self.backend_client.request_account_info()
            if account_info:
                self.update_account_info(account_info)
                logger.info("계좌 정보 동기화 완료")
    
    async def initial_data_load(self) -> None:
        """서버 시작 시 초기 데이터 로드"""
        logger.info("서버 시작 시 초기 데이터 로드 시작")
        
        try:
            # 거래 대금 상위 종목 초기 로드
            if not hasattr(self.kiwoom_api, "get_all_top_trading_amount"):
                logger.warning("KiwoomAPI에 get_all_top_trading_amount 메서드가 없습니다.")
                return
                
            top_stocks = await self.kiwoom_api.get_all_top_trading_amount(limit=100)
            
            if not top_stocks:
                logger.warning("거래 대금 상위 종목을 가져오지 못했습니다.")
                return
                
            self.top_trading_amount_stocks = top_stocks
            self.last_top_amount_update = datetime.now()
            
            # 교차 검증 실행
            await self.cross_verify_target_stocks()
            
            logger.info(f"초기 거래 대금 상위 종목 로드 완료: {len(top_stocks)}개")
            logger.info(f"초기 교차 검증된 매매 대상 종목: {len(self.potential_targets)}개")
            
        except Exception as e:
            logger.error(f"초기 데이터 로드 중 오류: {str(e)}", exc_info=True)
    
    async def stop(self) -> None:
        """트레이딩 모델 중지"""
        self.is_running = False
        logger.info("단타 매매 모델 중지")
    
    async def refresh_indicators(self) -> int:
        """지표 갱신 (데이터 갱신 후 호출)"""
        logger.info("단타 매매 지표 갱신")
        refreshed_count = 0
        
        if not self.stock_cache:
            logger.warning("StockCache가 설정되지 않아 지표를 갱신할 수 없습니다.")
            return refreshed_count
        
        try:
            # 교차 검증된 매매 대상 종목 가져오기
            target_stocks = await self.get_cross_verified_target_stocks()
            if not target_stocks:
                logger.warning("교차 검증된 매매 대상 종목이 없어 지표를 갱신할 수 없습니다.")
                return refreshed_count
            
            # 각 종목에 대해 분봉 데이터 캐시 갱신
            for symbol in target_stocks:
                try:
                    # 분봉 데이터 가져오기
                    minute_candles = self._get_minute_candles(symbol)
                    if minute_candles and len(minute_candles) >= 10:
                        # 캐시에 저장
                        self.minute_candle_cache[symbol] = minute_candles
                        self.last_minute_candle_update[symbol] = datetime.now()
                        refreshed_count += 1
                except Exception as e:
                    logger.error(f"종목 {symbol}의 분봉 데이터 갱신 중 오류: {str(e)}")
            
            logger.info(f"단타 매매 지표 갱신 완료: {refreshed_count}개 종목")
            return refreshed_count
            
        except Exception as e:
            logger.error(f"단타 매매 지표 갱신 중 오류: {str(e)}")
            return refreshed_count
    
    async def monitor_top_trading_amount_stocks(self) -> None:
        """거래 대금 상위 종목 모니터링 (1시간 주기)"""
        logger.info("거래 대금 상위 종목 모니터링 시작 (1시간 주기)")
        
        while self.is_running:
            try:
                now = datetime.now()
                
                # 업데이트 필요 여부 확인 (1시간 간격)
                time_since_update = (now - self.last_top_amount_update).total_seconds()
                
                if time_since_update >= self.top_amount_update_interval:
                    await self._update_top_trading_stocks()
                
                # 10분 단위로 체크
                await asyncio.sleep(600)
                
            except Exception as e:
                logger.error(f"거래 대금 상위 종목 모니터링 중 오류: {str(e)}", exc_info=True)
                await asyncio.sleep(600)  # 오류 시 10분 후 재시도
    
    async def _update_top_trading_stocks(self) -> None:
        """거래 대금 상위 종목 업데이트"""
        if not hasattr(self.kiwoom_api, "get_all_top_trading_amount"):
            logger.warning("KiwoomAPI에 get_all_top_trading_amount 메서드가 없습니다.")
            return
            
        top_stocks = await self.kiwoom_api.get_all_top_trading_amount(limit=100)
        
        if not top_stocks:
            logger.warning("거래 대금 상위 종목을 가져오지 못했습니다.")
            return
            
        self.top_trading_amount_stocks = top_stocks
        self.last_top_amount_update = datetime.now()
        
        # 교차 검증 실행
        await self.cross_verify_target_stocks()
        
        logger.info(f"거래 대금 상위 종목 업데이트 완료: {len(top_stocks)}개")
        logger.info(f"교차 검증된 매매 대상 종목: {len(self.potential_targets)}개")
    
    async def cross_verify_target_stocks(self) -> List[str]:
        """거래 대금 상위 종목과 필터링된 종목 간 교차 검증"""
        try:
            # 1. 필터링된 종목 목록 가져오기
            filtered_stocks = self.get_filtered_stocks()
            filtered_set = set(filtered_stocks)
            
            if not filtered_stocks:
                logger.warning("필터링된 종목 목록을 가져올 수 없습니다.")
                return []
                    
            # 2. 거래 대금 상위 종목 중 필터링된 종목에 포함된 것만 선택
            verified_targets = {}
            potential_targets = []
            
            for rank, stock in enumerate(self.top_trading_amount_stocks, 1):
                symbol = stock.get("code", "")
                
                # '_AL' 접미사가 있으면 제거
                if symbol.endswith('_AL'):
                    symbol = symbol[:-3]
                
                # 필터링된 종목 집합에 있는지 확인
                if symbol in filtered_set:
                    # 교차 검증 통과한 종목 추가
                    potential_targets.append(symbol)
                    verified_targets[symbol] = {
                        "rank": rank,
                        "trading_amount": stock.get("trading_amount", 0),
                        "price": stock.get("price", 0),
                        "change_rate": stock.get("change_rate", 0),
                        "market_type": stock.get("market_type", "")
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
    
    async def get_cross_verified_target_stocks(self, limit: int = 30) -> List[str]:
        """교차 검증된 매매 대상 종목 반환"""
        # 현재 시간 확인
        now = datetime.now()
        
        # 마지막 업데이트로부터 1시간 이상 지났으면 재검증
        if (now - self.last_top_amount_update).total_seconds() >= self.top_amount_update_interval:
            await self.cross_verify_target_stocks()
        
        # 상위 N개만 반환
        return self.potential_targets[:limit]
            
    def get_filtered_stocks(self) -> List[str]:
        """필터링된 종목 목록 가져오기"""
        if not self.stock_cache:
            return []
            
        if hasattr(self.stock_cache, "get_filtered_stocks"):
            return self.stock_cache.get_filtered_stocks()
        elif hasattr(self.stock_cache, "filtered_stockcode_list"):
            return self.stock_cache.filtered_stockcode_list
            
        return []
    
    def _get_minute_candles(self, symbol: str) -> List[Dict[str, Any]]:
        """종목의 분봉 데이터 가져오기"""
        if not self.stock_cache:
            return []
        
        # StockCache의 get_minute_chart_data 메서드 사용
        if hasattr(self.stock_cache, "get_minute_chart_data"):
            minute_data = self.stock_cache.get_minute_chart_data(symbol, 5)
            # 최대 50개만 반환
            return minute_data[:50] if minute_data and len(minute_data) > 50 else minute_data
        
        # 없으면 빈 리스트 반환
        return []
    
    async def monitor_top_volume_stocks(self) -> None:
        """거래대금 상위 종목 모니터링"""
        logger.info("거래대금 상위 종목 모니터링 시작")
        
        while self.is_running:
            try:
                # 5분마다 확인
                await asyncio.sleep(300)
                await self._check_additional_buy_opportunity()
                
                # 교차 검증된 매매 대상 종목 가져오기
                target_stocks = await self.get_cross_verified_target_stocks()
                
                if not target_stocks:
                    continue
                    
                logger.info(f"교차 검증된 매매 대상 종목 리스트: {len(target_stocks)}개")
                
                # 각 종목에 대해 분석 실행
                for symbol in target_stocks:
                    price = self._get_current_price(symbol)
                    
                    if price > 0:
                        # 실시간 가격 처리
                        await self.handle_realtime_price(symbol, price)
            
            except Exception as e:
                logger.error(f"거래대금 상위 종목 모니터링 중 오류: {str(e)}", exc_info=True)
                await asyncio.sleep(60)
    
    def _get_current_price(self, symbol: str) -> float:
        """종목의 현재가 조회"""
        price = 0
        
        # 1. 캐시에서 가격 조회
        if self.stock_cache and hasattr(self.stock_cache, "get_price"):
            price = self.stock_cache.get_price(symbol)
        
        # 2. 거래대금 상위 종목 정보에서 가격 조회
        if price <= 0 and symbol in self.verified_targets:
            price = self.verified_targets[symbol].get("price", 0)
            
        return price
    
    async def handle_realtime_price(self, symbol: str, price: float, indicators: Dict[str, Any] = None) -> None:
        """실시간 가격 데이터 처리"""
        if not self.is_running:
            return
        
        try:
            # 중복 메시지 필터링
            if not self._should_process_price_update(symbol, price):
                return
            
            # 분봉 데이터 확인
            candle_data = await self._get_or_update_candle_data(symbol)
            
            # 분봉 데이터가 없으면 처리 중단
            if not candle_data or len(candle_data) < 10:
                return
            
            # 현재 시간
            now = datetime.now()
            
            # 교차 검증 정보 - 거래 대금 상위 종목 순위 정보 활용
            rank_info = self.verified_targets.get(symbol, {})
            rank = rank_info.get("rank", 0)
            
            # 이미 indicators가 전달되었으면 사용
            if indicators and 'short_term' in indicators:
                await self._process_with_external_indicators(symbol, price, indicators['short_term'], now)
                return
            
            # indicators가 없으면 직접 계산
            await self._process_with_calculated_indicators(symbol, price, candle_data, rank, now)
        
        except Exception as e:
            logger.error(f"실시간 가격 처리 중 오류: {str(e)}", exc_info=True)
    
    async def _get_or_update_candle_data(self, symbol: str) -> List[Dict[str, Any]]:
        """분봉 데이터 가져오기 또는 업데이트"""
        # 1. 캐시에서 분봉 데이터 가져오기
        if symbol in self.minute_candle_cache:
            last_update = self.last_minute_candle_update.get(symbol, datetime.min)
            if (datetime.now() - last_update).total_seconds() < 300:  # 5분 이내 갱신된 데이터면 사용
                return self.minute_candle_cache[symbol]
        
        # 2. 캐시에 없으면 StockCache에서 가져오기
        candle_data = self._get_minute_candles(symbol)
        if candle_data and len(candle_data) >= 10:
            # 최대 50개로 제한하고 캐시에 저장
            if len(candle_data) > 50:
                candle_data = candle_data[:50]
            
            # 캐시에 저장
            self.minute_candle_cache[symbol] = candle_data
            self.last_minute_candle_update[symbol] = datetime.now()
            
        return candle_data
    
    async def _process_with_external_indicators(self, symbol: str, price: float, 
                                               short_term_indicators: Dict[str, Any], 
                                               now: datetime) -> None:
        """외부에서 제공된 지표로 처리"""
        signal = short_term_indicators.get("signal", "중립")
        
        if signal == "매수":
            # 매수 신호 처리
            self._handle_buy_signal(symbol, price, "단타 매매 지표 매수 신호", now)
        
        elif signal == "매도" and symbol in self.positions:
            # 매도 신호 처리
            self._handle_sell_signal(symbol, price, "단타 매매 지표 매도 신호", now)
    
    async def _process_with_calculated_indicators(self, symbol: str, price: float, 
                                                 candle_data: List[Dict[str, Any]], 
                                                 rank: int, now: datetime) -> None:
        """직접 계산한 지표로 처리"""
        # 1. 거래량 급증 확인
        volume_surge, surge_candle = self._check_volume_surge(candle_data)
        
        # 2. 눌림목 패턴 확인
        pullback_pattern = False
        if volume_surge and surge_candle is not None:
            pullback_pattern = self._check_pullback_pattern(candle_data, surge_candle)
        
        # 3. 일목균형표 기준선 근처 확인
        ichimoku_signal = self._check_ichimoku_baseline(candle_data, price)
        
        # 4. 거래대금 순위 확인 - 상위 30위 이내인지 확인
        high_trading_amount = rank > 0 and rank <= 50
        
        # 매수 신호 생성 조건
        if volume_surge and pullback_pattern and ichimoku_signal and high_trading_amount:
            reason = f"거래량 급증 후 눌림목 형성 (거래대금 순위: {rank}위)"
            self._handle_buy_signal(symbol, price, reason, now)
        
        # 매도 신호 생성 (보유 종목인 경우에만)
        elif symbol in self.positions:
            await self._check_and_handle_sell_conditions(symbol, price, candle_data, now)
    
    async def _check_and_handle_sell_conditions(self, symbol: str, price: float, 
                                              candle_data: List[Dict[str, Any]], 
                                              now: datetime) -> None:
        """매도 조건 확인 및 처리"""
        position = self.positions.get(symbol, {})
        avg_price = position.get("avg_price", 0)
        
        # 손절 조건 확인
        stop_loss_triggered = (avg_price > 0) and (price < avg_price * (1 - self.stop_loss_pct / 100))
        
        # 이익 실현 조건 확인
        take_profit, profit_stage = self._check_take_profit(symbol, candle_data, price, avg_price)
        
        if stop_loss_triggered:
            # 손절 신호 처리
            self._handle_sell_signal(symbol, price, "손절 조건 충족", now)
        
        elif take_profit:
            # 이익 실현 신호 처리 - 단계 정보 포함
            reason = f"{profit_stage}차 이익 실현 (고점 대비 {profit_stage}% 하락)"
            
            # 분할 매도 상태 정보에 매도 단계 저장
            if symbol in self.division_status:
                self.division_status[symbol]['profit_stage'] = profit_stage
            
            self._handle_sell_signal(symbol, price, reason, now)
        
    def _handle_buy_signal(self, symbol: str, price: float, reason: str, now: datetime) -> None:
        """매수 신호 처리 - 분할 매수 적용"""
        # 이미 보유 중인지 확인
        if symbol in self.positions:
            return self._handle_additional_buy(symbol, price, reason, now)
        
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

    async def _check_additional_buy_opportunity(self) -> None:
        """보유 종목 중 추가 매수 기회 확인"""
        try:
            now = datetime.now()
            
            # 현재 보유 종목에 대해 확인
            for symbol, position in self.positions.items():
                avg_price = position.get("avg_price", 0)
                if avg_price <= 0:
                    continue
                
                # 현재가 확인
                current_price = self._get_current_price(symbol)
                if current_price <= 0:
                    continue
                
                # 매수 분할 상태 확인
                division_info = self.division_status.get(symbol, {})
                buy_count = division_info.get("buy_count", 0)
                last_division = division_info.get("last_division", datetime.min)
                
                # 매수 횟수가 최대 분할 횟수보다 적고, 마지막 매수 후 일정 시간이 지났는지 확인
                if buy_count < self.buy_division_count and buy_count > 0:
                    seconds_since_last = (now - last_division).total_seconds()
                    if seconds_since_last < self.division_interval:
                        continue
                    
                    # 2% 하락 확인 (추가 매수 조건)
                    price_drop_pct = ((avg_price - current_price) / avg_price) * 100
                    
                    if price_drop_pct >= self.additional_buy_drop_pct:
                        reason = f"첫 매수 대비 {price_drop_pct:.1f}% 하락으로 추가 매수 기회"
                        self._handle_additional_buy(symbol, current_price, reason, now)
                        logger.info(f"하락 기반 추가 매수 신호 발생: {symbol}, 현재가: {current_price:.2f}, "
                                f"평균단가: {avg_price:.2f}, 하락률: {price_drop_pct:.1f}%")
        except Exception as e:
            logger.error(f"추가 매수 기회 확인 중 오류: {str(e)}", exc_info=True)
    
    def _handle_additional_buy(self, symbol: str, price: float, reason: str, now: datetime) -> None:
        """추가 분할 매수 처리"""
        # 분할 매수 진행 중인지 확인
        if symbol not in self.division_status:
            # 아직 분할 매수 정보가 없으면 초기화
            logger.debug(f"이미 보유 중인 종목: {symbol}, 분할 매수 상태 초기화")
            self.division_status[symbol] = {
                "buy_count": 1,  # 이미 1회 매수 완료로 간주
                "sell_count": 0,
                "last_division": now
            }
            return
            
        division_info = self.division_status[symbol]
        buy_count = division_info.get("buy_count", 0)
        last_division = division_info.get("last_division", datetime.min)
        
        # 최대 분할 횟수에 도달했는지 확인
        if buy_count >= self.buy_division_count:
            logger.debug(f"종목 {symbol}의 분할 매수 완료 (총 {buy_count}회)")
            return
        
        # 분할 매수 간격 확인 - 2% 하락 시나리오에서는 이 조건을 우회할 수 있음
        seconds_since_last = (now - last_division).total_seconds()
        
        # 2% 하락 기반 추가 매수이거나 시간 간격 충족 시
        if "하락으로 추가 매수" in reason or seconds_since_last >= self.division_interval:
            # 추가 분할 매수 신호 생성
            self.trading_signals[symbol] = {
                "signal": "additional_buy",
                "price": price,
                "timestamp": now,
                "reason": f"분할 매수 ({buy_count + 1}/{self.buy_division_count}): {reason}",
                "division": buy_count + 1
            }
            
            # 분할 매수 상태 업데이트
            self.division_status[symbol]["buy_count"] = buy_count + 1
            self.division_status[symbol]["last_division"] = now
            
            logger.info(f"분할 매수 신호 발생: {symbol}, 현재가: {price:.2f}, 회차: {buy_count + 1}/{self.buy_division_count}")
        else:
            logger.debug(f"종목 {symbol}의 다음 분할 매수까지 {self.division_interval - seconds_since_last:.0f}초 남음")
            return
    
    def _handle_sell_signal(self, symbol: str, price: float, reason: str, now: datetime) -> None:
        """매도 신호 처리 - 분할 매도 적용"""
        # 매도 가능한지 확인 (보유 중인 종목인지)
        if symbol not in self.positions:
            logger.debug(f"보유하지 않은 종목 매도 신호 무시: {symbol}")
            return
        
        # 분할 매도 진행 중인지 확인
        if symbol in self.division_status:
            self._handle_partial_sell(symbol, price, reason, now)
        else:
            self._handle_first_sell(symbol, price, reason, now)
    
    def _handle_partial_sell(self, symbol: str, price: float, reason: str, now: datetime) -> None:
        """분할 매도 처리"""
        division_info = self.division_status[symbol]
        sell_count = division_info.get("sell_count", 0)
        last_division = division_info.get("last_division", datetime.min)
        
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
            quantity = self._calculate_sell_quantity(symbol, sell_count)
        
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
    
    def _handle_first_sell(self, symbol: str, price: float, reason: str, now: datetime) -> None:
        """첫 매도 처리"""
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
    
    def _calculate_sell_quantity(self, symbol: str, sell_count: int) -> Union[str, int]:
        """매도 수량 계산"""
        position = self.positions.get(symbol, {})
        total_quantity = position.get("quantity", 0)
        
        # 마지막 분할이면 잔량 전부
        if sell_count == self.sell_division_count - 1:
            return "all"  # 마지막 매도는 전량
        
        # 분할 매도 수량: 총 보유 수량을 분할 횟수로 나눔 (반올림)
        quantity = round(total_quantity / self.sell_division_count)
        
        # 최소 1주 이상
        return max(1, quantity)

    async def monitor_signals(self) -> None:
        """매매 신호 주기적 모니터링 및 처리"""
        logger.info("단타 매매 신호 모니터링 시작")
        
        while self.is_running:
            try:
                # 30초마다 매매 신호 확인
                await asyncio.sleep(30)
                
                # 계좌 정보 동기화
                await self._sync_account_info()
                
                # 매매 신호 로깅 및 정리
                self._log_and_clean_signals()
            
            except Exception as e:
                logger.error(f"매매 신호 모니터링 중 오류: {str(e)}", exc_info=True)
                await asyncio.sleep(30)
    
    def _log_and_clean_signals(self) -> None:
        """매매 신호 로깅 및 오래된 신호 제거"""
        if not self.trading_signals:
            return
            
        # 매매 신호 로깅
        buy_signals = sum(1 for v in self.trading_signals.values() if v["signal"] in ["buy", "additional_buy"])
        sell_signals = sum(1 for v in self.trading_signals.values() if v["signal"] in ["sell", "partial_sell", "stop_loss"])
        logger.info(f"현재 매매 신호: 매수={buy_signals}개, 매도={sell_signals}개")
        
        # 오래된 신호 제거 (10분 이상 경과)
        now = datetime.now()
        for symbol, signal_info in list(self.trading_signals.items()):
            timestamp = signal_info["timestamp"]
            if (now - timestamp).total_seconds() > 600:
                del self.trading_signals[symbol]
                logger.debug(f"종목 {symbol}의 오래된 신호 제거 (10분 경과)")

    async def get_trade_decisions(self, prices: Dict[str, float] = None) -> List[Dict[str, Any]]:
        """매매 의사결정 목록 반환 - 분할 매매 전략 적용"""
        if not self.is_running:
            logger.warning("트레이딩 모델이 실행 중이지 않습니다.")
            return []
        
        decisions = []
        try:
            # 계좌 정보 최신화
            await self._sync_account_info()
            
            # 계좌 정보 확인
            if not self.account_info:
                logger.warning("계좌 정보가 없습니다.")
                return []
            
            # 현재 시간
            now = datetime.now()
            
            # 매매 신호에 따른 거래 결정 처리
            for symbol, signal_info in list(self.trading_signals.items()):
                # 신호 유효 시간 (10분) 체크
                if self._is_signal_expired(signal_info, now):
                    del self.trading_signals[symbol]
                    continue
                
                signal = signal_info["signal"]
                
                # 매수 신호 처리
                if signal in ["buy", "additional_buy"]:
                    decision = await self._process_buy_decision(symbol, signal_info, prices, now)
                    if decision:
                        decisions.append(decision)
                
                # 매도 신호 처리
                elif signal in ["sell", "partial_sell", "stop_loss"]:
                    decision = await self._process_sell_decision(symbol, signal_info, prices, now)
                    if decision:
                        decisions.append(decision)
            
            return decisions
        
        except Exception as e:
            logger.error(f"매매 의사결정 생성 중 오류: {str(e)}", exc_info=True)
            return []
    
    def _is_signal_expired(self, signal_info: Dict[str, Any], now: datetime) -> bool:
        """신호 만료 여부 확인"""
        timestamp = signal_info.get("timestamp", now)
        return (now - timestamp).total_seconds() > 600  # 10분 경과 시 만료
    
    async def _process_buy_decision(self, symbol: str, signal_info: Dict[str, Any], 
                                  prices: Dict[str, float], now: datetime) -> Optional[Dict[str, Any]]:
        """매수 결정 처리"""
        signal = signal_info["signal"]
        price = signal_info["price"]
        reason = signal_info.get("reason", "단타 매매 신호")
        division = signal_info.get("division", 1)
        
        # 첫 매수인 경우 추가 검증
        if signal == "buy":
            # 현재 보유 종목 수 확인
            if len(self.positions) >= self.max_positions:
                logger.debug(f"최대 보유 종목 수({self.max_positions}) 도달, 매수 보류: {symbol}")
                return None
            
            # 이미 보유 중인지 확인 (이중 체크)
            if symbol in self.positions:
                logger.debug(f"이미 보유 중인 종목 매수 신호 무시: {symbol}")
                del self.trading_signals[symbol]
                return None
        
        # 충분한 현금 확인
        division_amount = self.trade_amount_per_stock / self.buy_division_count
        if self.cash_balance < division_amount:
            logger.debug(f"현금 부족({self.cash_balance}), 매수 보류: {symbol}")
            return None
        
        # 현재가 확인
        current_price = self._get_updated_price(symbol, price, prices)
        if current_price <= 0:
            logger.warning(f"종목 {symbol}의 가격 정보 없음, 매수 보류")
            return None
        
        # 매수 수량 계산
        quantity = int(division_amount / current_price)
        if quantity <= 0:
            logger.warning(f"종목 {symbol}의 매수 수량이 0, 매수 보류")
            return None
        
        # 매수 결정 생성
        decision = {
            "symbol": symbol,
            "action": "buy",
            "quantity": quantity,
            "price": current_price,
            "reason": reason,
            "division": division,
            "timestamp": now.isoformat()
        }
        
        # 거래 이력 업데이트
        self.trade_history[symbol] = {
            "last_trade": now,
            "last_action": signal
        }
        
        # 신호 제거
        del self.trading_signals[symbol]
        
        # 로깅
        action_str = "추가 매수" if signal == "additional_buy" else "매수"
        logger.info(f"{action_str} 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}, 이유: {reason}, 회차: {division}/{self.buy_division_count}")
        
        return decision
    
    async def _process_sell_decision(self, symbol: str, signal_info: Dict[str, Any], 
                                   prices: Dict[str, float], now: datetime) -> Optional[Dict[str, Any]]:
        """매도 결정 처리"""
        signal = signal_info["signal"]
        price = signal_info["price"]
        reason = signal_info.get("reason", "단타 매매 신호")
        division = signal_info.get("division", 1)
        
        # 보유 중인 종목인지 확인
        if symbol not in self.positions:
            logger.debug(f"미보유 종목 매도 신호 무시: {symbol}")
            del self.trading_signals[symbol]
            return None
        
        position = self.positions[symbol]
        total_quantity = position.get("quantity", 0)
        
        if total_quantity <= 0:
            logger.warning(f"종목 {symbol}의 보유 수량이 0, 매도 보류")
            return None
        
        # 매도 수량 처리
        sell_quantity = self._process_sell_quantity(signal_info, total_quantity)
        if sell_quantity <= 0:
            logger.warning(f"종목 {symbol}의 매도 수량이 0, 매도 보류")
            return None
        
        # 현재가 확인
        current_price = self._get_updated_price(symbol, price, prices)
        if current_price <= 0:
            logger.warning(f"종목 {symbol}의 가격 정보 없음, 매도 보류")
            return None
        
        # 매도 결정 생성
        decision = {
            "symbol": symbol,
            "action": "sell",
            "quantity": sell_quantity,
            "price": current_price,
            "reason": reason,
            "division": division,
            "timestamp": now.isoformat()
        }
        
        # 거래 이력 업데이트
        last_action = "sell_all" if sell_quantity == total_quantity else "partial_sell"
        self.trade_history[symbol] = {
            "last_trade": now,
            "last_action": last_action
        }
        
        # 신호 제거
        del self.trading_signals[symbol]
        
        # 로깅
        if signal == "stop_loss":
            logger.info(f"손절 매도 결정: {symbol}, {sell_quantity}주, 가격: {current_price:.2f}, 이유: {reason}")
        else:
            logger.info(f"분할 매도 결정: {symbol}, {sell_quantity}주, 가격: {current_price:.2f}, 이유: {reason}, 회차: {division}/{self.sell_division_count}")
        
        return decision
    
    def _process_sell_quantity(self, signal_info: Dict[str, Any], total_quantity: int) -> int:
        """매도 수량 처리"""
        sell_quantity = signal_info.get("quantity", "half")
        
        if sell_quantity == "all":
            # 전량 매도
            return total_quantity
        elif sell_quantity == "half":
            # 절반 매도 (첫 분할)
            return max(1, round(total_quantity / 2))
        elif isinstance(sell_quantity, int) or str(sell_quantity).isdigit():
            # 숫자로 지정된 수량
            qty = int(sell_quantity)
            return min(qty, total_quantity)  # 보유량보다 많으면 전량으로 조정
        
        # 기본적으로 1주 반환
        return 1
    
    def _get_updated_price(self, symbol: str, price: float, prices: Dict[str, float] = None) -> float:
        """최신 가격 조회"""
        current_price = price
        
        if current_price <= 0:
            # 1. 외부에서 전달받은 가격 사용
            if prices and symbol in prices:
                current_price = prices[symbol]
            # 2. StockCache에서 가격 조회
            elif self.stock_cache:
                current_price = self.stock_cache.get_price(symbol)
        
        return current_price

    def update_account_info(self, account_info: Dict[str, Any]) -> bool:
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
        """중복 메시지 필터링"""
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

    def set_backend_client(self, backend_client) -> None:
        """백엔드 클라이언트 설정"""
        self.backend_client = backend_client
        logger.info("단타 매매 모델 백엔드 클라이언트 설정 완료")

    def _check_volume_surge(self, candle_data: List[Dict[str, Any]]) -> Tuple[bool, Optional[Dict[str, Any]]]:
        """거래량 급증 확인"""
        try:
            if not candle_data or len(candle_data) < 15:
                return False, None
            
            # 캔들 데이터 형식 확인 및 변환
            recent_candles = candle_data[:5]    # 최근 5개 캔들
            previous_candles = candle_data[5:15]  # 이전 10개 캔들 (평균 계산용)
            
            # 거래량 데이터 추출
            recent_volumes = self._extract_candle_data(recent_candles, 'volume', 5)
            previous_volumes = self._extract_candle_data(previous_candles, 'volume', 5)
            
            if not previous_volumes:
                return False, None
            
            # 이전 10개 캔들의 평균 거래량 계산
            avg_volume = sum(previous_volumes) / len(previous_volumes)
            
            # 최근 5개 캔들 중 급증 여부 확인
            for i, volume in enumerate(recent_volumes):
                # 최소 5배 이상 급증
                if volume > avg_volume * 5:
                    # 가격 상승 확인
                    candle = recent_candles[i]
                    open_price = self._extract_single_candle_data(candle, 'open', 1)
                    close_price = self._extract_single_candle_data(candle, 'close', 4)
                    
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
    
    def _extract_candle_data(self, candles: List[Any], field: str, index: int) -> List[float]:
        """캔들 데이터에서 특정 필드 추출"""
        result = []
        for candle in candles:
            value = self._extract_single_candle_data(candle, field, index)
            if value > 0:
                result.append(value)
        return result
    
    def _extract_single_candle_data(self, candle: Any, field: str, index: int) -> float:
        """단일 캔들에서 데이터 추출"""
        try:
            if isinstance(candle, dict) and field in candle:
                return float(candle[field])
            elif isinstance(candle, list) and len(candle) > index:
                return float(candle[index])
            return 0
        except (ValueError, TypeError):
            return 0

    def _check_pullback_pattern(self, candle_data: List[Dict[str, Any]], surge_candle: Dict[str, Any]) -> bool:
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
            
            if surge_index < 0 or surge_index >= len(candle_data) - 2:
                return False
            
            # 급등 캔들의 정보
            surge_high = self._extract_single_candle_data(surge_candle, 'high', 2)
            surge_close = self._extract_single_candle_data(surge_candle, 'close', 4)
            
            if surge_high <= 0 or surge_close <= 0:
                return False
            
            # 이후 캔들들의 정보
            after_candles = candle_data[surge_index + 1:surge_index + 3]  # 2개 캔들 확인
            
            lowest_after = float('inf')
            for candle in after_candles:
                low_price = self._extract_single_candle_data(candle, 'low', 3)
                if low_price > 0:
                    lowest_after = min(lowest_after, low_price)
            
            if lowest_after == float('inf'):
                return False
            
            # 눌림목 패턴: 이후 캔들에서 최저가가 급등 캔들의 고가보다 낮고, 
            # 급등 캔들 종가의 0.5%~3% 이내로 내려왔는지 확인
            if lowest_after < surge_high:
                price_decrease_pct = ((surge_close - lowest_after) / surge_close) * 100
                return 0.3 <= price_decrease_pct <= 5.0
            
            return False
        except Exception as e:
            logger.error(f"눌림목 패턴 확인 중 오류: {str(e)}")
            return False

    def _check_ichimoku_baseline(self, candle_data: List[Dict[str, Any]], current_price: float) -> bool:
        """일목균형표 기준선 확인"""
        try:
            if not candle_data or len(candle_data) < 26 or current_price <= 0:
                return False
            
            # 26일 기준선 계산 데이터 추출
            period_candles = candle_data[:26]
            
            # 고가와 저가 추출
            highs = self._extract_candle_data(period_candles, 'high', 2)
            lows = self._extract_candle_data(period_candles, 'low', 3)
            
            if not highs or not lows:
                return False
            
            # 기준선 계산 (26일 고가와 저가의 평균)
            highest = max(highs)
            lowest = min(lows)
            
            kijun_sen = (highest + lowest) / 2
            
            # 현재가가 기준선 근처인지 확인 (±0.5%)
            price_diff_pct = abs((current_price - kijun_sen) / kijun_sen) * 100
            
            return price_diff_pct <= 1.0
        except Exception as e:
            logger.error(f"일목균형표 기준선 확인 중 오류: {str(e)}")
            return False

    def _check_take_profit(self, symbol: str, candle_data: List[Dict[str, Any]], current_price: float, avg_price: float) -> Tuple[bool, int]:
        """이익 실현 조건 확인
        Returns:
            Tuple[bool, int]: (이익실현여부, 매도단계 1 또는 2)
        """
        try:
            if not candle_data or len(candle_data) < 5 or avg_price <= 0 or current_price <= 0:
                return False, 0
            
            # 수익률 계산
            profit_pct = ((current_price / avg_price) - 1) * 100
            
            # 최소 5% 이상 수익 발생 확인
            if profit_pct < 5.0:
                return False, 0
            
            # 최근 30개 캔들 사용
            lookback_candles = min(30, len(candle_data))
            recent_candles = candle_data[:lookback_candles]
            highest_prices = self._extract_candle_data(recent_candles, 'high', 2)
            
            if not highest_prices:
                return False, 0
                
            highest = max(highest_prices)
            
            # 고점 정보를 영구 저장 (분할 매도 상태 정보에 추가)
            if symbol and symbol in self.division_status:
                # 이미 저장된 고점이 없거나 새로운 고점이 더 높으면 업데이트
                if 'highest_price' not in self.division_status[symbol] or highest > self.division_status[symbol]['highest_price']:
                    self.division_status[symbol]['highest_price'] = highest
                    logger.info(f"종목 {symbol}의 신규 고점 기록: {highest:.2f}")
            else:
                # 분할 매도 상태 정보가 없으면 초기화
                if symbol:
                    self.division_status[symbol] = {
                        'buy_count': 1,  # 이미 매수한 상태로 가정
                        'sell_count': 0,
                        'last_division': datetime.now(),
                        'highest_price': highest
                    }
            
            # 저장된 고점 사용
            saved_highest = self.division_status.get(symbol, {}).get('highest_price', highest)
            
            # 고점에서 하락 비율 계산
            if saved_highest > current_price:
                drop_pct = ((saved_highest - current_price) / saved_highest) * 100
                
                # 1차 매도 (1% 하락)
                if drop_pct >= 2.0 and drop_pct < 3.0:
                    return True, 2
                # 2차 매도 (2% 이상 하락)
                elif drop_pct >= 3.0:
                    return True, 3
            
            return False, 0
        except Exception as e:
            logger.error(f"이익 실현 조건 확인 중 오류: {str(e)}")
            return False, 0