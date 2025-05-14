import asyncio
import logging
import time
from datetime import datetime
from typing import Dict, Any, Optional

from prometheus_client import start_http_server
from app.monitoring.metrics import *

logger = logging.getLogger(__name__)

class MetricsExporter:
    """프로메테우스 메트릭 익스포터 클래스"""
    
    def __init__(self, port: int = 9090):
        """
        메트릭 익스포터 초기화
        
        :param port: 프로메테우스 익스포터 HTTP 서버 포트
        """
        self.port = port
        self.is_running = False
        self.update_task = None
        self.bot_manager = None
        self.kiwoom_api = None
        self.service_status = None
        
        # 최근 데이터 업데이트 시간
        self.last_data_update = time.time()
        
        logger.info(f"메트릭 익스포터 초기화 완료 (포트: {port})")
    
    def start(self):
        """메트릭 익스포터 시작"""
        # 이미 실행 중이면 중복 시작 방지
        if self.is_running:
            logger.warning("메트릭 익스포터가 이미 실행 중입니다")
            return
        
        # 프로메테우스 HTTP 서버 시작
        start_http_server(self.port)
        self.is_running = True
        
        # 시스템 상태 업데이트
        system_up.set(1)
        
        logger.info(f"프로메테우스 메트릭 익스포터 시작됨 (포트: {self.port})")
    
    def set_dependencies(self, bot_manager, kiwoom_api, service_status):
        """
        의존성 설정
        
        :param bot_manager: 봇 관리자 인스턴스
        :param kiwoom_api: 키움 API 인스턴스
        :param service_status: 서비스 상태 딕셔너리
        """
        self.bot_manager = bot_manager
        self.kiwoom_api = kiwoom_api
        self.service_status = service_status
        logger.info("메트릭 익스포터 의존성 설정 완료")
    
    async def start_metrics_update(self, update_interval: int = 30):
        """
        메트릭 주기적 업데이트 시작
        
        :param update_interval: 업데이트 간격(초)
        """
        # 업데이트 작업이 이미 실행 중이면 중복 시작 방지
        if self.update_task and not self.update_task.done():
            logger.warning("메트릭 업데이트 작업이 이미 실행 중입니다")
            return
        
        # 메트릭 업데이트 작업 시작
        self.update_task = asyncio.create_task(self._metrics_update_loop(update_interval))
        logger.info(f"메트릭 업데이트 작업 시작됨 (간격: {update_interval}초)")
    
    async def _metrics_update_loop(self, update_interval: int):
        """
        메트릭 업데이트 루프
        
        :param update_interval: 업데이트 간격(초)
        """
        while self.is_running:
            try:
                # 모든 메트릭 업데이트
                await self._update_all_metrics()
                
                # 다음 업데이트까지 대기
                await asyncio.sleep(update_interval)
                
            except asyncio.CancelledError:
                logger.info("메트릭 업데이트 작업이 취소되었습니다")
                break
            except Exception as e:
                logger.error(f"메트릭 업데이트 중 오류 발생: {str(e)}", exc_info=True)
                error_count.labels(severity="error", module="monitoring").inc()
                await asyncio.sleep(60)  # 오류 시 1분 후 재시도
    
    async def _update_all_metrics(self):
        """모든 메트릭 업데이트"""
        # 시스템 메트릭 업데이트
        self._update_system_metrics()
        
        # 의존성이 설정되지 않았으면 나머지 메트릭 업데이트 건너뛰기
        if not self.bot_manager or not self.kiwoom_api or not self.service_status:
            logger.warning("메트릭 업데이트를 위한 의존성이 설정되지 않았습니다")
            return
        
        # 봇 관련 메트릭 업데이트
        await self._update_bot_metrics()
        
        # 계좌 관련 메트릭 업데이트
        await self._update_account_metrics()
        
        # 데이터 관련 메트릭 업데이트
        self._update_data_metrics()
        
        # 거래 관련 메트릭 업데이트
        await self._update_trade_metrics()
        
        logger.debug("모든 메트릭 업데이트 완료")
    
    def _update_system_metrics(self):
        """시스템 메트릭 업데이트"""
        # 시스템 가동 시간 업데이트
        uptime_seconds = time.time() - system_start_time
        system_uptime.set(uptime_seconds)
        
        # 서비스 상태가 설정되었으면 추가 업데이트
        if self.service_status:
            # 서비스 실행 중 여부
            system_up.set(1 if self.service_status.get("is_running", False) else 0)
    
    async def _update_bot_metrics(self):
        """봇 관련 메트릭 업데이트"""
        if not self.bot_manager:
            return
        
        # 활성 봇 수 업데이트
        running_bots = self.bot_manager.get_running_bots()
        active_bots.set(len(running_bots))
        
        # 모든 봇 상태 업데이트
        all_bots = self.bot_manager.get_all_bots()
        for email, bot in all_bots.items():
            # 봇 상태 (1: 실행 중, 0: 중지)
            bot_status.labels(email=email, strategy=bot.strategy.name).set(1 if bot.is_running else 0)
            
            # 봇 가동 시간
            if bot.start_time:
                uptime = (datetime.now() - bot.start_time).total_seconds()
                bot_uptime.labels(email=email).set(uptime)
            else:
                bot_uptime.labels(email=email).set(0)
    
    async def _update_account_metrics(self):
        """계좌 관련 메트릭 업데이트"""
        if not self.bot_manager:
            return
        
        # 모든 봇의 계좌 정보 업데이트
        all_bots = self.bot_manager.get_all_bots()
        for email, bot in all_bots.items():
            # 계좌 정보 가져오기
            cash = bot.get_cash()
            holdings = bot.get_holdings()
            
            # 예수금 잔액
            account_balance.labels(email=email).set(cash)
            
            # 보유 종목 수
            holding_count.labels(email=email).set(len(holdings))
            
            # 총 자산 가치
            asset_value = cash
            profit_loss_sum = 0
            
            # 보유 종목 순회
            for holding in holdings:
                quantity = holding.get("quantity", 0)
                current_price = holding.get("current_price", 0)
                avg_price = holding.get("avg_price", 0)
                
                if quantity > 0 and current_price > 0:
                    # 보유 주식 가치 합산
                    asset_value += quantity * current_price
                    
                    # 손익 계산
                    if avg_price > 0:
                        profit_loss_sum += (current_price - avg_price) * quantity
            
            # 총 자산 가치 업데이트
            total_asset_value.labels(email=email).set(asset_value)
            
            # 손익 업데이트
            profit_loss.labels(email=email).set(profit_loss_sum)
            
            # 손익률 계산 (초기 투자 금액 대비)
            initial_investment = cash + profit_loss_sum
            if initial_investment > 0:
                profit_rate = (profit_loss_sum / initial_investment) * 100
                profit_loss_rate.labels(email=email).set(profit_rate)
            else:
                profit_loss_rate.labels(email=email).set(0)
    
    def _update_data_metrics(self):
        """데이터 관련 메트릭 업데이트"""
        # 서비스 상태로부터 마지막 데이터 업데이트 시간 가져오기
        if self.service_status and "last_data_update" in self.service_status:
            last_update = self.service_status["last_data_update"]
            if last_update:
                # datetime 객체를 타임스탬프로 변환
                if isinstance(last_update, datetime):
                    timestamp = last_update.timestamp()
                    self.last_data_update = timestamp
                    
                    # 마지막 업데이트 시간 기록
                    data_update_time.set(timestamp)
        
        # 데이터 신선도 업데이트 (현재 시간 - 마지막 업데이트 시간)
        freshness = time.time() - self.last_data_update
        data_freshness.set(freshness)
        
        # 키움 API가 설정되어 있으면 차트 데이터 완전성 업데이트
        if self.kiwoom_api and hasattr(self.kiwoom_api, 'stock_cache'):
            # 필터링된 종목 수
            kospi_count = len(self.kiwoom_api.stock_cache.kospi_symbols)
            kosdaq_count = len(self.kiwoom_api.stock_cache.kosdaq_symbols)
            
            if kospi_count > 0:
                filtered_stocks_count.labels(market="KOSPI").set(kospi_count)
            
            if kosdaq_count > 0:
                filtered_stocks_count.labels(market="KOSDAQ").set(kosdaq_count)
            
            # 차트 데이터 완전성 계산
            kospi_with_data = 0
            kosdaq_with_data = 0
            
            for symbol in self.kiwoom_api.stock_cache.filtered_stockcode_list:
                chart_data = self.kiwoom_api.stock_cache.get_chart_data(symbol)
                if chart_data and len(chart_data) >= 20:  # 최소 20일 데이터가 있으면 완전한 것으로 간주
                    if symbol in self.kiwoom_api.stock_cache.kospi_symbols:
                        kospi_with_data += 1
                    elif symbol in self.kiwoom_api.stock_cache.kosdaq_symbols:
                        kosdaq_with_data += 1
            
            # 완전성 비율 계산 (%)
            if kospi_count > 0:
                kospi_completeness = (kospi_with_data / kospi_count) * 100
                chart_data_completeness.labels(market="KOSPI").set(kospi_completeness)
            
            if kosdaq_count > 0:
                kosdaq_completeness = (kosdaq_with_data / kosdaq_count) * 100
                chart_data_completeness.labels(market="KOSDAQ").set(kosdaq_completeness)
    
    async def _update_trade_metrics(self):
        """거래 관련 메트릭 업데이트"""
        if not self.bot_manager:
            return
        
        # 모든 봇의 거래 관련 메트릭 업데이트
        running_bots = self.bot_manager.get_running_bots()
        
        # 거래 신호 및 거래 이력 수집
        for email, bot in running_bots.items():
            if not bot.trading_model:
                continue
            
            strategy_name = bot.strategy.name
            
            # 거래 신호 수 수집
            if hasattr(bot.trading_model, 'trading_signals'):
                signals = bot.trading_model.trading_signals
                
                # 신호 유형별 카운트
                buy_signals = sum(1 for s in signals.values() if s.get('signal') in ['buy', 'additional_buy'])
                sell_signals = sum(1 for s in signals.values() if s.get('signal') in ['sell', 'partial_sell', 'stop_loss'])
                
                # 메트릭 게이지에 값 설정 (기존 값 유지가 필요해 Counter 대신 Gauge 사용)
                # 참고: 실제로는 신호가 처리될 때 Counter.inc()를 호출해야 함
                if buy_signals > 0:
                    logger.debug(f"Bot {email}의 매수 신호 수: {buy_signals}")
                
                if sell_signals > 0:
                    logger.debug(f"Bot {email}의 매도 신호 수: {sell_signals}")
            
            # 전략 성능 업데이트 (간단한 예시)
            if hasattr(bot, 'account_info'):
                account_info = bot.get_account_info()
                
                # 당일 손익률 (있는 경우)
                daily_profit = account_info.get('daily_profit_rate', 0)
                if daily_profit:
                    strategy_performance.labels(strategy=strategy_name, timeframe="daily").set(daily_profit)
                
                # 전체 손익률 (있는 경우)
                total_profit = account_info.get('total_profit_rate', 0)
                if total_profit:
                    strategy_performance.labels(strategy=strategy_name, timeframe="total").set(total_profit)
    
    def record_error(self, severity: str, module: str):
        """
        오류 기록
        
        :param severity: 오류 심각도 (error, warning, critical)
        :param module: 오류 발생 모듈
        """
        error_count.labels(severity=severity, module=module).inc()
    
    def record_api_request(self, api_name: str, endpoint: str, duration: float, error_type: Optional[str] = None):
        """
        API 요청 기록
        
        :param api_name: API 이름
        :param endpoint: 엔드포인트
        :param duration: 요청 소요 시간(초)
        :param error_type: 오류 유형 (오류 발생 시)
        """
        # API 요청 카운트 증가
        api_requests.labels(api_name=api_name, endpoint=endpoint).inc()
        
        # 요청 소요 시간 기록
        api_request_duration.labels(api_name=api_name).observe(duration)
        
        # 오류 발생 시 오류 카운트 증가
        if error_type:
            api_errors.labels(api_name=api_name, error_type=error_type).inc()
    
    def record_trade_signal(self, strategy: str, signal_type: str):
        """
        거래 신호 기록
        
        :param strategy: 전략 이름
        :param signal_type: 신호 유형 (buy, sell, stop_loss 등)
        """
        trade_signals.labels(strategy=strategy, signal_type=signal_type).inc()
    
    def record_trade_execution(self, strategy: str, action: str, quantity: int, amount: float):
        """
        거래 실행 기록
        
        :param strategy: 전략 이름
        :param action: 매매 유형 (buy, sell)
        :param quantity: 거래 수량
        :param amount: 거래 금액
        """
        # 거래 실행 카운트 증가
        trades_executed.labels(strategy=strategy, action=action).inc()
        
        # 거래 수량 증가
        trade_volume.labels(action=action).inc(quantity)
        
        # 거래 금액 증가
        trade_amount.labels(action=action).inc(amount)
    
    def update_market_indices(self, index_data: Dict[str, Dict[str, float]]):
        """
        시장 지수 업데이트
        
        :param index_data: 지수 데이터 (예: {'KOSPI': {'value': 2500.25, 'change': 1.25}})
        """
        for index_name, data in index_data.items():
            if 'value' in data:
                market_index_value.labels(index_name=index_name).set(data['value'])
            
            if 'change' in data:
                market_index_change.labels(index_name=index_name).set(data['change'])
    
    def stop(self):
        """메트릭 익스포터 중지"""
        if not self.is_running:
            return
        
        # 업데이트 작업 취소
        if self.update_task and not self.update_task.done():
            self.update_task.cancel()
        
        self.is_running = False
        system_up.set(0)
        
        logger.info("메트릭 익스포터 중지됨")