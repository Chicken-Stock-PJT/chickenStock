import asyncio
import logging
import os
from typing import Dict, List, Optional, Any
from datetime import datetime, timedelta, time

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

from app.models.trade_models import TradingStrategy
from app.auth.token_manager import TokenManager
from app.auth.kiwoom_auth import KiwoomAuthClient
from app.auth.auth_client import AuthClient

from app.api.backend_client import BackendClient
from app.api.kiwoom_api import KiwoomAPI
from app.bot.bot_manager import BotManager

from app.monitor.cache_monitor import add_monitor_to_app
from app.data.data_utils import save_all_chart_data, prepare_dataset_for_upload
from app.strategies.drl_utrans import DRLUTransTradingModel

# 설정 파일 import
from app.config import settings

# 로깅 설정
logging.basicConfig(
    level=getattr(logging, settings.LOG_LEVEL),
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("app")

# FastAPI 애플리케이션 초기화
app = FastAPI(
    title="주식 자동매매 봇 API",
    description="Envelope 및 볼린저밴드 전략 기반 한국 주식 자동매매 봇 API",
    version="1.0.0",
)

# CORS 설정
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:3000"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

class BotStatusResponse(BaseModel):
    """봇 상태 응답 모델"""
    email: str
    strategy: TradingStrategy
    is_running: bool
    start_time: Optional[str] = None
    last_data_update: Optional[str] = None
    account_info: Optional[Dict] = None

# 전역 변수
token_manager = None
auth_manager = None  # AuthClient 대신 AuthClient 사용
kiwoom_api = None
backend_client = None
bot_manager = None

# 서비스 상태
service_status = {
    "is_running": False,
    "start_time": None,
    "last_data_update": None,
    "active_strategy": None,  # 현재 활성화된 전략
    "current_user": None      # 현재 로그인된 사용자 이메일
}

# 태스크 관리용 변수들
trading_loop_task = None
scheduler_task_instance = None

os.environ['DRL_UTRANS_MODEL_PATH'] = os.path.join(os.path.dirname(__file__), "models")

async def get_next_run_time(target_hour=7, target_minute=0, target_second=0):
    """다음 실행 시간까지 대기해야 하는 시간(초) 계산"""
    now = datetime.now()
    target_time = datetime.now().replace(hour=target_hour, minute=target_minute, second=target_second, microsecond=0)
    
    # 이미 오늘의 목표 시간이 지났으면 내일로 설정
    if now >= target_time:
        tomorrow = now.date() + timedelta(days=1)
        target_time = datetime.combine(tomorrow, time(target_hour, target_minute, target_second))
    
    # 대기 시간 계산 (초 단위)
    wait_seconds = (target_time - now).total_seconds()
    return wait_seconds

async def initialize_service(strategy: TradingStrategy = TradingStrategy.ENVELOPE):
    """자동매매 서비스 초기화 및 시작"""
    global auth_manager, kiwoom_api, backend_client, service_status, token_manager, bot_manager
    global trading_loop_task, scheduler_task_instance
    
    try:
        logger.info(f"자동매매 서비스 초기화 시작 (전략: {strategy})")
        
        # 이미 실행 중이면 중복 초기화 방지
        if service_status["is_running"]:
            logger.info("서비스가 이미 실행 중입니다. 중복 초기화를 방지합니다.")
            return True
        
        # 변수 초기화
        default_email = None
        default_password = None

        # 토큰 관리자 초기화
        if not token_manager:
            token_manager = TokenManager()
            await token_manager.initialize()
        
        # 키움 API 토큰 발급을 위한 클라이언트
        kiwoom_auth_client = KiwoomAuthClient()
        kiwoom_auth_client.set_token_manager(token_manager)
        await kiwoom_auth_client.initialize()
        
        # 명시적으로 토큰 갱신
        kiwoom_token = await kiwoom_auth_client.refresh_token()
        if not kiwoom_token:
            logger.error("키움 API 토큰 발급 실패")
            return False

        # 백엔드 서버 로그인 확인
        if not auth_manager or not auth_manager.is_authenticated:
            logger.info("백엔드 서버 로그인 확인")
            
            # 설정에서 기본 계정 가져오기
            try:
                default_email = settings.DEFAULT_EMAIL
                default_password = settings.DEFAULT_PASSWORD
            except AttributeError:
                logger.warning("기본 계정 정보가 없습니다.")
                
            if not default_email or not default_password:
                logger.error("기본 계정 정보가 없습니다. settings.py에서 DEFAULT_EMAIL과 DEFAULT_PASSWORD를 설정하세요.")
                return False
            
            # 인증 매니저 초기화
            if not auth_manager:
                auth_manager = AuthClient()
                await auth_manager.initialize()
            
            # 백엔드 서버 로그인
            success = await auth_manager.login(default_email, default_password)
            if not success:
                logger.error("백엔드 서버 자동 로그인 실패")
                return False
            
            # 서비스 상태 업데이트
            service_status["current_user"] = default_email
            
            logger.info("백엔드 서버 자동 로그인 성공")

        # 키움 API 초기화
        if not kiwoom_api:
            kiwoom_api = KiwoomAPI(token_manager)
        
        # 백엔드 클라이언트 초기화
        if not backend_client:
            backend_client = BackendClient()
            backend_client.set_auth_manager(auth_manager)  # AuthClient 대신 AuthClient 사용
            await backend_client.start()
        
        # 키움 API 연결
        if not await kiwoom_api.connect():
            logger.error("키움 API 연결 실패")
            return False
            
        # 종목 정보 초기화 (백엔드에서 가져옴)
        stock_list = await backend_client.get_all_stocks()
        if not stock_list:
            logger.error("종목 정보를 가져오지 못했습니다.")
            return False
            
        # 필터링된 종목 리스트 가져오기 (시가총액 기준)
        filtered_symbols_by_market = await kiwoom_api.get_filtered_symbols(700, 200)
        logger.info(f"시가총액 기준 초기 필터링 완료")

        # 시장별로 실제 stock_list에 있는 종목만 추출
        available_symbols = {"KOSPI": [], "KOSDAQ": []}

        for market in ["KOSPI", "KOSDAQ"]:
            # 해당 시장의 필터링된 종목들
            market_symbols = filtered_symbols_by_market.get(market, [])
            
            # stock_list에서 실제 존재하는 종목만 필터링
            available_market_symbols = []

            for symbol_info in market_symbols:
                symbol = symbol_info['code']
                # stock_list에서 해당 종목 찾기
                matching_stocks = [
                    stock for stock in stock_list 
                    if stock.get('shortCode') == symbol and stock.get('market') == market
                ]
                
                if matching_stocks:  # 실제 시장에 존재하는 종목이면 추가
                    available_market_symbols.append(symbol)
            
            available_symbols[market] = available_market_symbols
            
            logger.info(f"{market} 시장: 시가총액 기준 {len(market_symbols)}개 → 실제 상장 {len(available_market_symbols)}개")

        # 각 시장별로 목표 수량 선택
        target_kospi_count = min(450, len(available_symbols["KOSPI"]))
        target_kosdaq_count = min(150, len(available_symbols["KOSDAQ"]))

        final_kospi_symbols = available_symbols["KOSPI"][:target_kospi_count]
        final_kosdaq_symbols = available_symbols["KOSDAQ"][:target_kosdaq_count]

        # 최종 종목 리스트 합치기
        final_symbols = final_kospi_symbols + final_kosdaq_symbols

        logger.info(f"최종 선택 종목: 코스피 {len(final_kospi_symbols)}개, 코스닥 {len(final_kosdaq_symbols)}개, 총 {len(final_symbols)}개")

        # 필터링된 종목 정보로 stock_cache 업데이트
        filtered_stock_list = [
            stock for stock in stock_list 
            if stock.get('shortCode') in final_symbols
        ]
        await kiwoom_api.initialize_stock_list(filtered_stock_list)

        filtered_stockcode_list = [
            stock.get("shortCode") for stock in filtered_stock_list if stock.get("shortCode")
        ]

        # StockCache에 필터링된 종목 리스트 설정
        kiwoom_api.stock_cache.set_filtered_stocks(filtered_stockcode_list)    

        filtered_symbols_by_market_kospi = filtered_symbols_by_market.get("KOSPI")
        filtered_symbols_by_market_kosdaq = filtered_symbols_by_market.get("KOSDAQ")

        current_price_kospi = {symbol.get("code"): symbol.get("lastPrice") for symbol in filtered_symbols_by_market_kospi
                                if symbol.get('code') in filtered_stockcode_list}
        current_price_kosdaq = {symbol.get("code"): symbol.get("lastPrice") for symbol in filtered_symbols_by_market_kosdaq
                                if symbol.get('code') in filtered_stockcode_list}

        for code in filtered_stockcode_list:
            if code in final_kospi_symbols:
                kiwoom_api.stock_cache.update_price(code, int(current_price_kospi.get(code)))
            elif code in final_kosdaq_symbols:
                kiwoom_api.stock_cache.update_price(code, int(current_price_kosdaq.get(code)))
        
        # 계좌 정보 초기화
        account_info = await backend_client.request_account_info()
        if account_info:
            kiwoom_api.update_account_info(account_info)
            
        # 차트 데이터 초기화 - 두 전략 중 더 긴 기간인 120일로 모든 종목 데이터 가져오기
        await kiwoom_api.initialize_chart_data(filtered_stockcode_list, period=120)
        logger.info(f"모든 종목({len(filtered_stockcode_list)}개)의 120일 차트 데이터 초기화 완료")

        await kiwoom_api.initialize_minute_chart_data(filtered_stockcode_list, time_interval=5)
        logger.info(f"모든 종목({len(filtered_stockcode_list)}개)의 5분봉 차트 데이터 초기화 완료")

        save_result = save_all_chart_data(kiwoom_api.stock_cache)

        # 모든 전략의 지표 계산 (두 전략 모두 미리 계산)
        # Envelope 지표 계산
        logger.info("Envelope 지표 계산 시작")
        kiwoom_api.stock_cache.calculate_envelope_indicators()
        logger.info("Envelope 지표 계산 완료")
        
        # 볼린저 밴드 지표 계산
        logger.info("볼린저 밴드 지표 계산 시작")
        kiwoom_api.stock_cache.calculate_bollinger_bands()
        logger.info("볼린저 밴드 지표 계산 완료")

        # 마지막 데이터 업데이트 시간 기록
        service_status["last_data_update"] = datetime.now()
        
        # 서비스 상태 업데이트
        service_status["is_running"] = True
        service_status["start_time"] = datetime.now()
        service_status["active_strategy"] = strategy
        
        # 봇 매니저 초기화 (아직 초기화되지 않은 경우에만)
        if not bot_manager:
            bot_manager = BotManager()
            bot_manager.shared_stock_cache = kiwoom_api.stock_cache
        
        # settings에서 추가 계정 정보 확인 및 봇 생성
        if hasattr(settings, 'ADDITIONAL_ACCOUNTS') and settings.ADDITIONAL_ACCOUNTS:
            logger.info(f"추가 계정으로 봇 생성 시작 (총 {len(settings.ADDITIONAL_ACCOUNTS)}개 계정)")
            
            for account_info in settings.ADDITIONAL_ACCOUNTS:
                email = account_info.email
                password = account_info.password
                strategy = account_info.strategy
                
                # 봇 생성 또는 업데이트
                try:
                    # 새 봇 생성 (공유 캐시 전달)
                    new_bot = await bot_manager.create_bot(email, password, strategy, kiwoom_api.stock_cache)
                    if new_bot:
                        # 봇 시작
                        await bot_manager.start_bot(email)
                        logger.info(f"추가 봇 생성 및 시작 성공: {email} (전략: {strategy})")
                    else:
                        logger.error(f"추가 봇 생성 실패: {email}")
                except Exception as e:
                    logger.error(f"추가 계정 봇 생성 중 오류: {email}, 오류: {str(e)}")
        
        # 기존 태스크가 있으면 취소
        if trading_loop_task and not trading_loop_task.done():
            trading_loop_task.cancel()
            
        if scheduler_task_instance and not scheduler_task_instance.done():
            scheduler_task_instance.cancel()

        # 실시간 데이터 구독 준비
        await kiwoom_api.prepare_subscription_groups(filtered_stock_list, 30)
        
        # 실시간 데이터 구독 로테이션 시작 (공통 콜백 사용)
        async def default_realtime_handler(symbol, price):
            # 실시간 데이터 받을 때 기본 처리 로직
            logger.debug(f"실시간 데이터 수신: 종목={symbol}, 가격={price}")
            
            # 공유 StockCache 업데이트 (현재가만 갱신)
            kiwoom_api.stock_cache.update_price(symbol, price)
            
            # 모든 실행 중인 봇의 트레이딩 모델에 알림
            running_bots = bot_manager.get_running_bots()
            for email, bot in running_bots.items():
                if bot.trading_model:
                    # 각 봇의 독립적인 트레이딩 모델로 실시간 데이터 전달
                    await bot_manager.handle_realtime_data(symbol, price)
            
            # 원래 형식과 호환되도록 딕셔너리 반환
            return {
                'symbol': symbol,
                'price': price
            }
            
        asyncio.create_task(
            kiwoom_api.start_rotating_subscriptions(default_realtime_handler)
        )
            
        # 거래 처리 루프 태스크 시작
        trading_loop_task = asyncio.create_task(trading_loop())
        
        # 스케줄러 (정기 데이터 갱신) 태스크 시작
        scheduler_task_instance = asyncio.create_task(scheduler_task())
        
        logger.info(f"자동매매 서비스 초기화 완료 (전략: {strategy})")
        return True
        
    except Exception as e:
        logger.error(f"서비스 초기화 중 오류 발생: {str(e)}", exc_info=True)
        service_status["is_running"] = False
        return False

async def trading_loop():
    """주기적인 매매 처리 루프 - 각 봇의 요청을 1초에 한 개씩 순차 처리"""
    global kiwoom_api, backend_client, bot_manager
    
    if not bot_manager:
        logger.error("거래 루프를 실행할 수 없습니다: 봇 매니저가 초기화되지 않았습니다.")
        return
    
    logger.info("거래 처리 루프 시작")
    
    # 마지막 처리 시간 초기화
    last_account_update_time = datetime.now()
    last_trading_decision_time = datetime.now()
    
    # 타이머 설정 (초 단위)
    ACCOUNT_UPDATE_INTERVAL = 10  # 계좌 정보 조회 간격 (10초)
    TRADING_DECISION_INTERVAL = 30  # 거래 처리 간격 (60초)
    
    # 마지막 주문 처리 시간 추적
    last_order_time = datetime.now() - timedelta(seconds=2)  # 초기에 바로 주문 가능하도록
    
    # 대기 중인 주문 큐 (모든 봇의 요청을 순차 처리하기 위한 큐)
    order_queue = []
    
    while service_status["is_running"]:
        try:
            # 현재 시간
            current_time = datetime.now()
            
            # 실행 중인 봇 조회
            running_bots = bot_manager.get_running_bots()
            
            # 10초마다 계좌 정보만 업데이트
            if (current_time - last_account_update_time).total_seconds() >= ACCOUNT_UPDATE_INTERVAL:
                logger.info(f"계좌 정보 업데이트 시작 (실행 중인 봇 수: {len(running_bots)})")
                
                # 병렬 처리를 위한 태스크 리스트
                account_update_tasks = []
                
                # 각 봇에 대한 계좌 정보 업데이트 함수 정의
                async def update_account_for_bot(email, bot):
                    try:
                        # 계좌 정보 업데이트 (인증 확인은 내부에서 처리됨)
                        await bot.update_account_info()
                        
                        # 계좌 정보 로그 출력
                        cash = bot.get_cash()
                        holdings = bot.get_holdings()
                        logger.info(f"봇 [{email}] 계좌 정보 업데이트 성공: 예수금={cash}, 보유종목수={len(holdings)}")
                    except Exception as e:
                        logger.error(f"봇 [{email}]의 계좌 정보 업데이트 중 오류: {str(e)}")
                
                # 각 봇에 대한 계좌 업데이트 태스크 생성
                for email, bot in running_bots.items():
                    account_update_tasks.append(update_account_for_bot(email, bot))
                
                # 모든 계좌 업데이트 태스크를 병렬로 실행 (타임아웃 10초로 설정)
                try:
                    await asyncio.wait_for(asyncio.gather(*account_update_tasks), timeout=10.0)
                    logger.info("모든 봇의 계좌 정보 업데이트 완료")
                except asyncio.TimeoutError:
                    logger.warning("일부 봇의 계좌 정보 업데이트가 시간 초과로 완료되지 않았습니다.")
                except Exception as e:
                    logger.error(f"계좌 정보 업데이트 중 오류 발생: {str(e)}")
                
                # 마지막 계좌 업데이트 시간 갱신
                last_account_update_time = current_time
            
            # 60초마다 매매 결정 수집 - 실제 주문 처리는 순차적으로 진행
            if (current_time - last_trading_decision_time).total_seconds() >= TRADING_DECISION_INTERVAL:
                logger.info(f"매매 결정 수집 시작 (실행 중인 봇 수: {len(running_bots)})")
                
                # 모든 봇의 매매 결정을 수집하는 함수
                async def collect_trading_decisions():
                    all_decisions = []  # 모든 봇의 모든 매매 결정을 담을 리스트
                    bot_decisions_count = {}  # 봇별 매매 결정 개수 추적
                    
                    for email, bot in running_bots.items():
                        try:
                            logger.info(f"봇 [{email}] 매매 결정 수집 시작 (전략: {bot.strategy})")
                            
                            if bot.trading_model:
                                # 현재가 정보는 bot_stock_cache를 통해 가져오도록 수정
                                prices = {}
                                filtered_symbols = bot.bot_stock_cache.get_filtered_stocks()
                                for symbol in filtered_symbols:
                                    price = bot.bot_stock_cache.get_price(symbol)
                                    if price:
                                        prices[symbol] = price
                                
                                # 매매 결정 요청
                                try:
                                    decisions = await bot.trading_model.get_trade_decisions(prices)
                                    
                                    if decisions:
                                        # 한 봇당 최대 처리할 매수 주문 제한
                                        buy_decisions = [d for d in decisions if d.get('action', '').lower() == 'buy']
                                        sell_decisions = [d for d in decisions if d.get('action', '').lower() == 'sell']
                                        
                                        # 최대 3개의 매수 주문만 허용 (신뢰도 기준 정렬)
                                        buy_decisions.sort(key=lambda x: x.get('confidence', 0), reverse=True)
                                        buy_decisions = buy_decisions[:3]  # 상위 3개만 유지
                                        
                                        # 최종 결정 목록
                                        limited_decisions = sell_decisions + buy_decisions
                                        
                                        logger.info(f"봇 [{email}] 매매 결정: {len(limited_decisions)}개 (원래: {len(decisions)}개, 제한 후: 매도 {len(sell_decisions)}개, 매수 {len(buy_decisions)}개)")
                                        
                                        # 각 결정에 봇 정보 추가 (이메일, backend_client 참조)
                                        for decision in limited_decisions:
                                            decision['bot_email'] = email
                                            decision['bot_reference'] = bot
                                        
                                        # 봇별 결정 카운트 추적
                                        bot_decisions_count[email] = len(limited_decisions)
                                        
                                        # 전체 결정 목록에 추가
                                        all_decisions.extend(limited_decisions)
                                    else:
                                        logger.info(f"봇 [{email}] 매매 결정: 없음")
                                except Exception as e:
                                    logger.error(f"봇 [{email}]의 매매 결정 수집 중 오류: {str(e)}")
                            
                            logger.info(f"봇 [{email}] 매매 결정 수집 완료")
                        except Exception as e:
                            logger.error(f"봇 [{email}]의 매매 결정 수집 중 오류: {str(e)}")
                    
                    # 봇별 결정 카운트 요약 로깅
                    if bot_decisions_count:
                        decisions_summary = ", ".join([f"{email}: {count}개" for email, count in bot_decisions_count.items()])
                        logger.info(f"봇별 매매 결정 수: {decisions_summary}")
                    
                    return all_decisions
                
                # 모든 봇의 매매 결정 수집
                try:
                    all_decisions = await collect_trading_decisions()
                    
                    # 매매 결정 정렬 (매도 먼저, 그 다음 매수 - 신뢰도 순)
                    sell_decisions = [d for d in all_decisions if d.get('action', '').lower() == 'sell']
                    buy_decisions = [d for d in all_decisions if d.get('action', '').lower() == 'buy']
                    
                    # 각각 신뢰도 순으로 정렬
                    sell_decisions.sort(key=lambda x: x.get('confidence', 0), reverse=True)
                    buy_decisions.sort(key=lambda x: x.get('confidence', 0), reverse=True)
                    
                    # 매도를 먼저 처리하도록 정렬된 결정 생성
                    sorted_decisions = sell_decisions + buy_decisions
                    
                    if sorted_decisions:
                        logger.info(f"총 {len(sorted_decisions)}개 매매 결정 수집 완료 (매도: {len(sell_decisions)}개, 매수: {len(buy_decisions)}개)")
                        
                        # 기존 대기 목록에 추가
                        order_queue.extend(sorted_decisions)
                        logger.info(f"현재 처리 대기 중인 주문: {len(order_queue)}개")
                    else:
                        logger.info("수집된 매매 결정 없음")
                
                except Exception as e:
                    logger.error(f"매매 결정 수집 중 오류 발생: {str(e)}")
                
                # 마지막 매매 결정 시간 갱신
                last_trading_decision_time = current_time
            
            # 대기 중인 주문이 있고, 마지막 주문 처리 후 1초 이상 지났으면 처리
            if order_queue and (current_time - last_order_time).total_seconds() >= 1.0:
                # 큐에서 첫 번째 주문 가져오기
                order = order_queue.pop(0)
                
                # 주문 처리
                try:
                    symbol = order.get("symbol")
                    action = order.get("action", "").lower()
                    quantity = order.get("quantity", 0)
                    price = order.get("price", 0)
                    
                    # 봇 정보 가져오기
                    bot_email = order.get("bot_email")
                    bot = order.get("bot_reference")
                    
                    if bot and bot.backend_client:
                        # 봇 자신의 백엔드 클라이언트를 통해 거래 요청 전송
                        if action == "buy":
                            logger.info(f"봇 [{bot_email}] 매수 요청 시작: {symbol} {quantity}주, 가격: {price}")
                            result = await bot.backend_client.request_buy(symbol, quantity, price)
                            if result:
                                logger.info(f"봇 [{bot_email}] 매수 요청 성공: {symbol} {quantity}주, 가격: {price}")
                            else:
                                logger.error(f"봇 [{bot_email}] 매수 요청 실패: {symbol} {quantity}주, 가격: {price}")
                        
                        elif action == "sell":
                            logger.info(f"봇 [{bot_email}] 매도 요청 시작: {symbol} {quantity}주, 가격: {price}")
                            result = await bot.backend_client.request_sell(symbol, quantity, price)
                            if result:
                                logger.info(f"봇 [{bot_email}] 매도 요청 성공: {symbol} {quantity}주, 가격: {price}")
                            else:
                                logger.error(f"봇 [{bot_email}] 매도 요청 실패: {symbol} {quantity}주, 가격: {price}")
                    else:
                        logger.error(f"주문 처리 실패: 봇 또는 백엔드 클라이언트가 없음 (봇: {bot_email})")
                
                except Exception as e:
                    logger.error(f"주문 처리 중 오류: {str(e)}")
                
                # 마지막 주문 시간 갱신
                last_order_time = current_time
                
                # 남은 주문 로깅
                logger.info(f"주문 처리 완료. 남은 대기 주문: {len(order_queue)}개")
            
            # 짧은 대기
            await asyncio.sleep(0.1)
            
        except asyncio.CancelledError:
            logger.info("거래 처리 루프가 취소되었습니다.")
            break
        except Exception as e:
            logger.error(f"거래 처리 루프 오류: {str(e)}")
            await asyncio.sleep(30)  # 오류 시 30초 후 재시도

async def scheduler_task():
    """매일 오전 7시에 서버 초기화 및 데이터 갱신을 실행하는 스케줄러"""
    global kiwoom_api, service_status, bot_manager, backend_client, auth_manager
    
    logger.info("스케줄러 작업 시작됨")
    
    while True:
        try:
            # 다음 실행 시간까지 대기할 시간 계산 (오전 7시)
            wait_seconds = await get_next_run_time(7, 0, 0)
            logger.info(f"다음 서버 초기화 및 데이터 갱신까지 {wait_seconds:.2f}초 남음")
            
            # 다음 실행 시간까지 대기
            await asyncio.sleep(wait_seconds)
            
            logger.info("정기 스케줄에 따른 서버 초기화 시작")
            
            # 서비스 상태 초기화
            service_status["is_running"] = False
            
            # 현재 활성화된 전략 확인
            current_strategy = service_status.get("active_strategy", TradingStrategy.ENVELOPE)
            
            # 모든 봇 정리
            if bot_manager:
                await bot_manager.cleanup()
                logger.info("봇 매니저 리소스 정리 완료")
            
            # 백엔드 클라이언트 명시적 종료
            if backend_client:
                await backend_client.close()
                logger.info("백엔드 클라이언트 종료 완료")
                backend_client = None  # 변수 초기화
            
            # 인증 매니저 명시적 종료
            if auth_manager:
                await auth_manager.close()
                logger.info("인증 매니저 종료 완료")
                auth_manager = None  # 변수 초기화
            
            # 키움 API 연결 종료
            if kiwoom_api:
                await kiwoom_api.close()
                logger.info("키움 API 연결 종료 완료")
                kiwoom_api = None  # 변수 초기화
            
            # 잠시 대기 (리소스 정리를 위한 시간)
            await asyncio.sleep(5)
            
            # 서비스 다시 초기화
            logger.info(f"서비스 재초기화 시작 (전략: {current_strategy})")
            service_initialized = await initialize_service(current_strategy)
            
            now = datetime.now()

            if now.weekday() == 6:
                try:
                    logger.info("DRL-UTrans 모델 학습용 데이터 준비 시작")
                    
                    # 모델 학습용 데이터 저장 및 압축
                    save_result = save_all_chart_data(kiwoom_api.stock_cache)
                    dataset_path = prepare_dataset_for_upload(compress=True)
                    
                    logger.info(f"학습용 데이터셋 준비 완료: {dataset_path}")
                except Exception as e:
                    logger.error(f"DRL-UTrans 데이터 준비 중 오류: {str(e)}")
            
            if not service_initialized:
                logger.error("서비스 재초기화 실패")
                # 실패 시 30분 후 재시도
                await asyncio.sleep(1800)
                continue
                
            logger.info("서비스 재초기화 성공")
            
        except asyncio.CancelledError:
            logger.info("스케줄러 작업이 취소되었습니다.")
            break
        except Exception as e:
            logger.error(f"스케줄러 작업 중 오류 발생: {str(e)}")
            # 10분 후 재시도
            await asyncio.sleep(600)


@app.on_event("startup")
async def startup_event():
    """애플리케이션 시작 시 실행"""
    global bot_manager, auth_manager, token_manager, backend_client, kiwoom_api
    
    logger.info("애플리케이션 시작 중...")
    
    try:
        # 기본 전략 설정
        default_strategy_str = settings.DEFAULT_STRATEGY.upper() if hasattr(settings, 'DEFAULT_STRATEGY') else "ENVELOPE"
        default_strategy = TradingStrategy.ENVELOPE  # 기본값
        
        # 전략 문자열을 열거형으로 변환
        if default_strategy_str == "BOLLINGER":
            default_strategy = TradingStrategy.BOLLINGER
        
        # 서비스 초기화 - 모든 설정 및 봇 관리 여기서 처리
        service_initialized = await initialize_service(default_strategy)
        
        if not service_initialized:
            logger.error("서비스 초기화 실패")
            logger.info("서비스 초기화 재시도")
            asyncio.sleep(300)
            service_initialized = await initialize_service(default_strategy)

            if not service_initialized:
                logger.error("서비스 초기화 실패")
                shutdown_event()
            
        logger.info("서비스 초기화 성공")

        # 모니터링 기능 추가
        add_monitor_to_app(app, kiwoom_api, bot_manager)
        logger.info("모니터링 기능 추가 완료")
        
    except Exception as e:
        logger.error(f"애플리케이션 시작 중 오류 발생: {str(e)}", exc_info=True)
    
    logger.info("애플리케이션 시작 완료")

@app.on_event("shutdown")
async def shutdown_event():
    """애플리케이션 종료 시 실행"""
    global bot_manager, auth_manager, kiwoom_api, token_manager, backend_client
    global trading_loop_task, scheduler_task_instance
    
    logger.info("애플리케이션 종료 중...")
    
    # 서비스 상태 업데이트
    service_status["is_running"] = False
    
    # 1. 먼저 실행 중인 태스크 취소
    if trading_loop_task and not trading_loop_task.done():
        trading_loop_task.cancel()
        try:
            await trading_loop_task
        except asyncio.CancelledError:
            logger.info("거래 루프 태스크가 취소되었습니다.")
        
    if scheduler_task_instance and not scheduler_task_instance.done():
        scheduler_task_instance.cancel()
        try:
            await scheduler_task_instance
        except asyncio.CancelledError:
            logger.info("스케줄러 태스크가 취소되었습니다.")
    
    # 2. 모든 태스크가 정리될 때까지 충분한 시간 대기
    await asyncio.sleep(1)
    
    # 3. 순서대로 리소스 정리
    try:
        # 모든 봇 정리
        if bot_manager:
            await bot_manager.cleanup()
            logger.info("봇 매니저 리소스 정리 완료")
        
        # 백엔드 클라이언트 명시적 종료
        if backend_client:
            await backend_client.close()
            logger.info("백엔드 클라이언트 종료 완료")
        
        # 키움 API 연결 종료
        if kiwoom_api:
            await kiwoom_api.close()
            logger.info("키움 API 연결 종료 완료")
        
        # 인증 매니저 종료
        if auth_manager:
            await auth_manager.close()
            logger.info("인증 매니저 종료 완료")
        
        # 토큰 관리자 종료
        if token_manager:
            await token_manager.close()
            logger.info("토큰 매니저 종료 완료")
        
    except Exception as e:
        logger.error(f"리소스 정리 중 오류: {str(e)}")
    
    # 4. 모든 리소스 정리 후 대기
    await asyncio.sleep(2)
    
    # 5. 남아있는 모든 태스크 강제 종료
    tasks = [t for t in asyncio.all_tasks() if t is not asyncio.current_task()]
    if tasks:
        logger.info(f"종료되지 않은 태스크 {len(tasks)}개가 있습니다. 강제 종료합니다.")
        for task in tasks:
            task.cancel()
            
        # 모든 태스크 종료 대기
        try:
            await asyncio.wait_for(asyncio.gather(*tasks, return_exceptions=True), timeout=5.0)
            logger.info("모든 태스크가 종료되었습니다.")
        except asyncio.TimeoutError:
            logger.warning("일부 태스크가 5초 이내에 종료되지 않았습니다.")
    
    logger.info("애플리케이션 종료 완료")