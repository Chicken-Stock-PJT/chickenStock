import asyncio
import logging
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
auth_client = None
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

async def get_next_run_time(target_hour=9, target_minute=0, target_second=0):
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
    global auth_client, kiwoom_api, backend_client, service_status, token_manager, bot_manager
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
        
        # 키움 API 토큰 발급
        kiwoom_auth_client = KiwoomAuthClient()
        kiwoom_auth_client.set_token_manager(token_manager)
        await kiwoom_auth_client.initialize()
        
        kiwoom_token = await kiwoom_auth_client.get_access_token()
        if not kiwoom_token:
            logger.error("키움 API 토큰 발급 실패")
            return False

        # 백엔드 서버 로그인 확인
        if not auth_client or not auth_client.is_authenticated:
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
            
            # 인증 클라이언트 초기화
            if not auth_client:
                auth_client = AuthClient()
                await auth_client.initialize()
            
            # 백엔드 서버 로그인
            success = await auth_client.login(default_email, default_password)
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
            backend_client.set_auth_client(auth_client)
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

        # 계좌 정보 초기화
        account_info = await backend_client.request_account_info()
        if account_info:
            kiwoom_api.update_account_info(account_info)
            
        # 차트 데이터 초기화 - 두 전략 중 더 긴 기간인 120일로 모든 종목 데이터 가져오기
        await kiwoom_api.initialize_chart_data(filtered_stockcode_list, period=120)
        logger.info(f"모든 종목({len(filtered_stockcode_list)}개)의 120일 차트 데이터 초기화 완료")

        # 모든 전략의 지표 계산 (두 전략 모두 미리 계산)
        # Envelope 지표 계산
        logger.info("Envelope 지표 계산 시작")
        kiwoom_api.stock_cache.calculate_envelope_indicators()
        logger.info("Envelope 지표 계산 완료")
        
        # 볼린저 밴드 지표 계산
        logger.info("볼린저 밴드 지표 계산 시작")
        kiwoom_api.stock_cache.calculate_bollinger_bands()
        logger.info("볼린저 밴드 지표 계산 완료")
            
        # 전략에 따른 트레이딩 모델 초기화는 제거
        # 공통 API 및 서비스 초기화만 여기서 진행
        
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
    """주기적인 매매 처리 루프"""
    global kiwoom_api, backend_client, bot_manager
    
    if not bot_manager:
        logger.error("거래 루프를 실행할 수 없습니다: 봇 매니저가 초기화되지 않았습니다.")
        return
    
    logger.info("거래 처리 루프 시작")
    
    # 마지막 처리 시간 초기화
    last_processing_time = datetime.now()
    
    while service_status["is_running"]:
        try:
            # 현재 시간
            current_time = datetime.now()
            
            # 10초마다 계좌 정보 업데이트 및 매매 결정 처리
            if (current_time - last_processing_time).total_seconds() >= 10:
                # 모든 실행 중인 봇의 매매 결정 처리
                running_bots = bot_manager.get_running_bots()
                logger.info(f"실행 중인 봇 수: {len(running_bots)}")
                
                # 병렬 처리를 위한 태스크 리스트
                bot_tasks = []
                
                # 각 봇에 대한 처리 함수 정의
                async def process_bot(email, bot):
                    try:
                        logger.info(f"봇 [{email}] 처리 시작 (전략: {bot.strategy})")
                        
                        # 인증 상태 확인
                        if not bot.auth_client or not bot.auth_client.is_authenticated:
                            logger.warning(f"봇 [{email}]의 인증이 유효하지 않습니다. 이 봇의 거래는 건너뜁니다.")
                            return
                        
                        # 계좌 정보 업데이트
                        logger.info(f"봇 [{email}] 계좌 정보 업데이트 요청 중...")
                        await bot.update_account_info()
                        
                        # 계좌 정보 로그 출력 (올바른 필드 이름 사용)
                        cash = bot.get_cash()  # get_cash 메서드 사용
                        holdings = bot.get_holdings()  # get_holdings 메서드 사용
                        logger.info(f"봇 [{email}] 계좌 정보 업데이트 성공: 예수금={cash}, 보유종목수={len(holdings)}")
                        
                        if bot.trading_model:
                            # 현재가 정보는 bot_stock_cache를 통해 가져오도록 수정
                            prices = {}
                            if hasattr(bot.trading_model, 'get_current_prices'):
                                prices = await bot.trading_model.get_current_prices()
                                price_count = len(prices) if prices else 0
                                logger.info(f"봇 [{email}] 캐싱된 현재가 종목 수: {price_count}")
                            
                            # 매매 결정 요청
                            logger.info(f"봇 [{email}] 매매 결정 요청 중...")
                            try:
                                decisions = await bot.trading_model.get_trade_decisions(prices)
                                
                                if decisions:
                                    logger.info(f"봇 [{email}] 매매 결정: {len(decisions)}개")
                                    for idx, decision in enumerate(decisions):
                                        logger.info(f"봇 [{email}] 매매 결정 #{idx+1}: {decision}")
                                else:
                                    logger.info(f"봇 [{email}] 매매 결정: 없음")
                                
                                # 매매 결정이 있으면 봇 자신의 백엔드 클라이언트로 요청 전송
                                for decision in decisions:
                                    try:
                                        symbol = decision.get("symbol")
                                        action = decision.get("action")
                                        quantity = decision.get("quantity", 0)
                                        price = decision.get("price", 0)
                                        
                                        # 봇 자신의 백엔드 클라이언트를 통해 거래 요청 전송
                                        if action.lower() == "buy":
                                            logger.info(f"봇 [{email}] 매수 요청 시작: {symbol} {quantity}주, 가격: {price}")
                                            result = await bot.backend_client.request_buy(symbol, quantity, price)
                                            if result:
                                                logger.info(f"봇 [{email}] 매수 요청 성공: {symbol} {quantity}주, 가격: {price}")
                                            else:
                                                logger.error(f"봇 [{email}] 매수 요청 실패: {symbol} {quantity}주, 가격: {price}")
                                        
                                        elif action.lower() == "sell":
                                            logger.info(f"봇 [{email}] 매도 요청 시작: {symbol} {quantity}주, 가격: {price}")
                                            result = await bot.backend_client.request_sell(symbol, quantity, price)
                                            if result:
                                                logger.info(f"봇 [{email}] 매도 요청 성공: {symbol} {quantity}주, 가격: {price}")
                                            else:
                                                logger.error(f"봇 [{email}] 매도 요청 실패: {symbol} {quantity}주, 가격: {price}")
                                    
                                    except Exception as e:
                                        logger.error(f"봇 [{email}]의 거래 요청 전송 중 오류: {str(e)}")
                            except Exception as e:
                                logger.error(f"봇 [{email}]의 매매 결정 처리 중 오류: {str(e)}")
                                
                        logger.info(f"봇 [{email}] 처리 완료")
                    except Exception as e:
                        logger.error(f"봇 [{email}]의 매매 처리 중 오류: {str(e)}")
                
                # 각 봇에 대한 태스크 생성
                for email, bot in running_bots.items():
                    bot_tasks.append(process_bot(email, bot))
                
                # 모든 봇 태스크를 병렬로 실행 (타임아웃 5초 설정)
                try:
                    # asyncio.wait_for를 사용하여 전체 gather에 타임아웃 설정
                    await asyncio.wait_for(asyncio.gather(*bot_tasks), timeout=5.0)
                except asyncio.TimeoutError:
                    logger.warning("일부 봇 처리가 시간 초과로 완료되지 않았습니다.")
                except Exception as e:
                    logger.error(f"봇 병렬 처리 중 오류 발생: {str(e)}")
                
                # 마지막 처리 시간 업데이트
                last_processing_time = current_time
            
            # 1초 대기
            await asyncio.sleep(1)
            
        except asyncio.CancelledError:
            logger.info("거래 처리 루프가 취소되었습니다.")
            break
        except Exception as e:
            logger.error(f"거래 처리 루프 오류: {str(e)}")
            await asyncio.sleep(30)  # 오류 시 30초 후 재시도

async def scheduler_task():
    """매일 오전 8시에 실행되는 작업 스케줄러"""
    global kiwoom_api, service_status, bot_manager
    
    logger.info("스케줄러 작업 시작됨")
    
    while True:
        try:
            # 서비스가 실행 중인지 확인
            if not service_status.get("is_running", False):
                logger.info("서비스가 실행 중이 아니므로 스케줄러 작업을 일시 중지합니다.")
                await asyncio.sleep(60)  # 1분마다 서비스 상태 확인
                continue
            
            # 다음 실행 시간까지 대기할 시간 계산 (오전 8시)
            wait_seconds = await get_next_run_time(8, 0, 0)
            logger.info(f"다음 차트 데이터 수집 및 지표 계산까지 {wait_seconds:.2f}초 남음")
            
            # 다음 실행 시간까지 대기
            await asyncio.sleep(wait_seconds)
            
            # 서비스가 여전히 실행 중이면 작업 수행
            if service_status.get("is_running", False) and kiwoom_api:
                logger.info("정기 스케줄에 따른 차트 데이터 수집 및 지표 계산 시작")
                
                # 공유 캐시 차트 데이터 업데이트 (120일 - 모든 전략을 위한 충분한 데이터)
                filtered_stockcode_list = kiwoom_api.stock_cache.filtered_stockcode_list
                await kiwoom_api.initialize_chart_data(filtered_stockcode_list, period=120)
                logger.info(f"차트 데이터 업데이트 완료: {len(filtered_stockcode_list)}개 종목")
                
                # 모든 봇의 개별 지표 새로고침 (각자의 전략에 맞게)
                if bot_manager:
                    logger.info("모든 봇의 지표 새로고침 시작")
                    refresh_results = await bot_manager.refresh_all_bot_indicators()
                    
                    # 결과 로깅
                    for email, count in refresh_results.items():
                        logger.info(f"봇 [{email}] 지표 새로고침 결과: {count}개 종목 성공")
                    
                    logger.info(f"모든 봇({len(refresh_results)}개)의 지표 새로고침 완료")
                
                # 마지막 데이터 업데이트 시간 기록
                service_status["last_data_update"] = datetime.now()
                
                logger.info("정기 스케줄에 따른 차트 데이터 수집 및 지표 계산 완료")
            
        except asyncio.CancelledError:
            logger.info("스케줄러 작업이 취소되었습니다.")
            break
        except Exception as e:
            logger.error(f"스케줄러 작업 중 오류 발생: {str(e)}")
            # 30분 후 재시도
            await asyncio.sleep(1800)


@app.on_event("startup")
async def startup_event():
    """애플리케이션 시작 시 실행"""
    global bot_manager, auth_client, token_manager, backend_client, kiwoom_api
    
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
            return
            
        logger.info("서비스 초기화 성공")
        
    except Exception as e:
        logger.error(f"애플리케이션 시작 중 오류 발생: {str(e)}", exc_info=True)
    
    logger.info("애플리케이션 시작 완료")

@app.on_event("shutdown")
async def shutdown_event():
    """애플리케이션 종료 시 실행"""
    global bot_manager, auth_client, kiwoom_api, token_manager, backend_client
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
        
        # 인증 클라이언트 종료
        if auth_client:
            await auth_client.close()
            logger.info("인증 클라이언트 종료 완료")
        
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

# 모니터링 전용 엔드포인트

@app.get("/status")
async def get_status():
    """서비스 상태 확인"""
    global kiwoom_api, bot_manager
    
    status_response = {
        "is_running": service_status.get("is_running", False),
        "connected_to_api": False,
        "subscribed_symbols": 0,
        "start_time": service_status.get("start_time").isoformat() if service_status.get("start_time") else None,
        "last_data_update": service_status.get("last_data_update").isoformat() if service_status.get("last_data_update") else None,
        "active_strategy": service_status.get("active_strategy", "").value if service_status.get("active_strategy") else None,
        "account_info": None
    }
    
    # 키움 API 상태 확인
    if kiwoom_api:
        # is_connected 대신 websocket이 있는지 확인하는 방식으로 수정
        status_response["connected_to_api"] = kiwoom_api.websocket is not None and not kiwoom_api.websocket.closed if hasattr(kiwoom_api, 'websocket') else False
        
        # 구독된 종목 수 가져오기
        if hasattr(kiwoom_api, 'stock_cache') and hasattr(kiwoom_api.stock_cache, 'get_subscribed_symbols_count'):
            status_response["subscribed_symbols"] = kiwoom_api.stock_cache.get_subscribed_symbols_count()
        else:
            # 메소드가 없으면 구독된 종목 목록의 길이로 대체
            subscribed_symbols = getattr(kiwoom_api.stock_cache, 'subscribed_symbols', []) if hasattr(kiwoom_api, 'stock_cache') else []
            status_response["subscribed_symbols"] = len(subscribed_symbols)
        
        # 계좌 정보 확인
        account_info = kiwoom_api.get_account_info() if hasattr(kiwoom_api, 'get_account_info') else None
        if account_info:
            status_response["account_info"] = account_info
    
    # 실행 중인 봇 정보 추가
    if bot_manager:
        running_bots = bot_manager.get_running_bots() if hasattr(bot_manager, 'get_running_bots') else {}
        status_response["running_bots_count"] = len(running_bots)
            
    return status_response

@app.get("/prices")
async def get_prices():
    """실시간 종목 가격 정보 조회"""
    global kiwoom_api
    
    if not kiwoom_api:
        raise HTTPException(
            status_code=503,
            detail="서비스가 아직 초기화되지 않았습니다."
        )
    
    try:
        # 구독 중인 종목 목록 가져오기
        subscribed_symbols = []
        if hasattr(kiwoom_api.stock_cache, 'subscribed_symbols'):
            subscribed_symbols = kiwoom_api.stock_cache.subscribed_symbols
        
        # 필터링된 종목 목록 (subscribed_symbols이 비어있으면 이것을 사용)
        filtered_symbols = []
        if hasattr(kiwoom_api.stock_cache, 'filtered_stockcode_list'):
            filtered_symbols = kiwoom_api.stock_cache.filtered_stockcode_list
            
        # 사용할 종목 목록 결정 (구독 중인 종목이 있으면 그것을 사용, 없으면 필터링된 종목 목록)
        symbols_to_use = subscribed_symbols if subscribed_symbols else filtered_symbols
        
        # 반환할 가격 정보 객체
        prices = {}
        
        # 종목별 가격 및 정보 가져오기
        for symbol in symbols_to_use:
            try:
                # 기본 정보
                stock_info = {
                    "price": 0,
                    "name": "",
                    "change": 0,
                    "changePercent": 0
                }
                
                # 종목 이름 가져오기
                if hasattr(kiwoom_api.stock_cache, 'get_stock_name'):
                    stock_info["name"] = kiwoom_api.stock_cache.get_stock_name(symbol) or ""
                
                # 현재가 가져오기
                if hasattr(kiwoom_api.stock_cache, 'get_price'):
                    current_price = kiwoom_api.stock_cache.get_price(symbol)
                    if current_price:
                        stock_info["price"] = current_price
                
                # 전일대비 및 등락률 가져오기
                # 이 부분은 KiwoomAPI 구현에 따라 달라질 수 있음
                if hasattr(kiwoom_api.stock_cache, 'get_change'):
                    change = kiwoom_api.stock_cache.get_change(symbol)
                    if change is not None:
                        stock_info["change"] = change
                
                if hasattr(kiwoom_api.stock_cache, 'get_change_percent'):
                    change_percent = kiwoom_api.stock_cache.get_change_percent(symbol)
                    if change_percent is not None:
                        stock_info["changePercent"] = change_percent
                
                # 가격이 0보다 큰 경우에만 추가 (유효한 데이터만 포함)
                if stock_info["price"] > 0:
                    prices[symbol] = stock_info
            
            except Exception as e:
                logger.error(f"종목 {symbol} 가격 정보 조회 중 오류: {str(e)}")
        
        return prices
    
    except Exception as e:
        logger.error(f"가격 정보 조회 중 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail="가격 정보 조회 중 오류가 발생했습니다."
        )

@app.get("/account")
async def get_account_info():
    """계좌 정보 확인"""
    global kiwoom_api
    
    if not kiwoom_api:
        raise HTTPException(
            status_code=503,
            detail="서비스가 아직 초기화되지 않았습니다."
        )
    
    account_info = kiwoom_api.get_account_info()
    
    if not account_info:
        raise HTTPException(
            status_code=404,
            detail="계좌 정보를 찾을 수 없습니다."
        )
    
    return account_info

@app.get("/bots/active")
async def get_active_bots():
    """실행 중인 봇 목록 확인"""
    global bot_manager
    
    if not bot_manager:
        raise HTTPException(
            status_code=503,
            detail="봇 관리자가 초기화되지 않았습니다."
        )
    
    # 실행 중인 봇 가져오기
    running_bots = bot_manager.get_running_bots()
    
    # 응답 형식에 맞게 변환
    response = []
    for email, bot in running_bots.items():
        bot_status = bot.get_status()
        response.append(bot_status)
    
    return response

@app.post("/refresh")
async def refresh_data():
    """데이터 새로고침"""
    global kiwoom_api, backend_client
    
    if not kiwoom_api or not backend_client:
        raise HTTPException(
            status_code=503,
            detail="서비스가 아직 초기화되지 않았습니다."
        )
    
    try:
        # 계좌 정보 업데이트
        account_info = await backend_client.request_account_info()
        if account_info:
            kiwoom_api.update_account_info(account_info)
            
        # 현재 시간 기록
        refresh_time = datetime.now()
        
        return {
            "success": True,
            "message": "데이터가 성공적으로 새로고침되었습니다.",
            "refresh_time": refresh_time.isoformat()
        }
    except Exception as e:
        logger.error(f"데이터 새로고침 중 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail=f"데이터 새로고침 중 오류: {str(e)}"
        )