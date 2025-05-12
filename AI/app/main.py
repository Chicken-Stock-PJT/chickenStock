import asyncio
import logging
from typing import Dict, List, Optional, Any
from datetime import datetime, timedelta, time

from fastapi import FastAPI, HTTPException, Depends, Query, Body
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

from app.models.trade_models import TradingStrategy
from app.auth.token_manager import TokenManager
from app.auth.kiwoom_auth import KiwoomAuthClient
from app.auth.auth_client import AuthClient

from app.api.backend_client import BackendClient
from app.api.kiwoom_api import KiwoomAPI
from app.strategies.bollinger import BollingerBandTradingModel
from app.strategies.envelope import EnvelopeTradingModel

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

class LoginRequest(BaseModel):
    """로그인 요청 모델"""
    email: str
    password: str
    platform: str = "mobile"
    override_strategy: Optional[TradingStrategy] = None

class BotCreateRequest(BaseModel):
    """봇 생성 요청 모델"""
    email: str
    password: str
    strategy: TradingStrategy

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
    
    try:
        logger.info(f"자동매매 서비스 초기화 시작 (전략: {strategy})")
        
        # 토큰 관리자 초기화
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
            default_email = settings.DEFAULT_EMAIL
            default_password = settings.DEFAULT_PASSWORD
            
            if not default_email or not default_password:
                logger.error("기본 계정 정보가 없습니다. settings.py에서 DEFAULT_EMAIL과 DEFAULT_PASSWORD를 설정하세요.")
                return False
            
            # 인증 클라이언트 초기화
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
        kiwoom_api = KiwoomAPI(token_manager)
        
        # 백엔드 클라이언트 초기화
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
            
        await kiwoom_api.initialize_stock_list(stock_list)
        
        # 필터링된 종목 리스트 가져오기 (시가총액 기준)
        initial_filtered_symbols = await kiwoom_api.get_filtered_symbols(450, 150)
        logger.info(f"시가총액 기준 초기 필터링 완료: 총 {len(initial_filtered_symbols)}개 종목")
        
        # 필터링된 종목 중 실제 stock_list에 있는 종목만 추출
        available_symbols = [
            symbol for symbol in initial_filtered_symbols
            if any(stock.get('shortCode') == symbol for stock in stock_list)
        ]
        
        # 정확히 600개 종목을 선택 (또는 가능한 최대)
        target_count = min(600, len(available_symbols))
        final_symbols = available_symbols[:target_count]
        
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
            
        # 전략에 따른 차트 데이터 수집 및 지표 계산
        if strategy == TradingStrategy.ENVELOPE:
            # Envelope 지표 계산용 차트 데이터 초기화 (120일 데이터)
            await kiwoom_api.initialize_chart_data(filtered_stockcode_list, period=120)
            # Envelope 지표 계산
            kiwoom_api.stock_cache.calculate_envelope_indicators()
            
            # 트레이딩 모델 초기화
            trading_model = EnvelopeTradingModel(kiwoom_api)
            trading_model.set_backend_client(backend_client)
        else:
            # 볼린저 밴드 지표 계산용 차트 데이터 초기화 (60일 데이터)
            await kiwoom_api.initialize_chart_data(filtered_stockcode_list, period=60)
            # 볼린저 밴드 지표 계산
            kiwoom_api.stock_cache.calculate_bollinger_bands()
            
            # 트레이딩 모델 초기화
            trading_model = BollingerBandTradingModel(kiwoom_api)
            trading_model.set_backend_client(backend_client)
        
        # 마지막 데이터 업데이트 시간 기록
        service_status["last_data_update"] = datetime.now()
        
        # 트레이딩 모델 시작
        await trading_model.start()
        
        # 실시간 데이터 구독 준비
        await kiwoom_api.prepare_subscription_groups(filtered_stock_list, 30)
        
        # 실시간 데이터 구독 로테이션 시작
        asyncio.create_task(
            kiwoom_api.start_rotating_subscriptions(trading_model.handle_realtime_price)
        )
        
        # 서비스 상태 업데이트
        service_status["is_running"] = True
        service_status["start_time"] = datetime.now()
        service_status["active_strategy"] = strategy
        
        # 기본 봇 추가 (서비스가 시작될 때 기본 계정으로 봇 생성)
        if bot_manager and default_email and default_password:
            default_bot = await bot_manager.create_bot(default_email, default_password, strategy)
            if default_bot:
                # 생성된 봇에 트레이딩 모델 설정
                default_bot.trading_model = trading_model
                await bot_manager.start_bot(default_email)
                logger.info(f"기본 봇 생성 및 시작 성공: {default_email} (전략: {strategy})")
        
        # settings에서 추가 계정 정보 확인 및 봇 생성
        # settings.ADDITIONAL_ACCOUNTS가 있는지 확인하고, 있으면 해당 계정들로 봇 생성
        if hasattr(settings, 'ADDITIONAL_ACCOUNTS') and settings.ADDITIONAL_ACCOUNTS:
            logger.info(f"추가 계정으로 봇 생성 시작 (총 {len(settings.ADDITIONAL_ACCOUNTS)}개 계정)")
            
            for account_info in settings.ADDITIONAL_ACCOUNTS:
                email = account_info.get('email')
                password = account_info.get('password')
                account_strategy = account_info.get('strategy', strategy)  # 기본값은 현재 서비스 전략
                
                if email and password:
                    # 전략 문자열을 열거형으로 변환
                    if isinstance(account_strategy, str):
                        if account_strategy.upper() == "BOLLINGER":
                            account_strategy = TradingStrategy.BOLLINGER
                        else:
                            account_strategy = TradingStrategy.ENVELOPE
                
                    # 봇 생성
                    try:
                        # 이미 존재하는 봇인지 확인
                        existing_bot = bot_manager.get_bot(email)
                        
                        if existing_bot:
                            # 봇이 있으면 전략 업데이트 및 시작
                            existing_bot.strategy = account_strategy
                            
                            # 봇에 새 트레이딩 모델 생성
                            if account_strategy == TradingStrategy.ENVELOPE:
                                bot_trading_model = EnvelopeTradingModel(kiwoom_api)
                            else:
                                bot_trading_model = BollingerBandTradingModel(kiwoom_api)
                                
                            bot_trading_model.set_backend_client(backend_client)
                            await bot_trading_model.start()
                            
                            # 봇에 트레이딩 모델 설정
                            existing_bot.trading_model = bot_trading_model
                            
                            # 봇 시작
                            await bot_manager.start_bot(email)
                            logger.info(f"기존 봇 업데이트 및 시작 성공: {email} (전략: {account_strategy})")
                        else:
                            # 새 봇 생성
                            new_bot = await bot_manager.create_bot(email, password, account_strategy)
                            if new_bot:
                                # 봇에 맞는 트레이딩 모델 생성
                                if account_strategy == TradingStrategy.ENVELOPE:
                                    bot_trading_model = EnvelopeTradingModel(kiwoom_api)
                                else:
                                    bot_trading_model = BollingerBandTradingModel(kiwoom_api)
                                    
                                bot_trading_model.set_backend_client(backend_client)
                                await bot_trading_model.start()
                                
                                # 봇에 트레이딩 모델 설정
                                new_bot.trading_model = bot_trading_model
                                
                                # 봇 시작
                                await bot_manager.start_bot(email)
                                logger.info(f"추가 봇 생성 및 시작 성공: {email} (전략: {account_strategy})")
                            else:
                                logger.error(f"추가 봇 생성 실패: {email}")
                                
                    except Exception as e:
                        logger.error(f"추가 계정 봇 생성 중 오류: {email}, 오류: {str(e)}")
        
        # 거래 처리 루프 태스크 시작
        asyncio.create_task(trading_loop())
        
        # 스케줄러 (정기 데이터 갱신) 태스크 시작
        asyncio.create_task(scheduler_task())
        
        logger.info(f"자동매매 서비스 초기화 완료 (전략: {strategy})")
        return True
        
    except Exception as e:
        logger.error(f"서비스 초기화 중 오류 발생: {str(e)}", exc_info=True)
        service_status["is_running"] = False
        return False

async def trading_loop():
    """주기적인 매매 처리 루프"""
    global kiwoom_api, backend_client, bot_manager
    
    if not kiwoom_api or not backend_client:
        logger.error("거래 루프를 실행할 수 없습니다: API 또는 백엔드 클라이언트가 초기화되지 않았습니다.")
        return
    
    logger.info("거래 처리 루프 시작")
    
    # 마지막 처리 시간 초기화
    last_processing_time = datetime.now()
    
    while service_status["is_running"]:
        try:
            # 백엔드 서버 인증 상태 확인
            if not auth_client or not auth_client.is_authenticated:
                logger.error("백엔드 서버 인증되지 않았습니다. 거래를 건너뜁니다.")
                await asyncio.sleep(30)
                continue
            
            # 현재 시간
            current_time = datetime.now()
            
            # 10초마다 계좌 정보 업데이트 및 매매 결정 처리
            if (current_time - last_processing_time).total_seconds() >= 10:
                # 계좌 정보 업데이트 (백엔드에서 가져옴)
                account_info = await backend_client.request_account_info()
                if account_info:
                    kiwoom_api.update_account_info(account_info)
                
                # 현재 활성화된 전략 가져오기
                active_strategy = service_status.get("active_strategy")
                
                # 모든 실행 중인 봇의 매매 결정 처리
                if bot_manager:
                    running_bots = bot_manager.get_running_bots()
                    for email, bot in running_bots.items():
                        # 전략 일치 여부 확인은 옵션이며, 필요에 따라 조정
                        # if bot.strategy == active_strategy:
                        if hasattr(bot, 'trading_model') and bot.trading_model:
                            # 트레이딩 모델에서 매매 결정 가져오기
                            decisions = await bot.trading_model.get_trade_decisions()
                            
                            # 매매 결정이 있으면 백엔드로 요청 전송
                            for decision in decisions:
                                try:
                                    symbol = decision.get("symbol")
                                    action = decision.get("action")
                                    quantity = decision.get("quantity", 0)
                                    price = decision.get("price", 0)
                                    
                                    # 백엔드 클라이언트를 통해 거래 요청 전송
                                    if action.lower() == "buy":
                                        result = await backend_client.request_buy(symbol, quantity, price)
                                        if result:
                                            logger.info(f"매수 요청 성공: {symbol} {quantity}주 (봇: {email})")
                                        else:
                                            logger.error(f"매수 요청 실패: {symbol} {quantity}주 (봇: {email})")
                                    
                                    elif action.lower() == "sell":
                                        result = await backend_client.request_sell(symbol, quantity, price)
                                        if result:
                                            logger.info(f"매도 요청 성공: {symbol} {quantity}주 (봇: {email})")
                                        else:
                                            logger.error(f"매도 요청 실패: {symbol} {quantity}주 (봇: {email})")
                                    
                                except Exception as e:
                                    logger.error(f"거래 요청 전송 중 오류: {str(e)}")
                
                # 마지막 처리 시간 업데이트
                last_processing_time = current_time
            
            # 1초 대기
            await asyncio.sleep(1)
            
        except Exception as e:
            logger.error(f"거래 처리 루프 오류: {str(e)}")
            await asyncio.sleep(30)  # 오류 시 30초 후 재시도

async def scheduler_task():
    """매일 오전 9시에 실행되는 작업 스케줄러"""
    global kiwoom_api, service_status, bot_manager
    
    logger.info("스케줄러 작업 시작됨")
    
    while True:
        try:
            # 서비스가 실행 중인지 확인
            if not service_status.get("is_running", False):
                logger.info("서비스가 실행 중이 아니므로 스케줄러 작업을 일시 중지합니다.")
                await asyncio.sleep(60)  # 1분마다 서비스 상태 확인
                continue
            
            # 다음 실행 시간까지 대기할 시간 계산 (오전 9시)
            wait_seconds = await get_next_run_time(9, 0, 0)
            logger.info(f"다음 차트 데이터 수집 및 지표 계산까지 {wait_seconds:.2f}초 남음")
            
            # 다음 실행 시간까지 대기
            await asyncio.sleep(wait_seconds)
            
            # 서비스가 여전히 실행 중이면 작업 수행
            if service_status.get("is_running", False) and kiwoom_api:
                logger.info("정기 스케줄에 따른 차트 데이터 수집 및 지표 계산 시작")
                
                # 현재 활성화된 전략 확인
                active_strategy = service_status.get("active_strategy")
                filtered_stockcode_list = kiwoom_api.stock_cache.filtered_stockcode_list
                
                # 전략에 따라 다른 지표 계산
                if active_strategy == TradingStrategy.ENVELOPE:
                    # Envelope 지표 계산용 차트 데이터 초기화
                    await kiwoom_api.initialize_chart_data(filtered_stockcode_list, period=120)
                    # Envelope 지표 계산
                    kiwoom_api.stock_cache.calculate_envelope_indicators()
                else:
                    # 볼린저 밴드 지표 계산용 차트 데이터 초기화
                    await kiwoom_api.initialize_chart_data(filtered_stockcode_list, period=60)
                    # 볼린저 밴드 지표 계산
                    kiwoom_api.stock_cache.calculate_bollinger_bands()
                
                # 마지막 데이터 업데이트 시간 기록
                service_status["last_data_update"] = datetime.now()
                
                # 봇 매니저가 있는 경우 해당 봇들의 트레이딩 모델 업데이트
                if bot_manager:
                    all_bots = bot_manager.get_all_bots()
                    for email, bot in all_bots.items():
                        if bot.is_running and hasattr(bot, 'trading_model'):
                            # 전략에 맞게 트레이딩 모델 업데이트
                            # 트레이딩 모델에서 데이터 갱신 후 계산 필요한 경우
                            if hasattr(bot.trading_model, 'refresh_indicators'):
                                await bot.trading_model.refresh_indicators()
                
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
    global bot_manager, auth_client, token_manager
    
    logger.info("애플리케이션 시작 중...")
    
    # 토큰 관리자 초기화
    token_manager = TokenManager()
    await token_manager.initialize()
    
    # 인증 클라이언트 초기화
    auth_client = AuthClient()
    await auth_client.initialize()
    
    # 봇 관리자 초기화
    bot_manager = BotManager()
    
    # 기본 계정으로 자동 로그인 시도
    try:
        # 설정에서 기본 계정 가져오기
        default_email = settings.DEFAULT_EMAIL
        default_password = settings.DEFAULT_PASSWORD
        
        if default_email and default_password:
            logger.info(f"기본 계정 {default_email}으로 자동 로그인 시도")
            success = await auth_client.login(default_email, default_password)
            
            if success:
                logger.info("백엔드 서버 자동 로그인 성공")
                
                # 서비스 상태 업데이트
                service_status["current_user"] = default_email
                service_status["is_running"] = True
                service_status["start_time"] = datetime.now()
                
                # 기본 전략 설정
                default_strategy_str = settings.DEFAULT_STRATEGY.upper()
                default_strategy = TradingStrategy.ENVELOPE  # 기본값
                
                # 전략 문자열을 열거형으로 변환
                if default_strategy_str == "BOLLINGER":
                    default_strategy = TradingStrategy.BOLLINGER
                
                service_status["active_strategy"] = default_strategy
                
                # 자동 초기화 함수 실행
                await initialize_service(default_strategy)
                
            else:
                logger.error("백엔드 서버 자동 로그인 실패")
        else:
            logger.info("기본 계정 정보가 없어 자동 로그인을 건너뜁니다.")
    except Exception as e:
        logger.error(f"자동 로그인 중 오류 발생: {str(e)}")
    
    logger.info("애플리케이션 시작 완료")

@app.on_event("shutdown")
async def shutdown_event():
    """애플리케이션 종료 시 실행"""
    global bot_manager, auth_client, kiwoom_api, token_manager
    
    logger.info("애플리케이션 종료 중...")
    
    # 서비스 상태 업데이트
    service_status["is_running"] = False
    
    # 모든 봇 정리
    if bot_manager:
        await bot_manager.cleanup()
    
    # 토큰 관리자 종료
    if token_manager:
        await token_manager.close()
    
    # 키움 API 연결 종료
    if kiwoom_api:
        await kiwoom_api.close()
    
    # 인증 클라이언트 종료
    if auth_client:
        await auth_client.close()
    
    logger.info("애플리케이션 종료 완료")

@app.post("/auth/login")
async def login(login_request: LoginRequest):
    """백엔드 API 로그인"""
    global auth_client
    
    if not auth_client:
        auth_client = AuthClient()
        await auth_client.initialize()
    
    try:
        # 백엔드 서버 로그인
        success = await auth_client.login(login_request.email, login_request.password)
        if success:
            # 서비스 상태에 현재 사용자 저장
            service_status["current_user"] = login_request.email
            
            # 전략 재정의가 요청되었는지 확인
            strategy = login_request.override_strategy or TradingStrategy.ENVELOPE
            
            # 서비스 상태에 전략 저장
            service_status["active_strategy"] = strategy
            
            return {
                "success": True,
                "message": "로그인 성공",
                "assigned_strategy": strategy
            }
        else:
            raise HTTPException(
                status_code=401,
                detail="로그인 실패: 이메일 또는 비밀번호가 일치하지 않습니다."
            )
    except Exception as e:
        logger.error(f"로그인 중 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail=f"로그인 중 오류: {str(e)}"
        )

@app.get("/auth/status")
async def get_auth_status():
    """현재 인증 상태 확인 (백엔드 서버 인증)"""
    global auth_client
    
    if not auth_client:
        return {
            "is_authenticated": False,
            "message": "인증 클라이언트가 초기화되지 않았습니다."
        }
    
    return {
        "is_authenticated": auth_client.is_authenticated,
        "access_token_expires_at": auth_client.access_token_expires_at.isoformat() if auth_client.access_token_expires_at else None,
        "current_user": service_status["current_user"]
    }

@app.post("/auth/refresh")
async def refresh_token():
    """백엔드 서버 액세스 토큰 갱신"""
    global auth_client
    
    if not auth_client:
        raise HTTPException(
            status_code=401,
            detail="인증 클라이언트가 초기화되지 않았습니다."
        )
    
    if not auth_client.refresh_token:
        raise HTTPException(
            status_code=401,
            detail="리프레시 토큰이 없습니다. 다시 로그인해주세요."
        )
    
    try:
        success = await auth_client.refresh_access_token()
        
        if success:
            return {
                "success": True,
                "message": "토큰 갱신 성공",
                "expires_at": auth_client.access_token_expires_at.isoformat() if auth_client.access_token_expires_at else None
            }
        else:
            raise HTTPException(
                status_code=401,
                detail="토큰 갱신 실패"
            )
    except Exception as e:
        logger.error(f"토큰 갱신 중 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail=f"토큰 갱신 중 오류: {str(e)}"
        )

@app.post("/bots/create", response_model=BotStatusResponse)
async def create_bot(request: BotCreateRequest):
    """새로운 봇 생성 및 로그인"""
    global bot_manager, kiwoom_api, backend_client
    
    if not bot_manager:
        raise HTTPException(
            status_code=500,
            detail="봇 관리자가 초기화되지 않았습니다."
        )
    
    # 이미 존재하는 봇인지 확인
    existing_bot = bot_manager.get_bot(request.email)
    if existing_bot:
        # 이미 존재하는 봇이 다른 전략을 사용하는 경우 실패
        if existing_bot.strategy != request.strategy:
            raise HTTPException(
                status_code=400,
                detail=f"이미 다른 전략({existing_bot.strategy})으로 생성된 봇입니다."
            )
        
        # 기존 봇 상태 반환
        return existing_bot.get_status()
    
    # 새 봇 생성
    bot = await bot_manager.create_bot(request.email, request.password, request.strategy)
    
    if not bot:
        raise HTTPException(
            status_code=500,
            detail="봇 생성 및 로그인 실패"
        )
    
    # 봇에 트레이딩 모델 할당 (기존 API 인스턴스 사용)
    if kiwoom_api and backend_client:
        if request.strategy == TradingStrategy.ENVELOPE:
            trading_model = EnvelopeTradingModel(kiwoom_api)
        else:
            trading_model = BollingerBandTradingModel(kiwoom_api)
        
        trading_model.set_backend_client(backend_client)
        await trading_model.start()
        
        # 봇에 트레이딩 모델 설정
        bot.trading_model = trading_model
    
    return bot.get_status()

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("app.main:app", host="0.0.0.0", port=8000, reload=True)