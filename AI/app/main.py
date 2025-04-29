import asyncio
import aiohttp
import logging
from typing import Dict, List
from datetime import datetime
from pydantic import BaseModel

from fastapi import FastAPI, HTTPException, BackgroundTasks, Depends
from fastapi.middleware.cors import CORSMiddleware

from app.config import settings
from app.kiwoom_api import KiwoomAPI
from app.auth_client import AuthClient
from app.ai_trading import TradingModel
from app.backend_client import BackendClient
from app.kiwoom_auth import KiwoomAuthClient

# 로깅 설정
logging.basicConfig(
    level=getattr(logging, settings.LOG_LEVEL),
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("app")

# FastAPI 애플리케이션 초기화
app = FastAPI(
    title="주식 자동매매 봇 API",
    description="Envelope 전략 기반 한국 주식 자동매매 봇 API - REST API 연동",
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

# 글로벌 변수
auth_client = None
kiwoom_api = None
trading_model = None
backend_client = None

# 서비스 상태 추적
service_status = {
    "is_running": False,
    "start_time": None,
}

# 로그인 요청 모델
class LoginRequest(BaseModel):
    email: str
    password: str
    platform: str = "mobile"

@app.on_event("startup")
async def startup_event():
    """애플리케이션 시작 시 실행"""
    global auth_client
    
    logger.info("애플리케이션 시작 중...")
    
    # 인증 클라이언트 초기화
    auth_client = AuthClient()
    await auth_client.initialize()

@app.on_event("shutdown")
async def shutdown_event():
    """애플리케이션 종료 시 실행"""
    logger.info("애플리케이션 종료 중...")
    
    if service_status["is_running"]:
        await stop_trading_service()
    
    # 인증 클라이언트 종료
    if auth_client:
        await auth_client.close()

@app.post("/auth/login")
async def login(login_request: LoginRequest):
    """백엔드 API 로그인"""
    global auth_client
    
    if not auth_client:
        auth_client = AuthClient()
        await auth_client.initialize()
    
    try:
        success = await auth_client.login(login_request.email, login_request.password)
        
        if success:
            return {
                "success": True,
                "message": "로그인 성공",
                "expires_at": auth_client.access_token_expires_at.isoformat() if auth_client.access_token_expires_at else None
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
    """현재 인증 상태 확인"""
    global auth_client
    
    if not auth_client:
        return {
            "is_authenticated": False,
            "message": "인증 클라이언트가 초기화되지 않았습니다."
        }
    
    return {
        "is_authenticated": auth_client.is_authenticated,
        "access_token_expires_at": auth_client.access_token_expires_at.isoformat() if auth_client.access_token_expires_at else None
    }

@app.post("/auth/refresh")
async def refresh_token():
    """액세스 토큰 갱신"""
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

@app.get("/", response_model=Dict[str, str])
async def root():
    """API 루트 엔드포인트"""
    return {"message": "주식 자동매매 봇 API에 오신 것을 환영합니다!"}

@app.get("/auth/token-status")
async def get_kiwoom_token_status():
    """키움 API 토큰 상태 조회"""
    try:
        # KiwoomAuthClient 인스턴스 생성
        kiwoom_auth_client = KiwoomAuthClient()
        
        # 토큰 상태 확인
        token = await kiwoom_auth_client.get_access_token()
        
        return {
            "has_token": token is not None,
            "is_valid": token is not None,
            "is_expired": token is None,
            "expires_at": None  # 현재 구현에서는 만료 시간 정보가 없음
        }
    except Exception as e:
        logger.error(f"키움 토큰 상태 조회 중 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail=f"키움 토큰 상태 조회 중 오류: {str(e)}"
        )

@app.post("/auth/request-token")
async def request_kiwoom_token():
    """키움 API 토큰 발급 요청"""
    try:
        # KiwoomAuthClient 인스턴스 생성
        kiwoom_auth_client = KiwoomAuthClient()
        
        # 토큰 발급 요청
        token = await kiwoom_auth_client.get_access_token()
        
        if token:
            return {
                "success": True,
                "message": "키움 API 토큰이 성공적으로 발급되었습니다.",
                "token_info": {
                    "token": token
                }
            }
        else:
            raise HTTPException(
                status_code=500,
                detail="키움 API 토큰 발급에 실패했습니다."
            )
    except Exception as e:
        logger.error(f"키움 토큰 요청 중 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail=f"키움 토큰 요청 중 오류: {str(e)}"
        )

@app.get("/status")
async def get_status():
    """현재 서비스 상태 조회"""
    global auth_client, kiwoom_api
    
    auth_status = {
        "is_authenticated": False,
        "access_token_expires_at": None
    }
    
    if auth_client:
        auth_status = {
            "is_authenticated": auth_client.is_authenticated,
            "access_token_expires_at": auth_client.access_token_expires_at.isoformat() if auth_client.access_token_expires_at else None
        }
    
    return {
        "status": "running" if service_status["is_running"] else "stopped",
        "is_running": service_status["is_running"],
        "start_time": service_status["start_time"].isoformat() if service_status["start_time"] else None,
        "connected_to_api": kiwoom_api.connected if kiwoom_api else False,
        "auth_status": auth_status,
        "account_info": {
            "cash_balance": kiwoom_api.account_info["cash_balance"] if kiwoom_api else 0,
            "positions_count": len(kiwoom_api.account_info["positions"]) if kiwoom_api else 0,
            "total_asset_value": kiwoom_api.account_info["total_asset_value"] if kiwoom_api else 0
        } if kiwoom_api else None
    }

@app.post("/service/start")
async def start_trading_service(background_tasks: BackgroundTasks):
    """자동매매 서비스 시작"""
    global auth_client, kiwoom_api, trading_model, backend_client, service_status

    if service_status["is_running"]:
        return {"message": "서비스가 이미 실행 중입니다."}
    
    # 인증 상태 확인
    if not auth_client or not auth_client.is_authenticated:
        raise HTTPException(
            status_code=401,
            detail="인증되지 않았습니다. 먼저 로그인이 필요합니다."
        )

    try:
        # 키움 API 인스턴스 생성 (인증 클라이언트 전달)
        kiwoom_api = KiwoomAPI(auth_client)
        
        # 트레이딩 모델 생성
        trading_model = TradingModel(kiwoom_api)
        
        # 백엔드 클라이언트 생성 (매매 결과 전송용)
        backend_client = BackendClient(trading_model, auth_client)
        
        # 백그라운드 태스크로 서비스 초기화
        background_tasks.add_task(initialize_service)
        
        # 상태 업데이트
        service_status["is_running"] = True
        service_status["start_time"] = datetime.now()
        
        return {"message": "자동매매 서비스를 시작합니다. 초기화 작업이 진행 중입니다."}
    
    except Exception as e:
        logger.error(f"서비스 시작 중 오류 발생: {str(e)}")
        if kiwoom_api:
            kiwoom_api = None
        if trading_model:
            trading_model = None
        if backend_client:
            backend_client = None
        service_status["is_running"] = False
        raise HTTPException(
            status_code=500,
            detail=f"서비스 시작 실패: {str(e)}",
        )

async def initialize_service():
    """자동매매 서비스 초기화 및 시작"""
    global auth_client, kiwoom_api, trading_model, backend_client, service_status
    
    try:
        # REST API 연결
        logger.info("REST API 연결 시작")
        if not await kiwoom_api.connect():
            logger.error("REST API 연결 실패")
            service_status["is_running"] = False
            return
        
        # 종목 정보 초기화
        await kiwoom_api.initialize_stock_list()
        
        # 계좌 정보는 connect() 메서드 내에서 이미 요청됨
        
        # 트레이딩 모델 초기화
        logger.info("트레이딩 모델 초기화")
        await trading_model.start()
        
        # 필터링된 종목 리스트 가져오기 (상위 600개)
        filtered_symbols = kiwoom_api.stock_cache.get_filtered_symbols(450, 150)
        
        # 트레이딩 모델에 차트 데이터 초기화 - Envelope 지표 계산용
        logger.info("차트 데이터 초기화")
        await trading_model.initialize_chart_data(filtered_symbols)
        
        # 실시간 데이터 구독 준비
        logger.info("실시간 데이터 구독 그룹 준비")
        await kiwoom_api.prepare_subscription_groups(filtered_symbols, 30)
        
        # 실시간 데이터 구독 로테이션 시작
        logger.info("실시간 데이터 구독 로테이션 시작")
        await kiwoom_api.start_rotating_subscriptions(trading_model.handle_realtime_price)
        
        # 웹소켓 메시지 처리 태스크 시작
        asyncio.create_task(kiwoom_api.handle_websocket_message())
        
        # 백엔드 클라이언트 시작
        await backend_client.start()
        
        # 거래 처리 루프 시작
        asyncio.create_task(trading_loop())
        
        logger.info("자동매매 서비스 초기화 완료")
    
    except Exception as e:
        logger.error(f"서비스 초기화 중 오류 발생: {str(e)}")
        service_status["is_running"] = False
        if kiwoom_api:
            await kiwoom_api.close()
            kiwoom_api = None
        if trading_model:
            trading_model = None
        if backend_client:
            backend_client = None

async def trading_loop():
    """주기적인 매매 처리 루프"""
    global auth_client, kiwoom_api, trading_model, backend_client, service_status
    
    logger.info("거래 처리 루프 시작")
    
    # 마지막 처리 시간 초기화
    last_processing_time = datetime.now()
    
    while service_status["is_running"]:
        try:
            # 인증 상태 확인
            if not auth_client or not auth_client.is_authenticated:
                logger.error("인증되지 않았습니다. 거래를 건너뜁니다.")
                await asyncio.sleep(30)
                continue
            
            # 현재 시간
            current_time = datetime.now()
            
            # 1분마다 계좌 정보 업데이트 및 매매 결정 처리
            if (current_time - last_processing_time).total_seconds() >= 60:
                # 계좌 정보 업데이트
                await kiwoom_api.request_account_info()
                
                # 매매 결정 처리
                decisions = await trading_model.get_trade_decisions()
                
                # 매매 결정이 있으면 실행
                for decision in decisions:
                    try:
                        # 거래 실행
                        result = await kiwoom_api.execute_trade_decision(decision)
                        
                        if result and result["success"]:
                            # 백엔드로 결과 전송
                            if backend_client:
                                await backend_client.send_trade_result(decision, result)
                        else:
                            logger.warning(f"거래 실패: {decision['symbol']} {decision['action']} - {result.get('message', '')}")
                    
                    except Exception as e:
                        logger.error(f"거래 실행 중 오류: {str(e)}")
                
                # 마지막 처리 시간 업데이트
                last_processing_time = current_time
                logger.info(f"거래 처리 완료: {len(decisions)}개 결정 처리됨")
            
            # 1초 대기
            await asyncio.sleep(1)
            
        except Exception as e:
            logger.error(f"거래 처리 루프 오류: {str(e)}")
            await asyncio.sleep(30)  # 오류 시 30초 후 재시도

@app.post("/service/stop")
async def stop_trading_service():
    """자동매매 서비스 중지"""
    global kiwoom_api, trading_model, backend_client, service_status
    
    if not service_status["is_running"]:
        return {"message": "서비스가 이미 중지되었습니다."}
    
    try:
        # API 연결 종료 (구독 해제 포함)
        if kiwoom_api:
            await kiwoom_api.close()
            kiwoom_api = None
        
        # 트레이딩 모델 종료
        if trading_model:
            await trading_model.stop()
            trading_model = None
        
        # 백엔드 클라이언트 종료
        if backend_client:
            await backend_client.stop()
            backend_client = None
        
        # 서비스 상태 업데이트
        service_status["is_running"] = False
        
        return {"message": "자동매매 서비스를 중지했습니다."}
    
    except Exception as e:
        logger.error(f"서비스 중지 중 오류 발생: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail=f"서비스 중지 실패: {str(e)}",
        )

@app.get("/account")
async def get_account_info():
    """계좌 정보 조회"""
    if not service_status["is_running"] or not kiwoom_api:
        raise HTTPException(
            status_code=400,
            detail="서비스가 실행 중이지 않습니다."
        )
    
    # 최신 계좌 정보 요청
    await kiwoom_api.request_account_info()
    
    return kiwoom_api.account_info

@app.get("/symbols")
async def get_symbols():
    """관심 종목 목록 조회"""
    if not service_status["is_running"] or not kiwoom_api:
        raise HTTPException(
            status_code=400,
            detail="서비스가 실행 중이지 않습니다."
        )
    
    return {
        "kospi": kiwoom_api.stock_cache.kospi_symbols,
        "kosdaq": kiwoom_api.stock_cache.kosdaq_symbols,
        "total": len(kiwoom_api.stock_cache.kospi_symbols) + len(kiwoom_api.stock_cache.kosdaq_symbols)
    }

@app.get("/prices")
async def get_realtime_prices():
    """실시간 가격 정보 조회"""
    if not service_status["is_running"] or not kiwoom_api:
        raise HTTPException(
            status_code=400,
            detail="서비스가 실행 중이지 않습니다."
        )
    
    return kiwoom_api.stock_cache.price_cache

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("app.main:app", host="0.0.0.0", port=8000, reload=False)