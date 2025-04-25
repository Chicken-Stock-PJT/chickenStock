import asyncio
import logging
from typing import Dict
from datetime import datetime, timedelta
from typing import List
from pydantic import BaseModel

from fastapi import FastAPI, HTTPException, BackgroundTasks, Depends
from fastapi.middleware.cors import CORSMiddleware
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials

from app.config import settings
from app.kiwoom_api import KiwoomAPI
from app.ai_trading import TradingModel
from app.backend_client import BackendClient

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
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# 보안 스키마 설정
security = HTTPBearer()

# 글로벌 변수
kiwoom_api = None
trading_model = None
backend_client = None

# 토큰 관리
token_info = {
    "access_token": None,
    "expires_at": None
}

# 서비스 상태 추적
service_status = {
    "is_running": False,
    "start_time": None,
}

# 최근 거래 정보를 위한 모델 정의
class TradeInfo(BaseModel):
    symbol: str
    name: str
    type: str  # "buy" 또는 "sell"
    price: float
    quantity: int
    amount: float
    date_time: datetime
    status: str

# 최근 거래 내역 저장 (메모리에 임시 저장)
recent_trades = []

@app.on_event("startup")
async def startup_event():
    """애플리케이션 시작 시 실행"""
    logger.info("애플리케이션 시작 중...")

@app.on_event("shutdown")
async def shutdown_event():
    """애플리케이션 종료 시 실행"""
    logger.info("애플리케이션 종료 중...")
    if service_status["is_running"]:
        await stop_trading_service()

async def get_valid_token():
    """유효한 접근 토큰 반환 (필요시 갱신)"""
    global token_info
    
    current_time = datetime.now()
    if not token_info["access_token"] or (token_info["expires_at"] and current_time >= token_info["expires_at"]):
        logger.info("접근 토큰 발급 요청")
        try:
            # 임시 세션 생성
            import aiohttp
            async with aiohttp.ClientSession() as session:
                auth_data = {
                    "grant_type": "client_credentials",
                    "appkey": settings.KIWOOM_API_KEY,
                    "secretkey": settings.KIWOOM_API_SECRET
                }
                
                async with session.post(f"{settings.API_BASE_URL}/oauth2/token", json=auth_data) as response:
                    result = await response.json()
                    logger.info(f"토큰 발급 응답: {result}")
                    
                    if response.status == 200 and result.get("return_code") == 0:
                        token_info["access_token"] = result.get("token")
                        logger.info("접근 토큰 갱신 성공")
                        return token_info["access_token"]
                    else:
                        logger.error(f"토큰 발급 실패: {result}")
                        return None
        
        except Exception as e:
            logger.error(f"토큰 발급 중 오류: {str(e)}")
            return None
    
    return token_info["access_token"]

async def verify_token(credentials: HTTPAuthorizationCredentials = Depends(security)):
    """토큰 검증 의존성"""
    if not credentials or not credentials.credentials:
        raise HTTPException(
            status_code=401,
            detail="유효한 인증 정보가 없습니다"
        )
    return credentials.credentials

@app.get("/", response_model=Dict[str, str])
async def root():
    """API 루트 엔드포인트"""
    return {"message": "주식 자동매매 봇 API에 오신 것을 환영합니다!"}

@app.get("/status", dependencies=[Depends(verify_token)])
async def get_status():
    """현재 서비스 상태 조회"""
    return {
        "status": "running" if service_status["is_running"] else "stopped",
        "is_running": service_status["is_running"],
        "start_time": service_status["start_time"].isoformat() if service_status["start_time"] else None,
        "connected_to_api": kiwoom_api.connected if kiwoom_api else False,
        "subscribed_symbols": len(kiwoom_api.subscribed_symbols) if kiwoom_api else 0,
        "token_expires_at": token_info["expires_at"].isoformat() if token_info["expires_at"] else None,
        "account_info": {
            "cash_balance": kiwoom_api.account_info["cash_balance"] if kiwoom_api else 0,
            "positions_count": len(kiwoom_api.account_info["positions"]) if kiwoom_api else 0,
            "total_asset_value": kiwoom_api.account_info["total_asset_value"] if kiwoom_api else 0
        } if kiwoom_api else None
    }

@app.post("/service/start", dependencies=[Depends(verify_token)])
async def start_trading_service(background_tasks: BackgroundTasks):
    """자동매매 서비스 시작"""
    global kiwoom_api, trading_model, backend_client, service_status

    if service_status["is_running"]:
        return {"message": "서비스가 이미 실행 중입니다."}

    try:
        # 접근 토큰 발급
        access_token = await get_valid_token()
        if not access_token:
            raise HTTPException(
                status_code=500,
                detail="API 접근 토큰 발급 실패"
            )
        
        # 키움 API 인스턴스 생성 (토큰 전달)
        kiwoom_api = KiwoomAPI()
        kiwoom_api.access_token = access_token
        
        # 트레이딩 모델 생성
        trading_model = TradingModel(kiwoom_api)
        
        # 백엔드 클라이언트 생성 (매매 결과 전송용)
        backend_client = BackendClient(trading_model)
        
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

# 거래 실행 후 결과를 recent_trades에 추가하는 함수
async def add_trade_to_history(decision, result):
    """거래 결과를 거래 내역에 추가"""
    global recent_trades
    
    trade_info = TradeInfo(
        symbol=decision["symbol"],
        name=decision["name"] if "name" in decision else "",
        type=decision["action"],  # "buy" 또는 "sell"
        price=result["executed_price"],
        quantity=result["executed_quantity"],
        amount=result["executed_price"] * result["executed_quantity"],
        date_time=datetime.now(),
        status="완료" if result["success"] else "실패"
    )
    
    # 최대 50개까지만 저장
    recent_trades.append(trade_info.dict())
    if len(recent_trades) > 50:
        recent_trades = recent_trades[-50:]

async def initialize_service():
    """자동매매 서비스 초기화 및 시작"""
    global kiwoom_api, trading_model, backend_client, service_status
    
    try:
        # REST API 연결 및 로그인 (토큰 이미 설정됨)
        logger.info("REST API 연결 시작")
        if not await kiwoom_api.connect():
            logger.error("REST API 연결 실패")
            service_status["is_running"] = False
            return
        
        # 토큰 갱신 태스크 시작
        asyncio.create_task(token_refresh_task())
        
        # 계좌 정보 요청
        logger.info("계좌 정보 요청")
        if not await kiwoom_api.request_account_info():
            logger.error("계좌 정보 요청 실패")
            service_status["is_running"] = False
            return
        
        # 트레이딩 모델 초기화
        logger.info("트레이딩 모델 초기화")
        await trading_model.start()
        
        # 종목 리스트 가져오기
        symbols = kiwoom_api.get_all_symbols()
        
        # 트레이딩 모델에 차트 데이터 초기화
        logger.info("차트 데이터 초기화")
        await trading_model.initialize_chart_data(symbols)
        
        # 실시간 데이터 구독
        logger.info("실시간 데이터 구독 시작")
        # 배치 단위로 구독
        batch_size = 100
        for i in range(0, len(symbols), batch_size):
            batch_symbols = symbols[i:i+batch_size]
            await kiwoom_api.subscribe_realtime_data(batch_symbols, trading_model.handle_realtime_price)
            await asyncio.sleep(1)  # API 부하 방지
        
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

async def token_refresh_task():
    """토큰 자동 갱신 태스크"""
    while service_status["is_running"]:
        try:
            # 토큰 만료 10분 전에 갱신
            if token_info["expires_at"]:
                now = datetime.now()
                time_to_expiry = (token_info["expires_at"] - now).total_seconds()
                
                if time_to_expiry <= 600:  # 10분(600초) 이하로 남은 경우
                    logger.info("토큰 만료 시간이 10분 이내로 남아 갱신 시작")
                    await get_valid_token()
                else:
                    # 다음 갱신 시간까지 대기 (토큰 만료 10분 전)
                    wait_time = time_to_expiry - 600
                    await asyncio.sleep(min(wait_time, 300))  # 최대 5분씩 대기
            else:
                await asyncio.sleep(300)  # 5분 대기
        except Exception as e:
            logger.error(f"토큰 갱신 태스크 오류: {str(e)}")
            await asyncio.sleep(60)  # 오류 발생 시 1분 후 재시도

async def trading_loop():
    """주기적인 매매 처리 루프"""
    global kiwoom_api, trading_model, backend_client, service_status
    
    logger.info("거래 처리 루프 시작")
    
    while service_status["is_running"]:
        try:
            # 토큰 유효성 확인
            token = await get_valid_token()
            if not token:
                logger.error("유효한 접근 토큰이 없어 거래를 건너뜁니다")
                await asyncio.sleep(30)
                continue
                
            # 계좌 정보 업데이트 (1분 간격)
            await kiwoom_api.request_account_info()
            
            # 매매 결정 처리
            decisions = await trading_model.get_trade_decisions()
            
            # 매매 결정이 있으면 실행
            for decision in decisions:
                try:
                    # 거래 실행 (비동기로 변경)
                    result = await kiwoom_api.execute_trade_decision(decision)
                    if result:
                        # 거래 내역에 추가
                        await add_trade_to_history(decision, result)
                        # 백엔드로 결과 전송
                        if backend_client:
                            await backend_client.send_trade_result(decision)
                                        
                except Exception as e:
                    logger.error(f"거래 실행 중 오류: {str(e)}")
            
            # 다음 사이클까지 대기 (60초)
            await asyncio.sleep(60)
            
        except Exception as e:
            logger.error(f"거래 처리 루프 오류: {str(e)}")
            await asyncio.sleep(30)  # 오류 시 30초 후 재시도

@app.post("/service/stop", dependencies=[Depends(verify_token)])
async def stop_trading_service():
    """자동매매 서비스 중지"""
    global kiwoom_api, trading_model, backend_client, service_status
    
    if not service_status["is_running"]:
        return {"message": "서비스가 이미 중지되었습니다."}
    
    try:
        # 실시간 데이터 구독 해제
        if kiwoom_api:
            await kiwoom_api.unsubscribe_realtime_data()
            await kiwoom_api.close()  # API 연결 종료
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

@app.get("/account", dependencies=[Depends(verify_token)])
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

@app.get("/symbols", dependencies=[Depends(verify_token)])
async def get_symbols():
    """관심 종목 목록 조회"""
    if not service_status["is_running"] or not kiwoom_api:
        raise HTTPException(
            status_code=400,
            detail="서비스가 실행 중이지 않습니다."
        )
    
    return {
        "kospi": kiwoom_api.kospi_symbols,
        "kosdaq": kiwoom_api.kosdaq_symbols,
        "total": len(kiwoom_api.kospi_symbols) + len(kiwoom_api.kosdaq_symbols)
    }

@app.get("/prices", dependencies=[Depends(verify_token)])
async def get_realtime_prices():
    """실시간 가격 정보 조회"""
    if not service_status["is_running"] or not kiwoom_api:
        raise HTTPException(
            status_code=400,
            detail="서비스가 실행 중이지 않습니다."
        )
    
    return kiwoom_api.realtime_prices

@app.get("/trades", response_model=List[dict], dependencies=[Depends(verify_token)])
async def get_trades():
    """최근 거래 내역 조회"""
    if not service_status["is_running"]:
        raise HTTPException(
            status_code=400,
            detail="서비스가 실행 중이지 않습니다."
        )
    
    return recent_trades

@app.post("/auth/token")
async def get_access_token():
    """접근 토큰 발급 API"""
    try:
        access_token = await get_valid_token()
        logger.info(access_token)
        if not access_token:
            raise HTTPException(
                status_code=500,
                detail="토큰 발급 실패"
            )
        
        return {
            "access_token": access_token,
            "expires_at": token_info["expires_at"].isoformat() if token_info["expires_at"] else None,
            "token_type": "bearer"
        }
    except Exception as e:
        logger.error(f"토큰 발급 API 오류: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail=f"토큰 발급 실패: {str(e)}"
        )

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("app.main:app", host="0.0.0.0", port=8000, reload=False)