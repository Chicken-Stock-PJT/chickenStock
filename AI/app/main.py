import asyncio
import logging
import sys
from typing import Dict
from datetime import datetime

from fastapi import FastAPI, HTTPException, BackgroundTasks
from fastapi.middleware.cors import CORSMiddleware
from PyQt5.QtWidgets import QApplication

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
    description="Envelope 전략 기반 한국 주식 자동매매 봇 API - 키움증권 OPEN API 연동",
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

# 글로벌 변수
kiwoom_api = None
trading_model = None
backend_client = None
qt_app = None

# 서비스 상태 추적
service_status = {
    "is_running": False,
    "start_time": None,
}

@app.on_event("startup")
async def startup_event():
    """애플리케이션 시작 시 실행"""
    global qt_app
    logger.info("애플리케이션 시작 중...")
    
    # PyQt 애플리케이션 생성
    qt_app = QApplication(sys.argv)

@app.on_event("shutdown")
async def shutdown_event():
    """애플리케이션 종료 시 실행"""
    logger.info("애플리케이션 종료 중...")
    if service_status["is_running"]:
        await stop_trading_service()

@app.get("/", response_model=Dict[str, str])
async def root():
    """API 루트 엔드포인트"""
    return {"message": "주식 자동매매 봇 API에 오신 것을 환영합니다!"}

@app.get("/status")
async def get_status():
    """현재 서비스 상태 조회"""
    return {
        "status": "running" if service_status["is_running"] else "stopped",
        "is_running": service_status["is_running"],
        "start_time": service_status["start_time"].isoformat() if service_status["start_time"] else None,
        "connected_to_kiwoom": kiwoom_api.connected if kiwoom_api else False,
        "subscribed_symbols": len(kiwoom_api.subscribed_symbols) if kiwoom_api else 0,
        "account_info": {
            "cash_balance": kiwoom_api.account_info["cash_balance"] if kiwoom_api else 0,
            "positions_count": len(kiwoom_api.account_info["positions"]) if kiwoom_api else 0,
            "total_asset_value": kiwoom_api.account_info["total_asset_value"] if kiwoom_api else 0
        } if kiwoom_api else None
    }

@app.post("/service/start")
async def start_trading_service(background_tasks: BackgroundTasks):
    """자동매매 서비스 시작"""
    global kiwoom_api, trading_model, backend_client, service_status

    if service_status["is_running"]:
        return {"message": "서비스가 이미 실행 중입니다."}

    try:
        # 키움 API 인스턴스 생성
        kiwoom_api = KiwoomAPI()
        
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

async def initialize_service():
    """자동매매 서비스 초기화 및 시작"""
    global kiwoom_api, trading_model, backend_client, service_status
    
    try:
        # 키움 API 연결 및 로그인
        logger.info("키움 API 연결 시작")
        if not await kiwoom_api.connect():
            logger.error("키움 API 연결 실패")
            service_status["is_running"] = False
            return
        
        # 계좌 정보 요청 (백엔드에서)
        logger.info("백엔드에서 계좌 정보 요청")
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
            kiwoom_api.subscribe_realtime_data(batch_symbols, trading_model.handle_realtime_price)
            await asyncio.sleep(1)  # API 부하 방지
        
        # 거래 처리 루프 시작
        asyncio.create_task(trading_loop())
        
        logger.info("자동매매 서비스 초기화 완료")
    
    except Exception as e:
        logger.error(f"서비스 초기화 중 오류 발생: {str(e)}")
        service_status["is_running"] = False
        if kiwoom_api:
            kiwoom_api = None
        if trading_model:
            trading_model = None
        if backend_client:
            backend_client = None

async def trading_loop():
    """주기적인 매매 처리 루프"""
    global kiwoom_api, trading_model, backend_client, service_status
    
    logger.info("거래 처리 루프 시작")
    
    while service_status["is_running"]:
        try:
            # 계좌 정보 업데이트 (1분 간격) - 백엔드에서 가져옴
            await kiwoom_api.request_account_info()
            
            # 매매 결정 처리
            decisions = await trading_model.get_trade_decisions()
            
            # 매매 결정이 있으면 실행
            for decision in decisions:
                try:
                    # 거래 실행
                    result = kiwoom_api.execute_trade_decision(decision)
                    
                    # 거래 결과 백엔드로 전송
                    if result and backend_client:
                        await backend_client.send_trade_result(decision)
                    
                except Exception as e:
                    logger.error(f"거래 실행 중 오류: {str(e)}")
            
            # 다음 사이클까지 대기 (60초)
            await asyncio.sleep(60)
            
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
        # 실시간 데이터 구독 해제
        if kiwoom_api:
            kiwoom_api.unsubscribe_realtime_data()
        
        # 트레이딩 모델 종료
        if trading_model:
            await trading_model.stop()
            trading_model = None
        
        # 백엔드 클라이언트 종료
        if backend_client:
            await backend_client.stop()
            backend_client = None
        
        # 키움 API 정리
        kiwoom_api = None
        
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
    
    # 최신 계좌 정보 요청 (백엔드에서)
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
        "kospi": kiwoom_api.kospi_symbols,
        "kosdaq": kiwoom_api.kosdaq_symbols,
        "total": len(kiwoom_api.kospi_symbols) + len(kiwoom_api.kosdaq_symbols)
    }

@app.get("/prices")
async def get_realtime_prices():
    """실시간 가격 정보 조회"""
    if not service_status["is_running"] or not kiwoom_api:
        raise HTTPException(
            status_code=400,
            detail="서비스가 실행 중이지 않습니다."
        )
    
    return kiwoom_api.realtime_prices

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("app.main:app", host="0.0.0.0", port=8000, reload=False)