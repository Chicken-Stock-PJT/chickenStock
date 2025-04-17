from fastapi import FastAPI
import asyncio
import logging
import uvicorn
from app.backend_client import BackendClient
from app.config import settings

# 로깅 설정
logging.basicConfig(
    level=getattr(logging, settings.LOG_LEVEL),
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

app = FastAPI(title="AI Trading Server")
backend_client = None

@app.on_event("startup")
async def startup_event():
    """앱 시작 시 백엔드 연결 및 자동 거래 시작"""
    global backend_client
    logger.info("Starting AI Trading Server...")
    
    # 백엔드 클라이언트 초기화 및 연결
    backend_client = BackendClient()
    is_connected = await backend_client.connect()
    
    if is_connected:
        # 자동 종목 스크리닝 및 거래 시작
        asyncio.create_task(backend_client.automated_trading_loop())
        logger.info("Automated trading loop started")
    else:
        logger.error("Failed to connect to backend server. Automated trading not started.")

@app.get("/health")
async def health_check():
    """서버 상태 확인 API"""
    is_connected = backend_client.connected if backend_client else False
    return {
        "status": "healthy", 
        "backend_connected": is_connected,
        "active_symbols": backend_client.get_active_symbols() if backend_client else []
    }

@app.get("/trading/performance")
async def get_trading_performance():
    """현재 거래 성과 조회"""
    if not backend_client:
        return {"error": "Backend client not initialized"}
    
    return {
        "performance": backend_client.trading_model.get_performance(),
        "active_symbols": backend_client.get_active_symbols()
    }

if __name__ == "__main__":
    uvicorn.run("app.main:app", host="0.0.0.0", port=8000, reload=True)