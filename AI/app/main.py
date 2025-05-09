import asyncio
import logging
import aiohttp
from typing import Dict, List, Optional, Any, Callable
from enum import Enum
from datetime import datetime, timedelta, time
import numpy as np

from fastapi import FastAPI, HTTPException, BackgroundTasks, Depends, Query, Body
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

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

class TradingStrategy(str, Enum):
    """트레이딩 전략 유형"""
    ENVELOPE = "envelope"
    BOLLINGER = "bollinger"