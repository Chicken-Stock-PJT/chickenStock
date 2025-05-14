# app/monitoring/decorators.py
import functools
import time
import logging
from typing import Optional, Callable

from app.monitoring.metrics import api_requests, api_request_duration, api_errors, api_request_in_progress

logger = logging.getLogger(__name__)

def track_api_call(api_name: str, endpoint: str = "default"):
    """
    API 호출 추적 데코레이터
    
    :param api_name: API 이름
    :param endpoint: API 엔드포인트
    """
    def decorator(func):
        @functools.wraps(func)
        async def wrapper(*args, **kwargs):
            # 진행 중인 요청 카운트 증가
            api_request_in_progress.labels(api_name=api_name).inc()
            
            start_time = time.time()
            error_type = None
            
            try:
                # 함수 실행
                result = await func(*args, **kwargs)
                return result
            except Exception as e:
                # 오류 유형 기록
                error_type = type(e).__name__
                raise
            finally:
                # 요청 소요 시간 계산
                duration = time.time() - start_time
                
                # 요청 카운트 증가
                api_requests.labels(api_name=api_name, endpoint=endpoint).inc()
                
                # 요청 소요 시간 기록
                api_request_duration.labels(api_name=api_name).observe(duration)
                
                # 오류가 발생한 경우 오류 카운트 증가
                if error_type:
                    api_errors.labels(api_name=api_name, error_type=error_type).inc()
                    logger.warning(f"API 호출 오류: {api_name}.{endpoint}, 오류 유형: {error_type}, 소요 시간: {duration:.3f}초")
                else:
                    logger.debug(f"API 호출 성공: {api_name}.{endpoint}, 소요 시간: {duration:.3f}초")
                
                # 진행 중인 요청 카운트 감소
                api_request_in_progress.labels(api_name=api_name).dec()
        
        return wrapper
    
    return decorator