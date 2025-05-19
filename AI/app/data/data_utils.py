"""
DRL-UTrans 모델을 위한 간소화된 데이터 수집 및 처리 유틸리티
- 주식 차트 데이터만 저장 (date, open, high, low, close, volume)
- CSV 데이터 준비 (GPU 서버용)
"""
import os
import logging
import pandas as pd
import numpy as np
import json
from datetime import datetime
from typing import Dict, List, Optional, Any

# 로깅 설정
logger = logging.getLogger(__name__)

# 기본 데이터 디렉토리 설정
DATA_DIR = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "data")
os.makedirs(DATA_DIR, exist_ok=True)

def convert_chart_data_to_dataframe(chart_data: List[Dict]) -> pd.DataFrame:
    """
    차트 데이터를 기본 DataFrame으로 변환 (필수 컬럼만 유지)
    
    Args:
        chart_data: API에서 받아온 차트 데이터 리스트
        
    Returns:
        기본 컬럼으로 구성된 데이터프레임
    """
    try:
        if not chart_data or len(chart_data) < 5:
            logger.warning("충분한 차트 데이터가 없습니다.")
            return pd.DataFrame()
        
        # 차트 데이터 형식 확인 및 변환
        chart_list = []
        for item in chart_data:
            if isinstance(item, dict):
                # 딕셔너리 형식인 경우
                if all(k in item for k in ['date', 'open', 'high', 'low', 'close', 'volume']):
                    chart_list.append(item)
                else:
                    # 필요한 키가 없는 경우 건너뜀
                    logger.debug("필요한 키가 없는 항목 건너뜀")
                    continue
            elif isinstance(item, list) and len(item) >= 6:
                # 리스트 형식인 경우 (인덱스 기반 접근)
                chart_list.append({
                    'date': item[0],
                    'open': item[1],
                    'high': item[2],
                    'low': item[3],
                    'close': item[4],
                    'volume': item[5]
                })
        
        # 데이터프레임 생성
        df = pd.DataFrame(chart_list)
        
        # 필요한 컬럼 확인
        required_columns = ['date', 'open', 'high', 'low', 'close', 'volume']
        missing_columns = [col for col in required_columns if col not in df.columns]
        
        if missing_columns:
            logger.warning(f"차트 데이터에 필요한 컬럼이 없습니다: {missing_columns}")
            return pd.DataFrame()
        
        # 날짜 형식 변환 (필요시)
        if 'date' in df.columns:
            try:
                # 다양한 날짜 형식 처리
                if isinstance(df['date'].iloc[0], str) and len(df['date'].iloc[0]) == 8:
                    # YYYYMMDD 형식
                    df['date'] = pd.to_datetime(df['date'], format='%Y%m%d', errors='coerce')
                else:
                    # 기타 형식
                    df['date'] = pd.to_datetime(df['date'], errors='coerce')
            except Exception as e:
                logger.warning(f"날짜 변환 중 오류: {str(e)}")
                df['date'] = pd.to_datetime(df['date'], errors='coerce')
            
        # 데이터 타입 확인 및 변환
        numeric_columns = ['open', 'high', 'low', 'close', 'volume']
        for col in numeric_columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
        
        # NaN 값 처리 (앞/뒤 값 사용)
        df = df = df.ffill().bfill()
        
        # 필요한 컬럼만 선택
        df = df[required_columns]
        
        return df
    
    except Exception as e:
        logger.error(f"차트 데이터 변환 중 오류: {str(e)}")
        return pd.DataFrame()

def save_chart_data_to_csv(symbol: str, chart_data: List[Any], output_dir=None) -> Optional[str]:
    """
    차트 데이터를 CSV 파일로 저장 (필수 컬럼만)
    
    Args:
        symbol: 종목 코드
        chart_data: 차트 데이터 리스트 (StockCache 형식)
        output_dir: 출력 디렉토리 (기본값: DATA_DIR/chart_csv)
        
    Returns:
        저장된 CSV 파일 경로 또는 오류 시 None
    """
    try:
        if not chart_data:
            logger.warning(f"종목 {symbol}의 저장할 차트 데이터가 없습니다.")
            return None
        
        # 출력 디렉토리 설정
        if output_dir is None:
            output_dir = os.path.join(DATA_DIR, "chart_csv")
        
        # 디렉토리 생성
        os.makedirs(output_dir, exist_ok=True)
        
        # 차트 데이터를 DataFrame으로 변환
        df = convert_chart_data_to_dataframe(chart_data)
        
        if df.empty:
            logger.warning(f"종목 {symbol}의 변환된 차트 데이터가 비었습니다.")
            return None
        
        # 파일명 생성
        timestamp = datetime.now().strftime("%Y%m%d")
        csv_file = os.path.join(output_dir, f"{symbol}_{timestamp}.csv")
        
        # CSV 파일로 저장
        df.to_csv(csv_file, index=False)
        logger.info(f"종목 {symbol}의 차트 데이터 저장 완료: {csv_file}")
        
        return csv_file
    
    except Exception as e:
        logger.error(f"종목 {symbol}의 차트 데이터 저장 중 오류: {str(e)}")
        return None

def save_all_chart_data(stock_cache, output_dir=None):
    """
    모든 종목의 차트 데이터를 CSV로 저장
    
    Args:
        stock_cache: 종목 데이터가 저장된 StockCache 객체
        output_dir: 출력 디렉토리 (기본값: DATA_DIR/chart_csv)
        
    Returns:
        Dict: 저장된 파일 정보
    """
    if output_dir is None:
        output_dir = os.path.join(DATA_DIR, "chart_csv")
    
    # 디렉토리 생성
    os.makedirs(output_dir, exist_ok=True)
    
    # 결과 저장 딕셔너리
    result = {
        "timestamp": datetime.now().strftime("%Y%m%d_%H%M%S"),
        "success_count": 0,
        "failed_count": 0,
        "files": []
    }
    
    try:
        # 필터링된 종목 목록 가져오기
        symbols = stock_cache.filtered_stockcode_list if hasattr(stock_cache, 'filtered_stockcode_list') else []
        
        if not symbols:
            logger.warning("저장할 필터링된 종목이 없습니다.")
            return result
        
        logger.info(f"총 {len(symbols)}개 종목의 차트 데이터 저장 시작")
        
        # 각 종목별로 차트 데이터 저장
        for symbol in symbols:
            try:
                # 차트 데이터 가져오기
                chart_data = stock_cache.get_chart_data(symbol)
                
                if not chart_data or len(chart_data) < 5:
                    logger.warning(f"종목 {symbol}의 차트 데이터가 부족합니다.")
                    result["failed_count"] += 1
                    continue
                
                # CSV 파일로 저장
                csv_file = save_chart_data_to_csv(symbol, chart_data, output_dir)
                
                if csv_file:
                    result["success_count"] += 1
                    result["files"].append({
                        "symbol": symbol,
                        "file_path": csv_file
                    })
                else:
                    result["failed_count"] += 1
            
            except Exception as e:
                logger.error(f"종목 {symbol} 처리 중 오류: {str(e)}")
                result["failed_count"] += 1
        
        # 메타데이터 저장
        metadata = {
            "timestamp": result["timestamp"],
            "total_count": len(symbols),
            "success_count": result["success_count"],
            "failed_count": result["failed_count"],
            "symbols": [item["symbol"] for item in result["files"]]
        }
        
        metadata_file = os.path.join(output_dir, f"metadata_{result['timestamp']}.json")
        with open(metadata_file, 'w', encoding='utf-8') as f:
            json.dump(metadata, f, ensure_ascii=False, indent=2)
        
        logger.info(f"차트 데이터 저장 완료: 성공 {result['success_count']}개, 실패 {result['failed_count']}개")
        return result
    
    except Exception as e:
        logger.error(f"차트 데이터 일괄 저장 중 오류: {str(e)}")
        return result

def prepare_dataset_for_upload(output_dir=None, compress=True):
    """
    GPU 서버 업로드용 데이터셋 준비
    
    Args:
        output_dir: 출력 디렉토리 (기본값: DATA_DIR/upload)
        compress: 압축 여부
        
    Returns:
        str: 압축 파일 경로 또는 디렉토리 경로
    """
    import shutil
    
    if output_dir is None:
        output_dir = os.path.join(DATA_DIR, "upload")
    
    # 디렉토리 생성
    os.makedirs(output_dir, exist_ok=True)
    
    # 차트 데이터 디렉토리
    chart_dir = os.path.join(DATA_DIR, "chart_csv")
    
    if not os.path.exists(chart_dir):
        logger.error(f"차트 데이터 디렉토리가 존재하지 않습니다: {chart_dir}")
        return None
    
    try:
        # 타임스탬프
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # 압축 파일 경로
        zip_file = os.path.join(output_dir, f"chart_data_{timestamp}.zip")
        
        # 데이터 압축
        if compress:
            shutil.make_archive(
                os.path.splitext(zip_file)[0],  # 확장자 제외한 기본 이름
                'zip',  # 압축 형식
                chart_dir  # 압축할 디렉토리
            )
            logger.info(f"차트 데이터 압축 완료: {zip_file}")
            return zip_file
        else:
            # 압축하지 않고 디렉토리 복사
            dest_dir = os.path.join(output_dir, f"chart_data_{timestamp}")
            shutil.copytree(chart_dir, dest_dir)
            logger.info(f"차트 데이터 복사 완료: {dest_dir}")
            return dest_dir
    
    except Exception as e:
        logger.error(f"데이터셋 준비 중 오류: {str(e)}")
        return None