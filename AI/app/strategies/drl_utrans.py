"""
DRL-UTrans 기반 자동매매 모델 구현
- 기학습된 DRL-UTrans 모델을 로드하여 거래 판단
- 과거 데이터에 기반한 강화학습형 예측 모델
"""
import os
import logging
import asyncio
import numpy as np
import torch
import pandas as pd
import pickle
import sklearn
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional
from sklearn.preprocessing import StandardScaler
from sklearn.exceptions import NotFittedError
import torch.serialization

from app.strategies.base import BaseTradingModel

logger = logging.getLogger(__name__)

class DRLUTransTradingModel(BaseTradingModel):
    """DRL-UTrans 기반 AI 트레이딩 모델"""
    
    def __init__(self, stock_cache=None):
        """DRL-UTrans 전략 트레이딩 모델 초기화 - 매수 신호 감소 설정"""
        super().__init__(stock_cache)
        
        # 매매 관련 설정
        self.max_positions = 7  # 최대 보유 종목 수 (18에서 10으로 감소)
        self.trade_amount_per_stock = 14000000  # 종목당 매매 금액 (1000만원)
        
        # 모델 설정
        self.model_path = os.environ.get('DRL_UTRANS_MODEL_PATH', 
                                    os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 
                                                    "models/drl_utrans"))
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.model = None
        self.scalers = {}  # 종목별 스케일러
        
        # 거래 관련 설정
        self.action_map = {0: "hold", 1: "buy", 2: "sell"}  # DRL-UTrans 모델의 액션 매핑
        self.confidence_threshold = 0.98  # 매매 결정 신뢰도 임계값 (0.65에서 0.98로 대폭 증가)
        self.weight_threshold = 0.65  # 매매 가중치 임계값 (0.4에서 0.7로 증가)
        
        # 매매 신호 저장 딕셔너리
        self.trading_signals = {}  # {symbol: {"action": int, "weight": float, "price": float, "timestamp": datetime}}
        
        # 종목별 예측 결과 저장
        self.predictions = {}  # {symbol: {"action": int, "weight": float, "value": float, "timestamp": datetime}}
        
        # 거래 이력 저장
        self.trade_history = {}  # {symbol: {"last_trade": datetime, "last_action": "buy"/"sell"}}
        
        # 중복 메시지 필터링을 위한 변수
        self.last_processed_prices = {}  # {symbol: price}
        self.last_processed_times = {}   # {symbol: datetime}
        self.min_price_change_pct = 0.1  # 최소 가격 변동 비율 (0.1%)
        self.min_process_interval = 5    # 최소 처리 간격 (초)
        
        # 시퀀스 길이 (모델 입력용)
        self.seq_len = 20  # DRL-UTrans의 기본 시퀀스 길이
        
        # 배치 예측 주기 (시간)
        self.prediction_interval = 4  # 4시간마다 일괄 예측
        
        # 예측 결과 유효 시간
        self.prediction_valid_hours = 24  # 예측 결과 24시간 유효
        
        # 실행 상태
        self.is_running = False
        self.is_model_loaded = False
        
        # 매수 신호 제한 설정 (1일 최대 매수 신호 개수)
        self.max_daily_buy_signals = 20
        self.daily_buy_signals_count = 0  # 당일 매수 신호 카운트
        self.last_reset_date = datetime.now().date()  # 마지막 리셋 날짜
        
        # 실시간 처리 관련 설정
        self.skip_realtime_prediction = False  # 일봉 기반 모델은 실시간 예측 생략
        self.realtime_confidence_threshold = 0.98  # 실시간 신호에 대한 별도 신뢰도 임계값 (매우 엄격하게)
        
        logger.info("DRL-UTrans 트레이딩 모델 초기화 완료")
        
    async def load_model(self):
        """DRL-UTrans 모델 로드 (PyTorch 2.6 호환성 문제 해결)"""
        try:
            # 모델 파일 경로 확인
            model_file = os.path.join(self.model_path, "drl_utrans_model.pth")
            scalers_file = os.path.join(self.model_path, "drl_utrans_scalers.pkl")
            
            if not os.path.exists(model_file):
                logger.error(f"DRL-UTrans 모델 파일이 없습니다: {model_file}")
                return False
            
            # 비동기 환경에서 모델 로드 (블로킹 작업)
            def _load_model():
                try:
                    # 모델 클래스 import
                    from app.models.drl_utrans_network import DRLUTransPPOnet
                    
                    # 기본 설정값 사용
                    input_dim = 26  # 기술적 지표 + 포트폴리오 정보
                    seq_len = 20    # 시퀀스 길이
                    action_dim = 3  # 액션: hold, buy, sell

                    logger.info(f"NumPy 버전: {np.__version__}")
                    
                    # ===== 오류 해결: 안전한 전역 함수 등록 =====
                    # 메시지에서 권장하는 대로 안전한 전역 함수 등록
                    try:
                        torch.serialization.add_safe_globals(['numpy._core.multiarray.scalar'])
                        logger.info("안전한 전역 함수 등록 완료: numpy._core.multiarray.scalar")
                    except Exception as e:
                        logger.warning(f"안전한 전역 함수 등록 실패: {str(e)}")
                    
                    # 모델 파일 로드 시도 1: weights_only=False 사용
                    logger.info("모델 파일 로드 시도 (weights_only=False 사용)")
                    try:
                        state_dict = torch.load(model_file, map_location=self.device, weights_only=False)
                        logger.info("모델 파일 로드 성공 (weights_only=False)")
                    except Exception as e:
                        logger.warning(f"weights_only=False 로드 실패: {str(e)}")
                        
                        # 모델 파일 로드 시도 2: 컨텍스트 매니저 사용
                        logger.info("모델 파일 로드 시도 (safe_globals 컨텍스트 매니저 사용)")
                        try:
                            with torch.serialization.safe_globals(['numpy._core.multiarray.scalar']):
                                state_dict = torch.load(model_file, map_location=self.device)
                            logger.info("모델 파일 로드 성공 (safe_globals 컨텍스트)")
                        except Exception as e2:
                            logger.error(f"모든 로드 방법 실패: {str(e2)}")
                            return None, {}
                    
                    # 모델 생성
                    logger.info("모델 인스턴스 생성")
                    model = DRLUTransPPOnet(
                        input_dim=input_dim,
                        seq_len=seq_len,
                        action_dim=action_dim,
                        d_model=64,
                        nhead=4,
                        num_layers=2,
                        d_ff=256,
                        dropout=0.1
                    ).to(self.device)
                    
                    # 가중치 로드
                    logger.info("모델 가중치 로드 시작")
                    # state_dict 형식 처리
                    if isinstance(state_dict, dict) and 'model_state_dict' in state_dict:
                        model_state_dict = state_dict['model_state_dict']
                    else:
                        # 직접 state_dict인 경우
                        model_state_dict = state_dict
                    
                    # 가중치 로드 (strict=False로 일부 레이어가 없어도 무시)
                    model.load_state_dict(model_state_dict, strict=False)
                    logger.info("모델 가중치 로드 완료")
                    
                    # 모델을 평가 모드로 설정
                    model.eval()
                    
                    # 스케일러 로드 (pickle 사용)
                    scalers = {}
                    if os.path.exists(scalers_file):
                        logger.info("스케일러 파일 로드 시작")
                        with open(scalers_file, 'rb') as f:
                            scalers = pickle.load(f)
                        logger.info("스케일러 파일 로드 완료")
                    else:
                        # 기본 스케일러 생성
                        scalers['default'] = StandardScaler()
                        logger.info("스케일러 파일 없음, 기본 스케일러 생성")
                    
                    # 모델 간단 테스트
                    logger.info("모델 동작 테스트 중...")
                    test_input = torch.zeros((1, seq_len, input_dim), dtype=torch.float32).to(self.device)
                    with torch.no_grad():
                        outputs = model(test_input)
                    
                    logger.info("모델 테스트 성공")
                    return model, scalers
                    
                except Exception as e:
                    logger.error(f"모델 로드 중 오류: {str(e)}", exc_info=True)
                    return None, {}
            
            # 비동기적으로 모델 로드
            loop = asyncio.get_event_loop()
            self.model, self.scalers = await loop.run_in_executor(None, _load_model)
            
            if self.model is None:
                logger.error("DRL-UTrans 모델 로드 실패")
                return False
            
            self.is_model_loaded = True
            logger.info(f"DRL-UTrans 모델 로드 완료 (device: {self.device})")
            return True
                
        except Exception as e:
            logger.error(f"DRL-UTrans 모델 로드 중 오류: {str(e)}", exc_info=True)
            return False
    
    async def start(self):
        """트레이딩 모델 시작"""
        if self.is_running:
            logger.warning("DRL-UTrans 트레이딩 모델이 이미 실행 중입니다.")
            return
        
        # 모델 로드
        if not self.is_model_loaded:
            model_loaded = await self.load_model()
            if not model_loaded:
                logger.error("DRL-UTrans 모델 로드 실패로 시작할 수 없습니다.")
                return
        
        self.is_running = True
        logger.info("DRL-UTrans 트레이딩 모델 시작")
        
        # 계좌 정보 초기 동기화
        if self.backend_client:
            account_info = await self.backend_client.request_account_info()
            if account_info:
                self.update_account_info(account_info)
                logger.info(f"계좌 정보 초기 동기화 완료: 현금={self.cash_balance}, 보유종목={len(self.positions)}")
        
        # 일괄 예측 스케줄러 시작
        asyncio.create_task(self.batch_prediction_scheduler())
        
        # 매매 신호 모니터링 시작
        asyncio.create_task(self.monitor_signals())
    
    async def stop(self):
        """트레이딩 모델 중지"""
        self.is_running = False
        logger.info("DRL-UTrans 트레이딩 모델 중지")
    
    async def refresh_indicators(self):
        """
        지표 갱신 (데이터 갱신 후 호출)
        일괄 예측을 비활성화하고 보유 종목 및 관심 종목만 분석
        """
        logger.info("DRL-UTrans 지표 갱신 시작 (제한된 예측 수행)")
        
        # 보유 종목 및 관심 종목 리스트 작성
        watchlist = []
        
        # 1. 보유 종목 추가
        watchlist.extend(list(self.positions.keys()))
        
        # 2. 관심 종목 추가 (예: 실시간 가격 갱신이 있었던 종목)
        recently_updated = []
        if self.stock_cache:
            # 최근 가격 업데이트가 있었던 종목 가져오기 (예: 최근 1시간)
            recently_updated = self.stock_cache.get_recently_updated_symbols(hours=1)
        
        # 중복 제거하며 합치기
        watchlist.extend([symbol for symbol in recently_updated if symbol not in watchlist])
        
        # 목록 길이 제한 (최대 30개)
        watchlist = watchlist[:30]
        
        if not watchlist:
            logger.info("DRL-UTrans 지표 갱신: 분석할 종목 없음")
            return
        
        logger.info(f"DRL-UTrans 지표 갱신: {len(watchlist)}개 종목 분석 (보유 종목 및 관심 종목)")
        
        # 선별된 종목만 예측 실행
        for symbol in watchlist:
            await self.process_single_prediction(symbol)
        
        logger.info(f"DRL-UTrans 지표 갱신 완료: {len(watchlist)}개 종목 분석 완료")

    
    def _should_process_price_update(self, symbol, price):
        """
        실시간 가격 업데이트 필터링
        (중복/불필요한 처리 방지)
        """
        now = datetime.now()
        
        # 이전 처리 가격과 시간 가져오기
        last_price = self.last_processed_prices.get(symbol, 0)
        last_time = self.last_processed_times.get(symbol, datetime.min)
        
        # 필터링 조건 확인
        
        # 1. 최소 가격 변동 확인
        price_change_pct = abs(price - last_price) / last_price if last_price > 0 else 1.0
        price_changed = price_change_pct >= self.min_price_change_pct / 100.0
        
        # 2. 최소 처리 간격 확인
        time_passed = (now - last_time).total_seconds() >= self.min_process_interval
        
        # 처리해야 하는 경우
        if price_changed or time_passed:
            # 마지막 처리 정보 업데이트
            self.last_processed_prices[symbol] = price
            self.last_processed_times[symbol] = now
            return True
        
        return False
    
    async def calculate_technical_features(self, symbol):
        """
        기술적 지표 계산 (DRL-UTrans 모델 입력용)
        """
        try:
            if not self.stock_cache:
                logger.error(f"종목 {symbol} 특성 계산 실패: stock_cache가 없습니다.")
                return None
            
            # 차트 데이터 가져오기 (최소 60일치 데이터 필요)
            chart_data = self.stock_cache.get_chart_data(symbol)
            if not chart_data or len(chart_data) < self.seq_len + 10:  # 여유분 포함 필요 데이터
                logger.warning(f"종목 {symbol} 차트 데이터 부족: {len(chart_data) if chart_data else 0}일")
                return None
            
            # 데이터프레임 변환
            df = pd.DataFrame(chart_data)
            
            # 필요한 컬럼 확인 (OHLCV)
            required_columns = ['date', 'open', 'high', 'low', 'close', 'volume']
            missing_columns = [col for col in required_columns if col not in df.columns]
            
            if missing_columns:
                logger.warning(f"종목 {symbol} 차트 데이터에 필요한 컬럼이 없습니다: {missing_columns}")
                return None
            
            # 날짜순 정렬 (오름차순)
            df['date'] = pd.to_datetime(df['date'])
            df = df.sort_values(by='date')
            
            # 숫자형 데이터로 변환
            numeric_columns = ['open', 'high', 'low', 'close', 'volume']
            for col in numeric_columns:
                df[col] = pd.to_numeric(df[col], errors='coerce')
            
            # NaN 값 처리
            df = df.ffill().bfill()
            
            # 기술적 지표 계산 (DRL-UTrans 모델과 동일한 지표 사용)
            
            # 1. 이동평균
            df['ma5'] = df['close'].rolling(window=5).mean()
            df['ma10'] = df['close'].rolling(window=10).mean()
            df['ma20'] = df['close'].rolling(window=20).mean()
            df['ma60'] = df['close'].rolling(window=60).mean()
            
            # 2. 볼린저 밴드 (20일 기준)
            df['ma20_std'] = df['close'].rolling(window=20).std()
            df['upper_band'] = df['ma20'] + (df['ma20_std'] * 2)
            df['lower_band'] = df['ma20'] - (df['ma20_std'] * 2)
            
            # 3. RSI (14일 기준)
            delta = df['close'].diff()
            up = delta.clip(lower=0)
            down = -1 * delta.clip(upper=0)
            avg_up = up.rolling(window=14).mean()
            avg_down = down.rolling(window=14).mean()
            rs = avg_up / avg_down
            df['rsi'] = 100 - (100 / (1 + rs))
            
            # 4. MACD
            exp1 = df['close'].ewm(span=12, adjust=False).mean()
            exp2 = df['close'].ewm(span=26, adjust=False).mean()
            df['macd'] = exp1 - exp2
            df['macd_signal'] = df['macd'].ewm(span=9, adjust=False).mean()
            df['macd_hist'] = df['macd'] - df['macd_signal']
            
            # 5. 거래량 이동평균
            df['vma20'] = df['volume'].rolling(window=20).mean()
            
            # 6. 가격 변화율
            df['pct_change'] = df['close'].pct_change()
            df['pct_change_5'] = df['close'].pct_change(periods=5)
            df['pct_change_10'] = df['close'].pct_change(periods=10)
            df['pct_change_20'] = df['close'].pct_change(periods=20)
            
            # 7. ATR (14일 기준)
            tr1 = df['high'] - df['low']
            tr2 = abs(df['high'] - df['close'].shift())
            tr3 = abs(df['low'] - df['close'].shift())
            tr = pd.concat([tr1, tr2, tr3], axis=1).max(axis=1)
            df['atr'] = tr.rolling(window=14).mean()
            
            # 8. 최근 N일 고점/저점 대비 하락/상승률
            for n in [5, 10, 20]:
                df[f'max_close_{n}'] = df['close'].rolling(window=n).max()
                df[f'min_close_{n}'] = df['close'].rolling(window=n).min()
                df[f'close_to_max_{n}'] = (df['close'] / df[f'max_close_{n}']) - 1
                df[f'close_to_min_{n}'] = (df['close'] / df[f'min_close_{n}']) - 1
            
            # NaN 값 처리
            df = df.ffill().bfill()
            
            # 모델 학습에 사용된 특성 선택
            selected_features = [
                'open', 'high', 'low', 'close', 'volume',
                'ma5', 'ma10', 'ma20', 'ma60',
                'upper_band', 'lower_band', 'rsi',
                'macd', 'macd_signal', 'macd_hist',
                'vma20', 'pct_change', 'pct_change_5', 'pct_change_10', 'atr',
                'close_to_max_10', 'close_to_min_10'
            ]
            
            # 특성 데이터프레임 추출
            features_df = df[selected_features].tail(self.seq_len)
            
            # 충분한 데이터가 있는지 확인
            if len(features_df) < self.seq_len:
                logger.warning(f"종목 {symbol} 특성 데이터 부족: {len(features_df)}개 (필요: {self.seq_len}개)")
                return None
            
            # 마지막 종가 저장 (나중에 사용)
            last_close = df['close'].iloc[-1]
            
            # 특성 배열로 변환
            features_array = features_df.values
            
            # 스케일링 (종목별 스케일러 사용)
            if symbol in self.scalers or 'default' in self.scalers:
                # 기존 스케일러 가져오기
                scaler = self.scalers.get(symbol, self.scalers.get('default'))
                
                # 스케일러가 feature_names를 사용했는지 확인 (set_output 사용 여부 확인)
                has_feature_names = hasattr(scaler, 'feature_names_in_')
                
                if has_feature_names:
                    # 스케일러에 저장된 feature_names 가져오기
                    feature_names = scaler.feature_names_in_
                    
                    # feature_names가 있다면 DataFrame으로 변환하여 동일한 열 이름 사용
                    try:
                        features_df = pd.DataFrame(features_array, columns=feature_names)
                        features_scaled = scaler.transform(features_df)
                    except Exception as e:
                        # DataFrame 변환 실패 시 기존 방식으로 진행
                        logger.warning(f"DataFrame 변환 실패, 기존 방식으로 스케일링: {str(e)}")
                        features_scaled = scaler.transform(features_array)
                else:
                    # feature_names가 없는 스케일러라면 그냥 변환
                    features_scaled = scaler.transform(features_array)
            else:
                # 새 스케일러 생성
                scaler = StandardScaler()
                scaler.fit(features_array)
                features_scaled = scaler.transform(features_array)
                self.scalers[symbol] = scaler
            
            # 포트폴리오 정보 추가 (기본값)
            # [잔고 비율, 주식 보유 비율, 평균 단가 대비 현재가 비율, 현재 손익률]
            portfolio_info = np.array([
                1.0,  # 잔고 비율 (기본값: 1.0)
                0.0,  # 주식 보유 비율 (기본값: 0.0)
                0.0,  # 평균 단가 대비 현재가 비율 (기본값: 0.0)
                0.0   # 현재 손익률 (기본값: 0.0)
            ])
            
            # 보유 종목인 경우 포트폴리오 정보 업데이트
            if self.is_holding(symbol):
                position = self.positions[symbol]
                position_value = position.get("value", 0)
                avg_price = position.get("avgPrice", 0)
                
                if avg_price > 0 and last_close > 0:
                    # 포트폴리오 정보 계산
                    total_value = self.cash_balance + sum(p.get("value", 0) for p in self.positions.values())
                    
                    # 포트폴리오 정보 업데이트
                    portfolio_info = np.array([
                        self.cash_balance / total_value if total_value > 0 else 1.0,  # 잔고 비율
                        position_value / total_value if total_value > 0 else 0.0,  # 주식 보유 비율
                        avg_price / last_close if last_close > 0 else 1.0,  # 평균 단가 대비 현재가 비율
                        (last_close / avg_price - 1) if avg_price > 0 else 0.0   # 현재 손익률
                    ])
            
            # 포트폴리오 정보를 각 시퀀스 단계에 추가
            # 결과: [seq_len, feature_dim + portfolio_dim]
            result = np.zeros((self.seq_len, features_scaled.shape[1] + portfolio_info.shape[0]), dtype=np.float32)
            
            for i in range(self.seq_len):
                result[i] = np.concatenate([features_scaled[i], portfolio_info])
            
            return result
            
        except Exception as e:
            logger.error(f"종목 {symbol} 특성 계산 중 오류: {str(e)}", exc_info=True)
            return None
    
    async def predict_stock(self, symbol, features=None):
        """
        DRL-UTrans 모델을 사용하여 단일 종목 예측
        """
        try:
            if not self.is_model_loaded or self.model is None:
                logger.error(f"종목 {symbol} 예측 실패: 모델이 로드되지 않았습니다.")
                return None
            
            # 특성 계산 (제공되지 않은 경우)
            if features is None:
                features = await self.calculate_technical_features(symbol)
                
            if features is None:
                logger.warning(f"종목 {symbol} 특성 계산 실패")
                return None
            
            # 텐서 변환
            features_tensor = torch.tensor(features, dtype=torch.float32).unsqueeze(0).to(self.device)
            
            # 예측 수행 (비동기 환경에서 블로킹 작업)
            def _run_prediction():
                try:
                    with torch.no_grad():
                        # DRLUTransPPOnet 모델은 (action_probs, action_weight, state_value) 반환
                        action_probs, action_weight, state_value = self.model(features_tensor)
                        
                        # 액션 확률 가져오기 (dim=1에서 argmax)
                        action = torch.argmax(action_probs, dim=1).item()
                        
                        # 액션 확률, 가중치, 가치 가져오기
                        action_prob = action_probs[0, action].item()
                        action_weight_val = action_weight.item()
                        state_value_val = state_value.item()
                        
                        return {
                            "action": action,  # 0=hold, 1=buy, 2=sell
                            "action_probs": action_probs[0].cpu().numpy().tolist(),  # 각 액션의 확률
                            "action_prob": action_prob,  # 선택된 액션의 확률
                            "action_weight": action_weight_val,  # 액션 가중치 (0~1)
                            "state_value": state_value_val,  # 상태 가치
                            "timestamp": datetime.now()
                        }
                except Exception as e:
                    logger.error(f"예측 수행 중 오류: {str(e)}", exc_info=True)
                    return None
            
            # 비동기적으로 예측 수행
            loop = asyncio.get_event_loop()
            result = await loop.run_in_executor(None, _run_prediction)
            
            if result is not None:
                logger.debug(f"종목 {symbol} 예측 완료: 액션={self.action_map[result['action']]}, " +
                           f"가중치={result['action_weight']:.4f}, 확률={result['action_prob']:.4f}")
            
            return result
            
        except Exception as e:
            logger.error(f"종목 {symbol} 예측 중 오류: {str(e)}", exc_info=True)
            return None
    
    async def batch_prediction_scheduler(self):
        """
        일괄 예측 스케줄러
        일정 주기로 모든 종목의 예측 수행
        """
        logger.info("DRL-UTrans 일괄 예측 스케줄러 시작")
        
        # 처음 시작 시 바로 예측 실행
        await self.run_batch_predictions()
        
        while self.is_running:
            try:
                # 다음 예측 시간까지 대기 (prediction_interval 시간)
                await asyncio.sleep(self.prediction_interval * 60 * 60)
                
                # 일괄 예측 실행
                await self.run_batch_predictions()
                
            except asyncio.CancelledError:
                logger.info("DRL-UTrans 일괄 예측 스케줄러 취소됨")
                break
            except Exception as e:
                logger.error(f"일괄 예측 스케줄러 오류: {str(e)}")
                await asyncio.sleep(15 * 60)  # 오류 시 15분 대기 후 재시도
    
    async def run_batch_predictions(self):
        """
        모든 종목에 대해 일괄 예측 수행
        """
        if not self.is_running or not self.is_model_loaded:
            logger.warning("일괄 예측을 수행할 수 없습니다: 모델이 실행 중이 아니거나 로드되지 않았습니다.")
            return
        
        try:
            # 필터링된 종목 목록 가져오기
            symbols = self.stock_cache.get_filtered_stocks() if self.stock_cache else []
            
            if not symbols:
                logger.warning("예측할 종목이 없습니다.")
                return
            
            # 보유 중인 종목 먼저 처리
            priority_symbols = []
            normal_symbols = []
            
            for symbol in symbols:
                if self.is_holding(symbol):
                    priority_symbols.append(symbol)
                else:
                    normal_symbols.append(symbol)
            
            # 순서 정렬 (보유 종목 먼저)
            sorted_symbols = priority_symbols + normal_symbols
            
            logger.info(f"총 {len(sorted_symbols)}개 종목 일괄 예측 시작 (보유 종목: {len(priority_symbols)}개)")
            
            # 이전 매매 신호 백업
            old_signals = dict(self.trading_signals)
            
            # 신호 초기화
            self.trading_signals = {}
            
            # 동시 처리 종목 수 제한 (너무 많은 동시 요청 방지)
            concurrency_limit = 5
            
            # 배치 단위로 처리
            for i in range(0, len(sorted_symbols), concurrency_limit):
                batch_symbols = sorted_symbols[i:i+concurrency_limit]
                
                # 배치 내 종목들을 동시에 처리
                tasks = [self.process_single_prediction(symbol) for symbol in batch_symbols]
                await asyncio.gather(*tasks)
                
                # API 부하 방지를 위한 짧은 대기
                await asyncio.sleep(0.5)
            
            # 매매 신호 결과 요약
            buy_signals = sum(1 for v in self.trading_signals.values() if v["action"] == 1)
            sell_signals = sum(1 for v in self.trading_signals.values() if v["action"] == 2)
            
            logger.info(f"일괄 예측 완료: 총 {len(sorted_symbols)}개 종목, 매수 신호={buy_signals}개, 매도 신호={sell_signals}개")
            
        except Exception as e:
            logger.error(f"일괄 예측 중 오류: {str(e)}", exc_info=True)
    
    async def handle_realtime_price(self, symbol, price, indicators=None):
        """
        실시간 가격 데이터 처리 - 추상 메서드 구현
        내부적으로 _process_price_update 공통 메서드를 호출하여 중복 코드 방지
        """
        if not self.is_running or not self.is_model_loaded:
            return
            
        # 공통 로직을 별도 메서드로 분리하여 호출
        await self._process_price_update(symbol, price, from_realtime=True)
        
    async def _process_price_update(self, symbol, price, from_realtime=False):
        """
        가격 업데이트 공통 처리 로직
        handle_realtime_price와 process_single_prediction에서 공통으로 사용
        
        :param symbol: 종목 코드
        :param price: 현재가
        :param from_realtime: 실시간 업데이트에서 호출되었는지 여부
        """
        try:
            # 중복 메시지 필터링 - 처리할 필요가 없으면 함수 종료
            if not self._should_process_price_update(symbol, price):
                return
            
            # 이 종목에 대한 예측 결과가 있는지 확인
            prediction_info = self.predictions.get(symbol)
            
            if not prediction_info:
                # 예측 결과가 없는 경우 온디맨드 예측 수행
                # 실시간 환경에서는 예측하지 않을 수도 있음 (일봉 기반 모델의 경우)
                if from_realtime and self.skip_realtime_prediction:
                    return
                    
                features = await self.calculate_technical_features(symbol)
                if features is not None:
                    prediction_info = await self.predict_stock(symbol, features)
                    if prediction_info:
                        self.predictions[symbol] = prediction_info
                    else:
                        return
                else:
                    return
            
            # 예측 결과가 너무 오래되었는지 확인 (prediction_valid_hours 이상)
            prediction_timestamp = prediction_info["timestamp"]
            now = datetime.now()
            
            if (now - prediction_timestamp).total_seconds() > self.prediction_valid_hours * 3600:
                # 예측이 오래되었으면 새로 예측
                # 실시간 환경에서는 예측하지 않을 수도 있음 (일봉 기반 모델의 경우)
                if from_realtime and self.skip_realtime_prediction:
                    return
                    
                features = await self.calculate_technical_features(symbol)
                if features is not None:
                    new_prediction = await self.predict_stock(symbol, features)
                    if new_prediction:
                        self.predictions[symbol] = new_prediction
                        prediction_info = new_prediction
                
            # 예측 결과 해석
            action = prediction_info["action"]
            action_prob = prediction_info["action_prob"]
            action_weight = await self.calculate_independent_weight(symbol)
            
            # 보유 여부 확인
            is_holding = self.is_holding(symbol)
            
            # 이미 매매 신호가 있는지 확인
            if symbol in self.trading_signals:
                existing_signal = self.trading_signals[symbol]
                existing_action = existing_signal["action"]
                
                # 이미 매수 신호가 있는데 매수한 경우 신호 제거
                if existing_action == 1 and is_holding:
                    del self.trading_signals[symbol]
                    logger.debug(f"종목 {symbol} 매수 완료, 신호 제거")
                    return
                
                # 이미 매도 신호가 있는데 매도한 경우 신호 제거
                elif existing_action == 2 and not is_holding:
                    del self.trading_signals[symbol]
                    logger.debug(f"종목 {symbol} 매도 완료, 신호 제거")
                    return
                
                # 그 외의 경우 기존 신호 유지
                return
            
            # 실시간 환경에서는 신호 생성 조건을 더 엄격하게 적용할 수 있음
            if from_realtime:
                required_confidence = self.confidence_threshold
                if self.realtime_confidence_threshold > self.confidence_threshold:
                    required_confidence = self.realtime_confidence_threshold
            else:
                required_confidence = self.confidence_threshold
            
            # 1. 매도 신호 (현재 보유 중인 종목만)
            if action == 2 and is_holding:
                if action_prob >= required_confidence and action_weight >= self.weight_threshold:
                    # 매도 신호 생성
                    self.trading_signals[symbol] = {
                        "action": action,
                        "action_name": self.action_map[action],
                        "weight": action_weight,
                        "price": price,
                        "confidence": action_prob,
                        "value": prediction_info["state_value"],
                        "timestamp": now,
                        "source": "realtime" if from_realtime else "batch"
                    }
                    log_msg = "실시간" if from_realtime else "일괄"
                    logger.info(f"{log_msg} 매도 신호 생성: {symbol}, 가격={price}, 확률={action_prob:.4f}, 가중치={action_weight:.4f}")
            
            # 2. 매수 신호 (현재 미보유 종목만)
            elif action == 1 and not is_holding:
                # 일일 매수 신호 제한 확인
                if self.daily_buy_signals_count >= self.max_daily_buy_signals:
                    # 실시간과 일괄 예측에 따라 다른 임계값 적용 가능
                    exception_threshold = 0.85 if from_realtime else 0.8
                    if action_prob < exception_threshold:
                        logger.debug(f"일일 매수 신호 제한({self.max_daily_buy_signals}개)에 도달, 신호 생성 보류: {symbol}")
                        return
                
                if action_prob >= required_confidence and action_weight >= self.weight_threshold:
                    # 매수 신호 생성
                    self.trading_signals[symbol] = {
                        "action": action,
                        "action_name": self.action_map[action],
                        "weight": action_weight,
                        "price": price,
                        "confidence": action_prob,
                        "value": prediction_info["state_value"],
                        "timestamp": now,
                        "source": "realtime" if from_realtime else "batch"
                    }
                    log_msg = "실시간" if from_realtime else "일괄"
                    logger.info(f"{log_msg} 매수 신호 생성: {symbol}, 가격={price}, 확률={action_prob:.4f}, 가중치={action_weight:.4f}")
                    
                    # 일일 매수 신호 카운트 증가
                    self.daily_buy_signals_count += 1
        
        except Exception as e:
            logger.error(f"가격 처리 중 오류: {str(e)}", exc_info=True)
    
    # process_single_prediction 메서드 수정 - 공통 메서드 사용
    async def process_single_prediction(self, symbol):
        """
        단일 종목 예측 및 매매 신호 생성
        내부적으로 _process_price_update 공통 메서드 활용
        """
        try:
            # 특성 계산
            features = await self.calculate_technical_features(symbol)
            if features is None:
                return
            
            # 예측 수행
            prediction = await self.predict_stock(symbol, features)
            if prediction is None:
                return
            
            # 예측 결과 저장
            self.predictions[symbol] = prediction
            
            # 현재가 가져오기
            current_price = self.stock_cache.get_price(symbol) if self.stock_cache else 0
            if current_price <= 0:
                logger.warning(f"종목 {symbol} 현재가를 가져올 수 없습니다.")
                return
            
            # 일일 매수 신호 제한 확인 및 필요시 카운터 리셋
            current_date = datetime.now().date()
            if current_date != self.last_reset_date:
                self.daily_buy_signals_count = 0
                self.last_reset_date = current_date
                logger.info(f"일일 매수 신호 카운터 리셋 (날짜 변경: {current_date})")
            
            # 공통 로직 호출 - from_realtime=False로 일괄 예측임을 표시
            await self._process_price_update(symbol, current_price, from_realtime=False)
            
        except Exception as e:
            logger.error(f"종목 {symbol} 예측 처리 중 오류: {str(e)}", exc_info=True)
    
    async def monitor_signals(self):
        """매매 신호 주기적 모니터링 및 처리"""
        logger.info("DRL-UTrans 매매 신호 모니터링 시작")
        
        while self.is_running:
            try:
                # 60초마다 매매 신호 확인 (원래 주기 유지)
                await asyncio.sleep(60)
                
                # 계좌 정보 동기화 (백엔드에서 가져옴)
                if self.backend_client:
                    account_info = await self.backend_client.request_account_info()
                    if account_info:
                        self.update_account_info(account_info)
                        logger.debug("계좌 정보 정기 동기화 완료")
                
                # 매매 신호 로깅
                if self.trading_signals:
                    # 출처별로 신호 카운트
                    batch_signals = [v for v in self.trading_signals.values() if v.get("source") == "batch"]
                    realtime_signals = [v for v in self.trading_signals.values() if v.get("source") == "realtime"]
                    unknown_signals = [v for v in self.trading_signals.values() if "source" not in v]
                    
                    # 타입별로 신호 카운트
                    signal_counts = {
                        "buy": sum(1 for v in self.trading_signals.values() if v["action"] == 1),
                        "sell": sum(1 for v in self.trading_signals.values() if v["action"] == 2),
                        "hold": sum(1 for v in self.trading_signals.values() if v["action"] == 0)
                    }
                    
                    # 평균 신뢰도 계산
                    avg_confidence = {}
                    for action_name in ["buy", "sell", "hold"]:
                        action_code = {"buy": 1, "sell": 2, "hold": 0}[action_name]
                        signals = [v for v in self.trading_signals.values() if v["action"] == action_code]
                        avg_confidence[action_name] = sum(v["confidence"] for v in signals) / max(1, len(signals))
                    
                    logger.info(f"현재 DRL-UTrans 매매 신호: " +
                            f"매수={signal_counts['buy']}개(신뢰도 {avg_confidence['buy']:.2f}), " +
                            f"매도={signal_counts['sell']}개(신뢰도 {avg_confidence['sell']:.2f})")
                            
                    logger.debug(f"신호 출처: 일괄예측={len(batch_signals)}개, 실시간={len(realtime_signals)}개, 미분류={len(unknown_signals)}개")
                
                # 오래된 신호 제거 (4.5시간 이상 경과)
                now = datetime.now()
                for symbol, signal_info in list(self.trading_signals.items()):
                    timestamp = signal_info["timestamp"]
                    if (now - timestamp).total_seconds() > 16200:  # 4.5시간
                        del self.trading_signals[symbol]
                        logger.debug(f"종목 {symbol}의 오래된 신호 제거 (4.5시간 경과)")
                
            except Exception as e:
                logger.error(f"매매 신호 모니터링 중 오류: {str(e)}")
                await asyncio.sleep(30)

    async def calculate_independent_weight(self, symbol):
        """
        기술적 지표에 기반한 독립적인 가중치 계산
        신뢰도(action_prob)와는 별개로 독립적인 지표로 사용
        """
        try:
            # 차트 데이터 가져오기
            if not self.stock_cache:
                return 0.5  # 기본값
                
            chart_data = self.stock_cache.get_chart_data(symbol)
            if not chart_data or len(chart_data) < 30:
                return 0.5  # 데이터 부족 시 기본값
                
            # 데이터프레임 변환
            df = pd.DataFrame(chart_data)
            df['date'] = pd.to_datetime(df['date'])
            df = df.sort_values(by='date')
            
            # 숫자형 데이터로 변환
            numeric_columns = ['open', 'high', 'low', 'close', 'volume']
            for col in numeric_columns:
                df[col] = pd.to_numeric(df[col], errors='coerce')
            
            # NaN 값 처리
            df = df.ffill().bfill()
            
            # 1. 추세 분석 (20%)
            # - 단기, 중기, 장기 이동평균선 방향
            df['ma5'] = df['close'].rolling(window=5).mean()
            df['ma20'] = df['close'].rolling(window=20).mean()
            df['ma60'] = df['close'].rolling(window=60).mean()
            
            # 이동평균선 방향 (상승=1, 하락=-1)
            ma5_direction = 1 if df['ma5'].iloc[-1] > df['ma5'].iloc[-2] else -1
            ma20_direction = 1 if df['ma20'].iloc[-1] > df['ma20'].iloc[-2] else -1
            ma60_direction = 1 if df['ma60'].iloc[-1] > df['ma60'].iloc[-3] else -1
            
            # 이동평균선 배열 (황금 십자 등)
            ma_alignment = 0
            if df['ma5'].iloc[-1] > df['ma20'].iloc[-1] > df['ma60'].iloc[-1]:
                ma_alignment = 1  # 완전한 상승 배열
            elif df['ma5'].iloc[-1] < df['ma20'].iloc[-1] < df['ma60'].iloc[-1]:
                ma_alignment = -1  # 완전한 하락 배열
            
            # 종합 추세 점수 (-1~1 범위)
            trend_score = (ma5_direction * 0.5 + ma20_direction * 0.3 + ma60_direction * 0.2 + ma_alignment) / 2
            
            # 2. 모멘텀 분석 (30%)
            # - RSI
            delta = df['close'].diff()
            gain = delta.where(delta > 0, 0)
            loss = -delta.where(delta < 0, 0)
            avg_gain = gain.rolling(window=14).mean()
            avg_loss = loss.rolling(window=14).mean()
            rs = avg_gain / avg_loss.replace(0, 0.001)  # 0으로 나누기 방지
            df['rsi'] = 100 - (100 / (1 + rs))
            
            # RSI 점수 변환 (0~100 -> -1~1, 중앙값 50)
            # 70 이상은 과매수, 30 이하는 과매도
            rsi_value = df['rsi'].iloc[-1]
            if rsi_value > 70:
                rsi_score = (70 - rsi_value) / 30  # 70 초과시 음수 (과매수 상태)
            elif rsi_value < 30:
                rsi_score = (30 - rsi_value) / 30  # 30 미만시 음수 (과매도 상태)
            else:
                rsi_score = (rsi_value - 50) / 20  # 30~70 사이는 -1~1로 변환
            
            # MACD
            df['ema12'] = df['close'].ewm(span=12, adjust=False).mean()
            df['ema26'] = df['close'].ewm(span=26, adjust=False).mean()
            df['macd'] = df['ema12'] - df['ema26']
            df['macd_signal'] = df['macd'].ewm(span=9, adjust=False).mean()
            df['macd_hist'] = df['macd'] - df['macd_signal']
            
            # MACD 히스토그램 방향 및 크기
            macd_hist = df['macd_hist'].iloc[-1]
            macd_hist_prev = df['macd_hist'].iloc[-2]
            macd_direction = macd_hist - macd_hist_prev
            
            # MACD 히스토그램 점수 (-1~1)
            macd_score = min(1, max(-1, macd_hist / abs(df['macd'].mean()) * 3))
            
            # 모멘텀 종합 점수
            momentum_score = rsi_score * 0.5 + macd_score * 0.5
            
            # 3. 변동성 분석 (20%)
            # - 볼린저 밴드
            df['std20'] = df['close'].rolling(window=20).std()
            df['upper_band'] = df['ma20'] + (df['std20'] * 2)
            df['lower_band'] = df['ma20'] - (df['std20'] * 2)
            
            # 볼린저 밴드 내 위치 (0~1)
            bb_range = df['upper_band'].iloc[-1] - df['lower_band'].iloc[-1]
            if bb_range > 0:
                bb_position = (df['close'].iloc[-1] - df['lower_band'].iloc[-1]) / bb_range
            else:
                bb_position = 0.5
            
            # 볼린저 밴드 폭 (표준화)
            bb_width = bb_range / df['ma20'].iloc[-1]
            bb_width_avg = df['std20'].rolling(window=20).mean().iloc[-1] * 4 / df['ma20'].iloc[-1]
            
            # 밴드 상대 폭 (평균 대비)
            relative_width = bb_width / bb_width_avg if bb_width_avg > 0 else 1
            
            # 변동성 점수 (-1~1)
            # 밴드가 좁아지면 양수, 넓어지면 음수
            volatility_score = (1 - min(2, relative_width)) * 0.5
            
            # 위치에 따른 조정 (상단 가까우면 음수, 하단 가까우면 양수)
            position_score = (0.5 - bb_position) * 2
            
            # 변동성 종합 점수
            volatility_score = volatility_score + position_score * 0.5
            
            # 4. 거래량 분석 (30%)
            # - 거래량 증가율
            df['vol_ma20'] = df['volume'].rolling(window=20).mean()
            volume_ratio = df['volume'].iloc[-1] / df['vol_ma20'].iloc[-1] if df['vol_ma20'].iloc[-1] > 0 else 1
            
            # 거래량 방향성 (가격과 일치 여부)
            price_up = df['close'].iloc[-1] > df['close'].iloc[-2]
            
            # 거래량 점수 계산
            if price_up:
                # 상승 + 거래량 증가 = 강한 긍정
                volume_score = min(1, (volume_ratio - 1) * 2) if volume_ratio > 1 else 0
            else:
                # 하락 + 거래량 증가 = 강한 부정
                volume_score = max(-1, (1 - volume_ratio) * 2) if volume_ratio > 1 else 0
            
            # 5. 종합 가중치 계산 (0~1 범위로 정규화)
            final_score = (
                trend_score * 0.2 +
                momentum_score * 0.3 +
                volatility_score * 0.2 +
                volume_score * 0.3
            )
            
            # -1~1 범위에서 0~1 범위로 변환
            normalized_weight = (final_score + 1) / 2
            
            # 가중치 범위 조정 (0.3~0.8)
            adjusted_weight = 0.3 + (normalized_weight * 0.5)
            
            # 결과 로깅
            logger.debug(f"{symbol} 기술적 가중치: {adjusted_weight:.4f} " +
                    f"[추세={trend_score:.2f}, 모멘텀={momentum_score:.2f}, " +
                    f"변동성={volatility_score:.2f}, 거래량={volume_score:.2f}]")
            
            return adjusted_weight
            
        except Exception as e:
            logger.error(f"독립 가중치 계산 오류 ({symbol}): {str(e)}", exc_info=True)
            return 0.5  # 오류 발생시 중간값 반환
    
    def is_holding(self, symbol):
        """종목 보유 여부 확인"""
        return symbol in self.positions
    
    def get_position(self, symbol):
        """종목 포지션 정보 반환"""
        return self.positions.get(symbol)
    
    def update_account_info(self, account_info: Dict):
      """계좌 정보 업데이트"""
      # 부모 클래스의 메서드 호출
      super().update_account_info(account_info)
      
      # 필요한 경우 추가 처리
      # 예: DRL-UTrans 모델에 필요한 특별한 계좌 정보 처리
      logger.debug(f"DRL-UTrans 계좌 정보 추가 처리 완료")
      
    
    async def get_trade_decisions(self, prices: Dict[str, float] = None) -> List[Dict[str, Any]]:
        """매매 의사결정 목록 반환 - 균형 잡힌 매수/매도 로직"""
        if not self.is_running:
            logger.warning("DRL-UTrans 트레이딩 모델이 실행 중이지 않습니다.")
            return []
        
        decisions = []
        try:
            # 계좌 정보 최신화 (백엔드에서 가져옴)
            if self.backend_client:
                account_info = await self.backend_client.request_account_info()
                if account_info:
                    self.update_account_info(account_info)
            
            # 계좌 정보 확인
            if not self.account_info:
                logger.warning("계좌 정보가 없습니다.")
                return []
            
            # 현재 시간
            now = datetime.now()
            
            # 매매 신호 목록 (신뢰도 기준 정렬)
            buy_signals = []
            sell_signals = []
            
            # 신호 분류 및 정렬 준비
            for symbol, signal_info in list(self.trading_signals.items()):
                action = signal_info["action"]
                
                # 신호 유효 시간 (4.5시간으로 조정) 체크
                timestamp = signal_info["timestamp"]
                if (now - timestamp).total_seconds() > 16200:  # 4.5시간
                    # 오래된 신호 제거
                    del self.trading_signals[symbol]
                    logger.debug(f"종목 {symbol}의 오래된 신호 제거 (4.5시간 경과)")
                    continue
                
                # 시그널 타입별로 분류
                if action == 1:  # 매수
                    buy_signals.append((symbol, signal_info))
                elif action == 2:  # 매도
                    sell_signals.append((symbol, signal_info))
            
            # 신뢰도 기준으로 정렬 (높은 순서대로)
            buy_signals.sort(key=lambda x: x[1]["confidence"], reverse=True)
            sell_signals.sort(key=lambda x: x[1]["confidence"], reverse=True)
            
            # 1. 매도 신호 먼저 처리 (자본 확보를 위해)
            for symbol, signal_info in sell_signals:
                action = signal_info["action"]
                confidence = signal_info["confidence"]
                weight = signal_info["weight"]
                
                # 보유 중인 종목인지 확인
                if symbol not in self.positions:
                    logger.debug(f"미보유 종목 매도 신호 무시: {symbol}")
                    del self.trading_signals[symbol]
                    continue
                
                position = self.positions[symbol]
                total_quantity = position.get("quantity", 0)
                
                if total_quantity <= 0:
                    logger.warning(f"종목 {symbol}의 보유 수량이 0, 매도 보류")
                    continue
                
                # 매도할 수량 계산 (비중에 따라 결정)
                sell_quantity = max(1, int(total_quantity * weight))
                
                # 현재가 확인
                current_price = 0
                if prices and symbol in prices:
                    current_price = prices[symbol]
                elif self.stock_cache:
                    current_price = self.stock_cache.get_price(symbol)
                    
                if not current_price or current_price <= 0:
                    logger.warning(f"종목 {symbol}의 가격 정보 없음, 매도 보류")
                    current_price = 0  # 가격 정보가 없어도 매도는 진행
                
                # 매도 결정 추가
                decision = {
                    "symbol": symbol,
                    "action": "sell",
                    "quantity": sell_quantity,
                    "price": current_price,
                    "reason": f"DRL-UTrans 모델 매도 신호 (신뢰도: {confidence:.2f}, 가중치: {weight:.2f})",
                    "confidence": confidence,
                    "weight": weight,
                    "timestamp": now.isoformat()
                }
                decisions.append(decision)
                
                # 거래 이력 업데이트
                self.trade_history[symbol] = {
                    "last_trade": now,
                    "last_action": "sell"
                }
                
                # 신호 제거
                del self.trading_signals[symbol]
                logger.info(f"매도 결정: {symbol}, {sell_quantity}주, 가격: {current_price if current_price > 0 else '시장가'}, 신뢰도: {confidence:.2f}")
            
            # 2. 매수 신호 처리 (신뢰도 기준 정렬했으므로 순서대로 처리)
            # 균형 잡힌 매수를 위해 상위 7개 신호 처리 (5개는 너무 적고, 10개는 너무 많음)
            max_buy_signals = 7
            buy_count = 0
            
            for symbol, signal_info in buy_signals:
                # 최대 매수 신호 제한
                if buy_count >= max_buy_signals:
                    logger.debug(f"최대 매수 신호 개수({max_buy_signals})에 도달, 신호 처리 중단")
                    break
                    
                # 현재 보유 종목 수 확인
                if len(self.positions) >= self.max_positions:
                    # 매우 강한 신호(0.9+)만 예외 허용
                    if signal_info["confidence"] < 0.9:
                        logger.debug(f"최대 보유 종목 수({self.max_positions}) 도달, 매수 보류: {symbol}")
                        continue
                
                # 충분한 현금 확인 - 균형 잡힌 매수 금액 기준 (80%)
                min_required_cash = self.trade_amount_per_stock * 0.8
                if self.cash_balance < min_required_cash:
                    logger.debug(f"현금 부족({self.cash_balance}), 매수 보류: {symbol}")
                    continue
                
                # 이미 보유 중인지 확인
                if symbol in self.positions:
                    logger.debug(f"이미 보유 중인 종목 매수 신호 무시: {symbol}")
                    del self.trading_signals[symbol]
                    continue
                
                # 매수 관련 정보 가져오기
                confidence = signal_info["confidence"]
                weight = signal_info["weight"]
                
                # 현재가 확인
                current_price = 0
                if prices and symbol in prices:
                    current_price = prices[symbol]
                elif self.stock_cache:
                    current_price = self.stock_cache.get_price(symbol)
                        
                if not current_price or current_price <= 0:
                    logger.warning(f"종목 {symbol}의 가격 정보 없음, 매수 보류")
                    continue
                
                # 매수 금액 계산 (신뢰도에 따른 조정)
                buy_amount_multiplier = 1.0
                if confidence >= 0.85:
                    buy_amount_multiplier = 1.1  # 높은 신뢰도는 10% 추가 투자
                
                buy_amount = self.trade_amount_per_stock * weight * buy_amount_multiplier
                
                # 남은 현금 상황에 따라 조정
                if buy_amount > self.cash_balance * 0.4:  # 잔액의 40% 이상 사용 방지
                    buy_amount = self.cash_balance * 0.4
                    logger.debug(f"종목 {symbol} 매수 금액 조정: {buy_amount:.0f}원 (잔액의 40%)")
                
                # 매수 수량 계산
                quantity = int(buy_amount / current_price)
                if quantity <= 0:
                    logger.warning(f"종목 {symbol}의 매수 수량이 0, 매수 보류")
                    continue
                
                # 매수 결정 추가
                decision = {
                    "symbol": symbol,
                    "action": "buy",
                    "quantity": quantity,
                    "price": current_price,
                    "reason": f"DRL-UTrans 모델 매수 신호 (신뢰도: {confidence:.2f}, 가중치: {weight:.2f})",
                    "confidence": confidence,
                    "weight": weight,
                    "timestamp": now.isoformat()
                }
                decisions.append(decision)
                
                # 거래 이력 업데이트
                self.trade_history[symbol] = {
                    "last_trade": now,
                    "last_action": "buy"
                }
                
                # 신호 제거
                del self.trading_signals[symbol]
                logger.info(f"매수 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}, 신뢰도: {confidence:.2f}")
                
                # 처리된 매수 신호 카운트 증가
                buy_count += 1
                
                # 자금 고갈 시 종료 - 최소 예비금 25%
                self.cash_balance -= (quantity * current_price)
                min_reserve = self.cash_balance * 0.25  # 전체 자금의 25%를 예비금으로 유지
                if self.cash_balance < min_reserve:
                    logger.debug(f"예비금 수준에 도달, 매수 신호 처리 중단 (잔액: {self.cash_balance})")
                    break
            
            return decisions
        
        except Exception as e:
            logger.error(f"매매 의사결정 생성 중 오류: {str(e)}", exc_info=True)
            return []