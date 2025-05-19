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
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional

from app.strategies.base import BaseTradingModel
from app.models.trade_models import TradingStrategy

logger = logging.getLogger(__name__)

class DRLUTransTradingModel(BaseTradingModel):
    """DRL-UTrans 기반 AI 트레이딩 모델"""
    
    def __init__(self, stock_cache=None):
        """DRL-UTrans 전략 트레이딩 모델 초기화"""
        super().__init__(stock_cache)
        
        # 매매 관련 설정
        self.max_positions = 15  # 최대 보유 종목 수
        self.trade_amount_per_stock = 6000000  # 종목당 매매 금액 (600만원)
        self.min_holding_period = 1  # 최소 보유 기간 (일)
        
        # 모델 설정
        self.model_path = os.environ.get('DRL_UTRANS_MODEL_PATH', 
                                        os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 
                                                    "models/drl_utrans"))
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.model = None
        self.scalers = {}  # 종목별 스케일러
        
        # 예측 관련 설정
        self.prediction_threshold = 0.65  # 예측 임계값 (신뢰도)
        self.prediction_window = 5  # 예측 기간 (일)
        self.max_prediction_age = 24 * 60 * 60  # 최대 예측 유효 시간 (초)
        
        # 매매 신호 저장 딕셔너리
        self.trading_signals = {}  # {symbol: {"signal": "buy", "price": price, "timestamp": datetime, "confidence": float}}
        
        # 종목별 예측 결과 저장
        self.predictions = {}  # {symbol: {"prediction": float, "confidence": float, "timestamp": datetime}}
        
        # 실행 상태
        self.is_running = False
        self.is_model_loaded = False
        
        # 한 번에 예측할 종목 수 (배치 크기)
        self.batch_size = 32
        
        logger.info("DRL-UTrans 트레이딩 모델 초기화 완료")
    
    async def load_model(self):
        """DRL-UTrans 모델 로드"""
        try:
            # 모델 파일 경로 확인
            model_file = os.path.join(self.model_path, "drl_utrans_model.pt")
            scalers_file = os.path.join(self.model_path, "scalers.pkl")
            
            if not os.path.exists(model_file):
                logger.error(f"DRL-UTrans 모델 파일이 없습니다: {model_file}")
                return False
            
            # 모델 로드 (비동기 환경에서 블로킹 작업이므로 run_in_executor 사용)
            def _load_model():
                try:
                    # PyTorch 모델 로드
                    model = torch.load(model_file, map_location=self.device)
                    model.eval()  # 평가 모드로 설정
                    
                    # 스케일러 로드 (있는 경우)
                    scalers = {}
                    if os.path.exists(scalers_file):
                        import pickle
                        with open(scalers_file, 'rb') as f:
                            scalers = pickle.load(f)
                    
                    return model, scalers
                except Exception as e:
                    logger.error(f"모델 로드 중 오류: {str(e)}")
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
            logger.error(f"DRL-UTrans 모델 로드 중 오류: {str(e)}")
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
        DRL-UTrans 모델은 자체 예측 로직이 있으므로 여기서는 
        새로운 데이터로 예측을 다시 실행하는 작업을 수행
        """
        logger.info("DRL-UTrans 지표 갱신 (일괄 예측 시작)")
        await self.run_batch_predictions()
    
    async def prepare_input_features(self, symbol):
        """
        모델 입력 특성 준비
        
        Args:
            symbol: 종목 코드
            
        Returns:
            torch.Tensor: 모델 입력 특성 텐서 또는 None (실패 시)
        """
        try:
            if not self.stock_cache:
                logger.error(f"종목 {symbol} 입력 특성 준비 실패: stock_cache가 없습니다.")
                return None
            
            # 차트 데이터 가져오기
            chart_data = self.stock_cache.get_chart_data(symbol)
            if not chart_data or len(chart_data) < 60:  # 최소 60일치 데이터 필요
                logger.warning(f"종목 {symbol} 차트 데이터 부족: {len(chart_data) if chart_data else 0}일")
                return None
            
            # 데이터프레임 변환
            df = pd.DataFrame(chart_data)
            
            # 필요한 컬럼 확인
            required_columns = ['date', 'open', 'high', 'low', 'close', 'volume']
            missing_columns = [col for col in required_columns if col not in df.columns]
            
            if missing_columns:
                logger.warning(f"종목 {symbol} 차트 데이터에 필요한 컬럼이 없습니다: {missing_columns}")
                return None
            
            # 숫자형 데이터로 변환
            numeric_columns = ['open', 'high', 'low', 'close', 'volume']
            for col in numeric_columns:
                df[col] = pd.to_numeric(df[col], errors='coerce')
            
            # NaN 값 처리
            df = df.fillna(method='ffill').fillna(method='bfill')
            
            # 거래량이 0인 행은 제외
            df = df[df['volume'] > 0]
            
            if len(df) < 60:
                logger.warning(f"종목 {symbol} 유효 데이터 부족: {len(df)}일")
                return None
            
            # DRL-UTrans 모델용 특성 계산 (간단한 기술적 지표만 사용)
            # 1. 가격 변화율
            df['price_change'] = df['close'].pct_change()
            
            # 2. 이동평균 계산
            for period in [5, 10, 20, 60]:
                df[f'ma{period}'] = df['close'].rolling(window=period).mean()
            
            # 3. 이동평균 대비 가격 비율
            for period in [5, 10, 20, 60]:
                df[f'close_to_ma{period}'] = df['close'] / df[f'ma{period}'] - 1
            
            # 4. 변동성 지표 (ATR)
            tr1 = df['high'] - df['low']
            tr2 = abs(df['high'] - df['close'].shift())
            tr3 = abs(df['low'] - df['close'].shift())
            df['tr'] = pd.concat([tr1, tr2, tr3], axis=1).max(axis=1)
            df['atr14'] = df['tr'].rolling(window=14).mean()
            
            # 5. 거래량 변화율
            df['volume_change'] = df['volume'].pct_change()
            
            # 모델 입력을 위한 특성 선택
            feature_columns = [
                'open', 'high', 'low', 'close', 'volume',
                'price_change', 
                'ma5', 'ma10', 'ma20', 'ma60',
                'close_to_ma5', 'close_to_ma10', 'close_to_ma20', 'close_to_ma60',
                'atr14', 'volume_change'
            ]
            
            # 특성 데이터 추출 (최근 60일 데이터)
            df = df.dropna()
            features_df = df[feature_columns].tail(60)
            
            if len(features_df) < 60:
                logger.warning(f"종목 {symbol} 입력 특성 길이 부족: {len(features_df)}일")
                return None
            
            # 스케일링 (종목별 스케일러가 있는 경우 사용)
            if symbol in self.scalers:
                scaler = self.scalers[symbol]
                features = scaler.transform(features_df.values)
            else:
                # 간단한 Min-Max 스케일링
                from sklearn.preprocessing import MinMaxScaler
                scaler = MinMaxScaler()
                features = scaler.fit_transform(features_df.values)
                self.scalers[symbol] = scaler
            
            # PyTorch 텐서로 변환
            features_tensor = torch.tensor(features, dtype=torch.float32, device=self.device)
            
            # 배치 차원 추가
            features_tensor = features_tensor.unsqueeze(0)
            
            return features_tensor
            
        except Exception as e:
            logger.error(f"종목 {symbol} 입력 특성 준비 중 오류: {str(e)}")
            return None
    
    async def predict_single_stock(self, symbol):
        """
        단일 종목 예측
        
        Args:
            symbol: 종목 코드
            
        Returns:
            dict: 예측 결과 딕셔너리 또는 None (실패 시)
        """
        try:
            if not self.is_model_loaded or self.model is None:
                logger.error(f"종목 {symbol} 예측 실패: 모델이 로드되지 않았습니다.")
                return None
            
            # 입력 특성 준비
            features = await self.prepare_input_features(symbol)
            if features is None:
                return None
            
            # 예측 수행 (비동기 환경에서 블로킹 작업이므로 run_in_executor 사용)
            def _run_prediction():
                try:
                    with torch.no_grad():
                        outputs = self.model(features)
                        
                        # 출력 구조에 따라 적절히 처리 (예: 다중 출력)
                        if isinstance(outputs, tuple):
                            prediction_values = outputs[0].cpu().numpy()  # 주 예측값
                            confidence_values = outputs[1].cpu().numpy()  # 신뢰도
                        else:
                            prediction_values = outputs.cpu().numpy()
                            confidence_values = np.abs(prediction_values)  # 확신도를 신뢰도로 사용
                        
                        # 배치 차원 제거
                        prediction = prediction_values[0]
                        confidence = confidence_values[0]
                        
                        # 결과 반환
                        return {
                            "prediction": float(prediction),
                            "confidence": float(confidence),
                            "timestamp": datetime.now()
                        }
                except Exception as e:
                    logger.error(f"예측 수행 중 오류: {str(e)}")
                    return None
            
            # 비동기적으로 예측 수행
            loop = asyncio.get_event_loop()
            result = await loop.run_in_executor(None, _run_prediction)
            
            return result
            
        except Exception as e:
            logger.error(f"종목 {symbol} 예측 중 오류: {str(e)}")
            return None
    
    async def batch_prediction_scheduler(self):
        """
        일괄 예측 스케줄러
        일정 주기로 모든 종목의 예측 수행
        """
        logger.info("DRL-UTrans 일괄 예측 스케줄러 시작")
        
        while self.is_running:
            try:
                # 일괄 예측 실행
                await self.run_batch_predictions()
                
                # 4시간 대기 후 다시 예측
                await asyncio.sleep(4 * 60 * 60)
                
            except asyncio.CancelledError:
                logger.info("DRL-UTrans 일괄 예측 스케줄러 취소됨")
                break
            except Exception as e:
                logger.error(f"일괄 예측 스케줄러 오류: {str(e)}")
                await asyncio.sleep(30 * 60)  # 오류 시 30분 대기 후 재시도
    
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
            
            logger.info(f"총 {len(symbols)}개 종목 일괄 예측 시작")
            
            # 배치 처리를 위한 종목 분할
            batches = [symbols[i:i+self.batch_size] for i in range(0, len(symbols), self.batch_size)]
            
            total_processed = 0
            buy_signals = 0
            sell_signals = 0
            
            # 배치 단위로 예측
            for batch_idx, batch_symbols in enumerate(batches):
                logger.info(f"배치 {batch_idx+1}/{len(batches)} 예측 시작 ({len(batch_symbols)}개 종목)")
                
                # 한 배치의 모든 종목을 비동기적으로 예측
                prediction_tasks = [self.predict_single_stock(symbol) for symbol in batch_symbols]
                prediction_results = await asyncio.gather(*prediction_tasks, return_exceptions=True)
                
                # 결과 처리
                for i, result in enumerate(prediction_results):
                    symbol = batch_symbols[i]
                    
                    if isinstance(result, Exception):
                        logger.error(f"종목 {symbol} 예측 예외 발생: {str(result)}")
                        continue
                    
                    if result is None:
                        logger.warning(f"종목 {symbol} 예측 결과 없음")
                        continue
                    
                    # 예측 결과 저장
                    self.predictions[symbol] = result
                    
                    # 현재가 가져오기
                    current_price = self.stock_cache.get_price(symbol) if self.stock_cache else 0
                    
                    # 예측값과 신뢰도 확인
                    prediction = result["prediction"]
                    confidence = result["confidence"]
                    
                    # 예측값 해석 및 매매 신호 생성
                    # 예측값이 0보다 크면 매수, 작으면 매도 (임계값 이상 신뢰도일 때만)
                    if confidence >= self.prediction_threshold:
                        if prediction > 0.1:  # 상승 예측 (10% 이상)
                            # 보유 중인지 확인
                            if not self.is_holding(symbol):
                                # 매수 신호 생성
                                self.trading_signals[symbol] = {
                                    "signal": "buy",
                                    "price": current_price,
                                    "timestamp": datetime.now(),
                                    "confidence": confidence,
                                    "prediction": prediction
                                }
                                buy_signals += 1
                                logger.info(f"종목 {symbol} 매수 신호 생성: 예측={prediction:.4f}, 신뢰도={confidence:.4f}")
                        
                        elif prediction < -0.1:  # 하락 예측 (10% 이상)
                            # 보유 중인지 확인
                            if self.is_holding(symbol):
                                # 매도 신호 생성
                                self.trading_signals[symbol] = {
                                    "signal": "sell",
                                    "price": current_price,
                                    "timestamp": datetime.now(),
                                    "confidence": confidence,
                                    "prediction": prediction
                                }
                                sell_signals += 1
                                logger.info(f"종목 {symbol} 매도 신호 생성: 예측={prediction:.4f}, 신뢰도={confidence:.4f}")
                    
                    total_processed += 1
                
                # 배치 간 간격
                await asyncio.sleep(0.5)
            
            logger.info(f"일괄 예측 완료: 총 {total_processed}개 종목, 매수 신호={buy_signals}개, 매도 신호={sell_signals}개")
            
        except Exception as e:
            logger.error(f"일괄 예측 중 오류: {str(e)}")
    
    async def monitor_signals(self):
        """매매 신호 주기적 모니터링 및 처리"""
        logger.info("DRL-UTrans 매매 신호 모니터링 시작")
        
        while self.is_running:
            try:
                # 1분마다 매매 신호 확인
                await asyncio.sleep(60)
                
                # 계좌 정보 동기화 (백엔드에서 가져옴)
                if self.backend_client:
                    account_info = await self.backend_client.request_account_info()
                    if account_info:
                        self.update_account_info(account_info)
                        logger.debug("계좌 정보 정기 동기화 완료")
                
                # 매매 신호 로깅
                if self.trading_signals:
                    signal_counts = {
                        "buy": sum(1 for v in self.trading_signals.values() if v["signal"] == "buy"),
                        "sell": sum(1 for v in self.trading_signals.values() if v["signal"] == "sell")
                    }
                    
                    # 신뢰도 평균 계산
                    avg_confidence = {
                        "buy": sum(v["confidence"] for v in self.trading_signals.values() if v["signal"] == "buy") / max(1, signal_counts["buy"]),
                        "sell": sum(v["confidence"] for v in self.trading_signals.values() if v["signal"] == "sell") / max(1, signal_counts["sell"])
                    }
                    
                    logger.info(f"현재 DRL-UTrans 매매 신호: 매수={signal_counts['buy']}개(신뢰도 {avg_confidence['buy']:.2f}), " +
                              f"매도={signal_counts['sell']}개(신뢰도 {avg_confidence['sell']:.2f})")
                
                # 오래된 신호 제거 (2시간 이상 경과)
                now = datetime.now()
                for symbol, signal_info in list(self.trading_signals.items()):
                    timestamp = signal_info["timestamp"]
                    if (now - timestamp).total_seconds() > 7200:
                        del self.trading_signals[symbol]
                        logger.debug(f"종목 {symbol}의 오래된 신호 제거 (2시간 경과)")
                
            except Exception as e:
                logger.error(f"매매 신호 모니터링 중 오류: {str(e)}")
                await asyncio.sleep(30)
    
    async def handle_realtime_price(self, symbol, price, indicators=None):
        """실시간 가격 데이터 처리"""
        if not self.is_running:
            return
        
        try:
            # 중복 메시지 필터링 - 처리할 필요가 없으면 함수 종료
            if not self._should_process_price_update(symbol, price):
                return
            
            # 이 종목에 대한 예측 결과가 있는지 확인
            prediction_info = self.predictions.get(symbol)
            
            if not prediction_info:
                # 예측 결과가 없으면 처리하지 않음
                return
            
            # 예측 결과가 너무 오래되었는지 확인 (24시간 이상 지났으면 무시)
            prediction_timestamp = prediction_info["timestamp"]
            now = datetime.now()
            
            if (now - prediction_timestamp).total_seconds() > self.max_prediction_age:
                # 예측이 오래되었으면 새로 예측
                new_prediction = await self.predict_single_stock(symbol)
                if new_prediction:
                    self.predictions[symbol] = new_prediction
                    prediction_info = new_prediction
                else:
                    # 새 예측에 실패하면 이전 예측 사용
                    pass
            
            # 예측값과 신뢰도 가져오기
            prediction = prediction_info["prediction"]
            confidence = prediction_info["confidence"]
            
            # 보유 여부 확인
            is_holding = self.is_holding(symbol)
            
            # 신뢰도가 임계값 이상일 때만 처리
            if confidence >= self.prediction_threshold:
                # 이미 매매 신호가 있는지 확인
                if symbol in self.trading_signals:
                    existing_signal = self.trading_signals[symbol]["signal"]
                    
                    # 기존 매수 신호가 있는데 보유하게 된 경우 신호 제거
                    if existing_signal == "buy" and is_holding:
                        del self.trading_signals[symbol]
                    
                    # 기존 매도 신호가 있는데 더 이상 보유하지 않은 경우 신호 제거
                    elif existing_signal == "sell" and not is_holding:
                        del self.trading_signals[symbol]
                    
                    # 그 외의 경우 기존 신호 유지
                    return
                
                # 새 매매 신호 생성
                if prediction > 0.1 and not is_holding:  # 상승 예측 (10% 이상) & 미보유
                    # 매수 신호 생성
                    self.trading_signals[symbol] = {
                        "signal": "buy",
                        "price": price,
                        "timestamp": now,
                        "confidence": confidence,
                        "prediction": prediction
                    }
                    logger.info(f"실시간 DRL-UTrans 매수 신호 생성: {symbol}, 가격={price}, 예측={prediction:.4f}, 신뢰도={confidence:.4f}")
                
                elif prediction < -0.1 and is_holding:  # 하락 예측 (10% 이상) & 보유 중
                    # 손절 확인 (보유 평균가 대비 10% 이상 하락 시)
                    position = self.get_position(symbol)
                    if position:
                        avg_price = position.get("avgPrice", 0)
                        
                        # 평균가 대비 현재가 하락률
                        drop_rate = (avg_price - price) / avg_price if avg_price > 0 else 0
                        
                        # 하락률이 10% 이상이거나 예측이 매우 비관적인 경우(-0.2 이하) 매도 신호
                        if drop_rate >= 0.1 or prediction < -0.2:
                            self.trading_signals[symbol] = {
                                "signal": "sell",
                                "price": price,
                                "timestamp": now,
                                "confidence": confidence,
                                "prediction": prediction,
                                "drop_rate": drop_rate
                            }
                            logger.info(f"실시간 DRL-UTrans 매도 신호 생성: {symbol}, 가격={price}, 예측={prediction:.4f}, 신뢰도={confidence:.4f}, 하락률={drop_rate:.4f}")
            
        except Exception as e:
            logger.error(f"실시간 가격 처리 중 오류: {str(e)}")
    
    async def get_trade_decisions(self, prices: Dict[str, float] = None) -> List[Dict[str, Any]]:
        """매매 의사결정 목록 반환"""
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
                signal = signal_info["signal"]
                
                # 신호 유효 시간 (2시간) 체크
                timestamp = signal_info["timestamp"]
                if (now - timestamp).total_seconds() > 7200:
                    # 오래된 신호 제거
                    del self.trading_signals[symbol]
                    logger.debug(f"종목 {symbol}의 오래된 신호 제거 (2시간 경과)")
                    continue
                
                # 시그널 타입별로 분류
                if signal == "buy":
                    buy_signals.append((symbol, signal_info))
                elif signal in ["sell", "stop_loss"]:
                    sell_signals.append((symbol, signal_info))
            
            # 신뢰도 기준으로 정렬 (높은 순서대로)
            buy_signals.sort(key=lambda x: x[1]["confidence"], reverse=True)
            sell_signals.sort(key=lambda x: x[1]["confidence"], reverse=True)
            
            # 1. 매도/손절 신호 먼저 처리 (자본 확보를 위해)
            for symbol, signal_info in sell_signals:
                signal = signal_info["signal"]
                price = signal_info["price"]
                confidence = signal_info["confidence"]
                prediction = signal_info.get("prediction", 0)
                
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
                
                # 매도 결정 추가 (전량)
                current_price = price
                if not current_price or current_price <= 0:
                    # 제공된 prices 딕셔너리에서 조회
                    if prices and symbol in prices:
                        current_price = prices[symbol]
                    # 또는 stock_cache에서 조회
                    elif self.stock_cache:
                        current_price = self.stock_cache.get_price(symbol)
                        
                    if not current_price or current_price <= 0:
                        logger.warning(f"종목 {symbol}의 가격 정보 없음, 매도 보류")
                        continue
                
                # 손절인지 정상 매도인지 구분
                reason = "DRL-UTrans 모델 예측 기반 손절 매도" if signal == "stop_loss" else f"DRL-UTrans 모델 예측 기반 매도 (예측: {prediction:.2f})"
                
                decision = {
                    "symbol": symbol,
                    "action": "sell",
                    "quantity": total_quantity,
                    "price": current_price,
                    "reason": f"{reason} (신뢰도: {confidence:.2f})",
                    "confidence": confidence,
                    "prediction": prediction,
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
                logger.info(f"매도 결정: {symbol}, {total_quantity}주, 가격: {current_price:.2f}, 이유: {reason}, 신뢰도: {confidence:.2f}")
            
            # 2. 매수 신호 처리 (신뢰도 기준 정렬했으므로 순서대로 처리)
            for symbol, signal_info in buy_signals:
                # 현재 보유 종목 수 확인
                if len(self.positions) >= self.max_positions:
                    logger.debug(f"최대 보유 종목 수({self.max_positions}) 도달, 매수 보류: {symbol}")
                    continue
                
                # 충분한 현금 확인
                if self.cash_balance < self.trade_amount_per_stock:
                    logger.debug(f"현금 부족({self.cash_balance}), 매수 보류: {symbol}")
                    continue
                
                # 이미 보유 중인지 확인
                if symbol in self.positions:
                    logger.debug(f"이미 보유 중인 종목 매수 신호 무시: {symbol}")
                    del self.trading_signals[symbol]
                    continue
                
                # 매수 수량 계산
                price = signal_info["price"]
                confidence = signal_info["confidence"]
                prediction = signal_info.get("prediction", 0)
                
                current_price = price
                if not current_price or current_price <= 0:
                    # 제공된 prices 딕셔너리에서 조회
                    if prices and symbol in prices:
                        current_price = prices[symbol]
                    # 또는 stock_cache에서 조회
                    elif self.stock_cache:
                        current_price = self.stock_cache.get_price(symbol)
                        
                    if not current_price or current_price <= 0:
                        logger.warning(f"종목 {symbol}의 가격 정보 없음, 매수 보류")
                        continue
                
                quantity = int(self.trade_amount_per_stock / current_price)
                if quantity <= 0:
                    logger.warning(f"종목 {symbol}의 매수 수량이 0, 매수 보류")
                    continue
                
                # 매수 결정 추가
                decision = {
                    "symbol": symbol,
                    "action": "buy",
                    "quantity": quantity,
                    "price": current_price,
                    "reason": f"DRL-UTrans 모델 예측 기반 매수 (예측: {prediction:.2f}, 신뢰도: {confidence:.2f})",
                    "confidence": confidence,
                    "prediction": prediction,
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
                logger.info(f"매수 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}, 예측: {prediction:.2f}, 신뢰도: {confidence:.2f}")
                
                # 자금 고갈 시 종료
                self.cash_balance -= (quantity * current_price)
                if self.cash_balance < self.trade_amount_per_stock:
                    logger.debug(f"가용 자금 소진, 매수 신호 처리 중단 (잔액: {self.cash_balance})")
                    break
            
            return decisions
        
        except Exception as e:
            logger.error(f"매매 의사결정 생성 중 오류: {str(e)}", exc_info=True)
            return []