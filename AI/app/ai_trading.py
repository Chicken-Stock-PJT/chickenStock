import numpy as np
import pandas as pd
import tensorflow as tf
from datetime import datetime
from typing import Dict, Any, List, Optional, Union
import logging
from .models import MarketData, TradeDecision
from .config import settings

logger = logging.getLogger(__name__)

class StockScreener:
    def __init__(self):
        """주식 스크리닝 클래스 초기화"""
        self.market_data = {}  # 심볼별 시장 데이터 저장
        self.market_stats = {}  # 심볼별 통계 저장
    
    def update_market_data(self, symbol: str, data: MarketData):
        """새로운 시장 데이터 업데이트"""
        if symbol not in self.market_data:
            self.market_data[symbol] = []
        
        self.market_data[symbol].append(data)
        
        # 버퍼 크기 제한
        max_buffer = 100
        if len(self.market_data[symbol]) > max_buffer:
            self.market_data[symbol] = self.market_data[symbol][-max_buffer:]
        
        # 통계 업데이트
        self._update_stats(symbol)
    
    def _update_stats(self, symbol: str):
        """종목별 통계 업데이트"""
        data_list = self.market_data[symbol]
        if len(data_list) < 20:  # 최소 20개 데이터 필요
            return
        
        # 가격 및 거래량 데이터 추출
        prices = [d.price for d in data_list]
        volumes = [d.volume for d in data_list]
        
        # 기본 통계 계산
        self.market_stats[symbol] = {
            "last_price": prices[-1],
            "last_volume": volumes[-1],
            "price_mean": np.mean(prices),
            "price_std": np.std(prices),
            "volume_mean": np.mean(volumes),
            "volume_std": np.std(volumes),
            "price_change_1d": prices[-1] / prices[0] - 1 if prices[0] > 0 else 0,
            "updated_at": datetime.now()
        }
        
        # 추가적인 기술적 지표 계산 (예: 이동평균, RSI 등)
        if len(prices) >= 20:
            self.market_stats[symbol]["ma5"] = np.mean(prices[-5:])
            self.market_stats[symbol]["ma20"] = np.mean(prices[-20:])
            
            # 볼륨 변화
            vol_prev = np.mean(volumes[-20:-10])
            vol_recent = np.mean(volumes[-10:])
            self.market_stats[symbol]["volume_change"] = vol_recent / vol_prev if vol_prev > 0 else 0
    
    def screen_stocks(self, max_stocks: int = 5) -> List[Dict[str, Any]]:
        """투자 가치가 있는 종목 선별"""
        candidates = []
        
        for symbol, stats in self.market_stats.items():
            # 최근 업데이트된 데이터만 고려
            time_diff = (datetime.now() - stats["updated_at"]).total_seconds()
            if time_diff > 3600:  # 1시간 이상 업데이트 안된 데이터는 스킵
                continue
            
            # 충분한 데이터가 없는 종목은 스킵
            if len(self.market_data[symbol]) < 20:
                continue
            
            # 점수 계산을 위한 지표들
            try:
                # 1. 단기 이동평균이 장기 이동평균 위에 있는지 (상승 추세)
                trend_score = stats["ma5"] / stats["ma20"] - 1 if stats["ma20"] > 0 else 0
                
                # 2. 거래량 증가 확인
                volume_score = stats["volume_change"] - 1 if stats["volume_change"] > 0 else 0
                
                # 3. 변동성 점수 (적절한 변동성이 있는 종목 선호)
                volatility = stats["price_std"] / stats["price_mean"] if stats["price_mean"] > 0 else 0
                volatility_score = min(volatility * 10, 1.0)  # 0~1 사이 값으로 정규화
                
                # 4. 최근 가격 변화
                momentum_score = stats["price_change_1d"] * 5  # 가중치 적용
                
                # 종합 점수 계산 (가중치 적용)
                # 상승 추세(40%), 거래량 증가(30%), 적절한 변동성(10%), 모멘텀(20%)
                score = (
                    trend_score * 0.4 + 
                    volume_score * 0.3 + 
                    volatility_score * 0.1 + 
                    momentum_score * 0.2
                )
                
                # 최종 점수에 페널티/보너스 적용
                # 예: 너무 낮은 가격이나 너무 높은 가격은 페널티
                price = stats["last_price"]
                if price < 1000:  # 너무 저렴한 주식 (예: 펌핑 위험)
                    score *= 0.5
                elif price > 1000000:  # 너무 비싼 주식
                    score *= 0.8
                
                candidates.append({
                    "symbol": symbol,
                    "score": score,
                    "last_price": stats["last_price"],
                    "volume_change": stats["volume_change"],
                    "trend": trend_score,
                    "volatility": volatility
                })
                
            except Exception as e:
                logger.error(f"Error calculating score for {symbol}: {str(e)}")
                continue
        
        # 점수 기준 상위 종목 선택
        candidates.sort(key=lambda x: x["score"], reverse=True)
        return candidates[:max_stocks]


class TradingModel:
    def __init__(self):
        """AI 트레이딩 모델 초기화"""
        self.model = self._load_model()
        self.data_buffer = {}  # 심볼별 데이터 버퍼
        self.positions = {}  # 현재 보유 포지션
        self.trade_history = {}  # 거래 내역
        self.window_size = 20  # 분석을 위한 데이터 윈도우 크기
        self.performance_metrics = {
            "total_trades": 0,
            "profitable_trades": 0,
            "total_profit": 0.0,
            "win_rate": 0.0
        }
    
    def _load_model(self) -> Optional[tf.keras.Model]:
        """TensorFlow 모델 로드"""
        try:
            model = tf.keras.models.load_model(settings.MODEL_PATH)
            logger.info(f"AI model loaded from {settings.MODEL_PATH}")
            return model
        except Exception as e:
            logger.warning(f"Failed to load model: {str(e)}. Using simple strategy.")
            return None
    
    def process_data(self, symbol: str, data: MarketData) -> Optional[TradeDecision]:
        """시장 데이터 처리 및 거래 결정 생성"""
        # 데이터 버퍼에 추가
        if symbol not in self.data_buffer:
            self.data_buffer[symbol] = []
        
        self.data_buffer[symbol].append(data)
        
        # 버퍼 크기 제한
        if len(self.data_buffer[symbol]) > self.window_size * 2:
            self.data_buffer[symbol] = self.data_buffer[symbol][-self.window_size * 2:]
        
        # 충분한 데이터가 있으면 거래 결정 생성
        if len(self.data_buffer[symbol]) >= self.window_size:
            return self._generate_trade_decision(symbol, data)
        
        return None
    
    def _generate_trade_decision(self, symbol: str, latest_data: MarketData) -> TradeDecision:
        """거래 결정 생성"""
        # TensorFlow 모델이 있으면 사용
        if self.model is not None:
            try:
                # 모델 입력 준비
                features = self._prepare_features(symbol)
                
                # 예측 수행
                prediction = self.model.predict(features, verbose=0)[0]
                
                # 예측 결과 해석 (예시: [매수확률, 매도확률, 홀딩확률])
                action_idx = np.argmax(prediction)
                actions = ["BUY", "SELL", "HOLD"]
                action = actions[action_idx]
                confidence = float(prediction[action_idx])
                
                # 포지션 확인하여 적절한 거래 결정
                return self._validate_decision(
                    symbol=symbol,
                    action=action,
                    confidence=confidence,
                    price=latest_data.price
                )
                
            except Exception as e:
                logger.error(f"Error in AI prediction for {symbol}: {str(e)}")
                # 모델 예측 실패 시 단순 전략 사용
                return self._simple_strategy(symbol)
        else:
            # 모델이 없으면 단순 전략 사용
            return self._simple_strategy(symbol)
    
    def _prepare_features(self, symbol: str) -> np.ndarray:
        """모델 입력을 위한 특성 준비"""
        data_list = self.data_buffer[symbol][-self.window_size:]
        
        # 기본 특성: 가격, 거래량, 고가, 저가
        prices = np.array([d.price for d in data_list])
        volumes = np.array([d.volume for d in data_list])
        highs = np.array([d.high if d.high else d.price for d in data_list])
        lows = np.array([d.low if d.low else d.price for d in data_list])
        
        # 가격 정규화 (최근 윈도우 기준)
        price_mean = np.mean(prices)
        price_std = np.std(prices) if np.std(prices) > 0 else 1
        norm_prices = (prices - price_mean) / price_std
        
        # 거래량 정규화
        volume_mean = np.mean(volumes)
        volume_std = np.std(volumes) if np.std(volumes) > 0 else 1
        norm_volumes = (volumes - volume_mean) / volume_std
        
        # 고가 및 저가 정규화
        norm_highs = (highs - price_mean) / price_std
        norm_lows = (lows - price_mean) / price_std
        
        # 추가 파생 특성 계산
        # 1. 단기 이동평균 (5일)
        ma5 = np.convolve(prices, np.ones(5)/5, mode='valid')
        ma5 = np.pad(ma5, (5-1, 0), 'constant', constant_values=(ma5[0],))
        
        # 2. 장기 이동평균 (20일)
        ma20 = prices.mean() * np.ones_like(prices)  # 데이터가 충분하지 않으면 평균값으로 대체
        
        # 3. MACD 계산
        ema12 = prices[-1]  # 간소화: 실제로는 EMA 계산 필요
        ema26 = prices[-1]  # 간소화: 실제로는 EMA 계산 필요
        macd = ema12 - ema26
        
        # 4. 상대 강도 지수 (RSI)
        # 간소화된 RSI 계산
        diff = np.diff(prices, prepend=prices[0])
        gain = np.where(diff > 0, diff, 0).mean()
        loss = np.where(diff < 0, -diff, 0).mean()
        rs = gain / loss if loss > 0 else 1
        rsi = 100 - (100 / (1 + rs))
        
        # 특성 벡터 구성
        features = np.column_stack([
            norm_prices,
            norm_volumes,
            norm_highs,
            norm_lows,
            (ma5 - price_mean) / price_std,  # 정규화된 MA5
            (ma20 - price_mean) / price_std,  # 정규화된 MA20
            np.ones_like(prices) * macd / price_std,  # 정규화된 MACD
            np.ones_like(prices) * rsi / 100  # 0-1 범위의 RSI
        ])
        
        # 모델 입력 형태로 변환 (샘플 1개, 시퀀스 길이, 특성 수)
        return np.expand_dims(features, axis=0)
    
    def _simple_strategy(self, symbol: str) -> TradeDecision:
        """단순 이동평균 기반 전략"""
        data_list = self.data_buffer[symbol]
        prices = np.array([d.price for d in data_list])
        
        # 단기 및 장기 이동평균 계산
        short_window = min(5, len(prices))
        long_window = min(20, len(prices))
        
        short_ma = prices[-short_window:].mean()
        long_ma = prices[-long_window:].mean()
        current_price = prices[-1]
        
        # 전략 로직
        if short_ma > long_ma * 1.01:
            action = "BUY"
            confidence = min(0.7, (short_ma / long_ma - 1) * 10)
        elif short_ma < long_ma * 0.99:
            action = "SELL"
            confidence = min(0.7, (1 - short_ma / long_ma) * 10)
        else:
            action = "HOLD"
            confidence = 0.5
        
        # 포지션 확인하여 적절한 거래 결정
        return self._validate_decision(
            symbol=symbol,
            action=action,
            confidence=confidence,
            price=current_price
        )
    
    def _validate_decision(self, symbol: str, action: str, confidence: float, price: float) -> TradeDecision:
        """포지션을 고려하여 거래 결정 유효성 검증"""
        has_position = symbol in self.positions and self.positions[symbol]["quantity"] > 0
        
        # 이미 매수한 종목에 대해 다시 매수 신호가 오면 HOLD로 변경
        if has_position and action == "BUY":
            action = "HOLD"
            confidence = 0.5
        
        # 포지션이 없는데 매도 신호가 오면 무시
        if not has_position and action == "SELL":
            action = "HOLD"
            confidence = 0.5
        
        # 최종 거래 결정 생성
        decision = TradeDecision(
            symbol=symbol,
            action=action,
            confidence=confidence,
            price=price,
            quantity=self._calculate_quantity(symbol, price, action),
            timestamp=datetime.now()
        )
        
        return decision
    
    def _calculate_quantity(self, symbol: str, price: float, action: str) -> int:
        """거래 수량 계산"""
        # 매수인 경우: 적절한 매수 수량 계산
        if action == "BUY":
            # 간단한 예시: 1,000,000원 기준으로 계산 (실제로는 더 복잡한 로직 필요)
            target_amount = 1000000  # 100만원 기준 매수
            quantity = max(1, int(target_amount / price))
            return quantity
        
        # 매도인 경우: 보유 수량 전체 매도
        elif action == "SELL" and symbol in self.positions:
            return self.positions[symbol]["quantity"]
        
        # 그 외 경우
        return 0
    
    def update_trade_result(self, result: Dict[str, Any]):
        """거래 결과 업데이트 및 성과 추적"""
        try:
            symbol = result["symbol"]
            action = result["action"]
            price = result["price"]
            quantity = result["quantity"]
            timestamp = datetime.fromtimestamp(result["timestamp"] / 1000)
            
            # 거래 내역 추가
            if symbol not in self.trade_history:
                self.trade_history[symbol] = []
            
            self.trade_history[symbol].append({
                "action": action,
                "price": price,
                "quantity": quantity,
                "timestamp": timestamp,
                "profit": result.get("profit", 0)
            })
            
            # 포지션 업데이트
            if action == "BUY":
                if symbol not in self.positions:
                    self.positions[symbol] = {
                        "quantity": 0,
                        "avg_price": 0,
                        "total_cost": 0
                    }
                
                # 포지션 정보 업데이트
                current = self.positions[symbol]
                new_quantity = current["quantity"] + quantity
                new_total_cost = current["total_cost"] + (price * quantity)
                new_avg_price = new_total_cost / new_quantity if new_quantity > 0 else 0
                
                self.positions[symbol] = {
                    "quantity": new_quantity,
                    "avg_price": new_avg_price,
                    "total_cost": new_total_cost
                }
                
            elif action == "SELL":
                if symbol in self.positions:
                    # 판매 수량만큼 포지션 감소
                    current = self.positions[symbol]
                    new_quantity = max(0, current["quantity"] - quantity)
                    
                    if new_quantity == 0:
                        # 모두 매도한 경우
                        self.positions.pop(symbol, None)
                    else:
                        # 일부만 매도한 경우 (평균 가격은 변동 없음)
                        self.positions[symbol]["quantity"] = new_quantity
                        self.positions[symbol]["total_cost"] = new_quantity * current["avg_price"]
            
            # 성과 지표 업데이트
            self.performance_metrics["total_trades"] += 1
            
            profit = result.get("profit", 0)
            if profit > 0:
                self.performance_metrics["profitable_trades"] += 1
                self.performance_metrics["total_profit"] += profit
            
            # 승률 업데이트
            if self.performance_metrics["total_trades"] > 0:
                self.performance_metrics["win_rate"] = (
                    self.performance_metrics["profitable_trades"] / 
                    self.performance_metrics["total_trades"]
                )
                
            logger.info(f"Updated position for {symbol}: {self.positions.get(symbol, 'No position')}")
            
        except Exception as e:
            logger.error(f"Error updating trade result: {str(e)}")
    
    def get_performance(self) -> Dict[str, Any]:
        """현재 트레이딩 성과 조회"""
        return {
            **self.performance_metrics,
            "active_positions": len(self.positions),
            "positions": [
                {
                    "symbol": symbol,
                    "quantity": pos["quantity"],
                    "avg_price": pos["avg_price"],
                    "total_cost": pos["total_cost"]
                }
                for symbol, pos in self.positions.items()
            ]
        }