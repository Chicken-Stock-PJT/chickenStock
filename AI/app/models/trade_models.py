"""
거래 관련 데이터 모델
DRL-UTrans 모델 관련 클래스 포함
"""
from enum import Enum
from typing import Dict, List, Optional, Any, Union
from datetime import datetime
import numpy as np

class TradingStrategy(str, Enum):
    """트레이딩 전략 유형"""
    ENVELOPE = "ENVELOPE"
    BOLLINGER = "BOLLINGER"
    SHORT_TERM = "SHORT_TERM"
    DRL_UTRANS = "DRL_UTRANS"  # DRL-UTrans 전략 추가됨

class TradeDecision:
    """매매 결정 모델"""
    def __init__(
        self,
        symbol: str,
        action: str,
        quantity: int,
        price: float,
        reason: str,
        confidence: float = None,
        weight: float = None,
        prediction_value: float = None
    ):
        self.symbol = symbol
        self.action = action  # "buy" 또는 "sell"
        self.quantity = quantity
        self.price = price
        self.reason = reason
        self.timestamp = datetime.now().isoformat()
        
        # DRL-UTrans 모델 관련 추가 필드
        self.confidence = confidence  # 모델 예측 확신도
        self.weight = weight  # 매매 비중 가중치
        self.prediction_value = prediction_value  # 상태 가치 예측값
    
    def to_dict(self) -> Dict[str, Any]:
        """딕셔너리로 변환"""
        result = {
            "symbol": self.symbol,
            "action": self.action,
            "quantity": self.quantity,
            "price": self.price,
            "reason": self.reason,
            "timestamp": self.timestamp
        }
        
        # DRL-UTrans 관련 필드가 있는 경우에만 포함
        if self.confidence is not None:
            result["confidence"] = self.confidence
        if self.weight is not None:
            result["weight"] = self.weight
        if self.prediction_value is not None:
            result["prediction_value"] = self.prediction_value
            
        return result

class DRLUTransPrediction:
    """DRL-UTrans 모델 예측 결과"""
    def __init__(
        self,
        symbol: str,
        action: int,  # 0: hold, 1: buy, 2: sell
        action_probs: List[float],
        action_weight: float,
        state_value: float,
        price: float = None
    ):
        self.symbol = symbol
        self.action = action
        self.action_name = {0: "hold", 1: "buy", 2: "sell"}.get(action, "unknown")
        self.action_probs = action_probs
        self.action_prob = action_probs[action] if action < len(action_probs) else 0
        self.action_weight = action_weight
        self.state_value = state_value
        self.price = price
        self.timestamp = datetime.now()
    
    def to_dict(self) -> Dict[str, Any]:
        """딕셔너리로 변환"""
        return {
            "symbol": self.symbol,
            "action": self.action,
            "action_name": self.action_name,
            "action_probs": self.action_probs,
            "action_prob": self.action_prob,
            "action_weight": self.action_weight,
            "state_value": self.state_value,
            "price": self.price,
            "timestamp": self.timestamp.isoformat()
        }
    
    @staticmethod
    def from_dict(data: Dict[str, Any]) -> 'DRLUTransPrediction':
        """딕셔너리에서 객체 생성"""
        return DRLUTransPrediction(
            symbol=data["symbol"],
            action=data["action"],
            action_probs=data.get("action_probs", [0.33, 0.33, 0.34]),
            action_weight=data.get("action_weight", 0.5),
            state_value=data.get("state_value", 0.0),
            price=data.get("price")
        )

class DRLUTransSignal:
    """DRL-UTrans 매매 신호"""
    def __init__(
        self,
        symbol: str,
        action: int,  # 0: hold, 1: buy, 2: sell
        weight: float,
        price: float,
        confidence: float,
        value: float = 0.0,
        timestamp: datetime = None
    ):
        self.symbol = symbol
        self.action = action
        self.action_name = {0: "hold", 1: "buy", 2: "sell"}.get(action, "unknown")
        self.weight = weight  # 매매 비중 (0~1)
        self.price = price
        self.confidence = confidence  # 신호 확신도 (0~1)
        self.value = value  # 상태 가치
        self.timestamp = timestamp or datetime.now()
    
    def to_dict(self) -> Dict[str, Any]:
        """딕셔너리로 변환"""
        return {
            "symbol": self.symbol,
            "action": self.action,
            "action_name": self.action_name,
            "weight": self.weight,
            "price": self.price,
            "confidence": self.confidence,
            "value": self.value,
            "timestamp": self.timestamp.isoformat()
        }
    
    @staticmethod
    def from_dict(data: Dict[str, Any]) -> 'DRLUTransSignal':
        """딕셔너리에서 객체 생성"""
        timestamp = data.get("timestamp")
        if isinstance(timestamp, str):
            timestamp = datetime.fromisoformat(timestamp)
        
        return DRLUTransSignal(
            symbol=data["symbol"],
            action=data["action"],
            weight=data["weight"],
            price=data["price"],
            confidence=data["confidence"],
            value=data.get("value", 0.0),
            timestamp=timestamp
        )
    
    def to_trade_decision(self, quantity: int) -> TradeDecision:
        """TradeDecision 객체로 변환"""
        action_str = "buy" if self.action == 1 else "sell" if self.action == 2 else "hold"
        if action_str == "hold":
            return None  # 홀딩 신호는 거래 결정으로 변환하지 않음
        
        reason = f"DRL-UTrans 모델 {self.action_name} 신호 (확률: {self.confidence:.2f}, 가중치: {self.weight:.2f})"
        
        return TradeDecision(
            symbol=self.symbol,
            action=action_str,
            quantity=quantity,
            price=self.price,
            reason=reason,
            confidence=self.confidence,
            weight=self.weight,
            prediction_value=self.value
        )

class DRLUTransFeatures:
    """DRL-UTrans 모델 입력 특성"""
    def __init__(
        self,
        symbol: str,
        ohlcv_data: np.ndarray,  # [seq_len, 5] 형태의 OHLCV 데이터
        technical_features: np.ndarray,  # [seq_len, n_features] 형태의 기술적 지표
        portfolio_info: np.ndarray = None  # [4] 형태의 포트폴리오 정보
    ):
        self.symbol = symbol
        self.ohlcv_data = ohlcv_data
        self.technical_features = technical_features
        
        # 기본 포트폴리오 정보 (보유 없음)
        if portfolio_info is None:
            self.portfolio_info = np.array([
                1.0,  # 잔고 비율
                0.0,  # 주식 보유 비율
                0.0,  # 평균 단가 대비 현재가 비율
                0.0   # 현재 손익률
            ])
        else:
            self.portfolio_info = portfolio_info
        
        self.timestamp = datetime.now()
    
    def get_model_input(self) -> np.ndarray:
        """
        모델 입력 형태로 변환
        [seq_len, features_dim + portfolio_dim] 형태 반환
        """
        # 특성 결합 (기본 데이터 + 기술적 지표)
        combined_features = np.concatenate(
            [self.ohlcv_data, self.technical_features], 
            axis=1
        )
        
        # 각 시퀀스 단계마다 포트폴리오 정보 추가
        seq_len = combined_features.shape[0]
        feature_dim = combined_features.shape[1]
        portfolio_dim = self.portfolio_info.shape[0]
        
        result = np.zeros((seq_len, feature_dim + portfolio_dim), dtype=np.float32)
        
        for i in range(seq_len):
            result[i] = np.concatenate([combined_features[i], self.portfolio_info])
        
        return result
    
    @staticmethod
    def from_dataframe(df, symbol, portfolio_info=None):
        """
        Pandas DataFrame에서 특성 생성
        df: OHLCV 및 기술적 지표가 포함된 데이터프레임
        """
        # OHLCV 데이터 추출
        ohlcv_columns = ['open', 'high', 'low', 'close', 'volume']
        if all(col in df.columns for col in ohlcv_columns):
            ohlcv_data = df[ohlcv_columns].values
        else:
            raise ValueError(f"DataFrame에 필수 OHLCV 컬럼이 없습니다: {ohlcv_columns}")
        
        # 기술적 지표 컬럼 (OHLCV 제외한 나머지)
        tech_columns = [col for col in df.columns if col not in ohlcv_columns]
        technical_features = df[tech_columns].values
        
        return DRLUTransFeatures(
            symbol=symbol,
            ohlcv_data=ohlcv_data,
            technical_features=technical_features,
            portfolio_info=portfolio_info
        )