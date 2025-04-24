import numpy as np
from datetime import datetime
from typing import Dict, Any, List, Optional, Union
import logging
import asyncio
from collections import defaultdict

from .models import TradeDecision
from .config import settings
from .envelope import ChartDataProcessor

logger = logging.getLogger(__name__)

class TradingModel:
    def __init__(self, kiwoom_api):
        """AI 트레이딩 모델 초기화"""
        # 키움 API 객체
        self.kiwoom_api = kiwoom_api
        
        # 차트 데이터 처리기 초기화
        self.chart_processor = ChartDataProcessor()
        
        # 계좌 정보 (키움 API에서 받아옴)
        self.account = {
            "cash_balance": 0,  # 현금 잔고
            "positions": {},    # 보유 종목 정보
            "total_asset_value": 0  # 총 자산가치 (현금 + 포지션)
        }
        
        # 매매 파라미터
        self.position_size_pct = settings.POSITION_SIZE_PCT  # 매수 시 총자산의 5%를 사용
        self.max_positions = settings.MAX_POSITIONS  # 최대 동시 보유 종목 수
        
        # 차트 데이터 캐시 관리
        self.cache_initialized = False
        
        # 포지션 관리를 위한 데이터
        self.position_history = {}  # 종목 코드 -> 포지션 이력
        self.initial_position_sizes = {}  # 종목 코드 -> 최초 매수 수량
        
        # 실시간 가격 및 처리 관련
        self.realtime_prices = {}  # 종목 코드 -> 현재가
        self.pending_decisions = defaultdict(list)  # 종목 코드 -> 매매 결정 리스트
        self.last_decision_time = {}  # 종목 코드 -> 마지막 매매 결정 시간
        
        # 결정 제한 (같은 종목에 대해 너무 자주 결정하지 않도록)
        self.decision_cooldown = 600  # 10분 (초 단위)
    
    async def start(self):
        """트레이딩 모델 시작"""
        # 계좌 정보 초기화
        self.update_account_info(self.kiwoom_api.account_info)
        logger.info("Trading model started")
        return True
    
    async def stop(self):
        """트레이딩 모델 종료"""
        logger.info("Trading model stopped")
        return True
    
    def update_account_info(self, account_info: Dict[str, Any]):
        """키움 API에서 받아온 계좌 정보 업데이트 및 포지션 관리 데이터 갱신"""
        try:
            # 기존 포지션 기록
            old_positions = set(self.account["positions"].keys())
            
            # 계좌 정보 업데이트
            self.account = account_info
            
            # 새 포지션 기록
            new_positions = set(self.account["positions"].keys())
            
            # 새로 추가된 포지션 (매수한 종목)
            added_positions = new_positions - old_positions
            for symbol in added_positions:
                position_data = self.account["positions"][symbol]
                # 최초 매수 수량 기록
                if symbol not in self.initial_position_sizes:
                    self.initial_position_sizes[symbol] = position_data["quantity"]
                    self.position_history[symbol] = {
                        "initial_quantity": position_data["quantity"],
                        "current_quantity": position_data["quantity"],
                        "position_status": "FULL",  # 전량 보유 상태
                        "last_action": "BUY",
                        "timestamp": datetime.now().isoformat()
                    }
                    logger.info(f"New position recorded for {symbol}: {position_data['quantity']} shares")
            
            # 제거된 포지션 (전량 매도된 종목)
            removed_positions = old_positions - new_positions
            for symbol in removed_positions:
                # 포지션 이력 업데이트
                if symbol in self.position_history:
                    self.position_history[symbol]["current_quantity"] = 0
                    self.position_history[symbol]["position_status"] = "NONE"
                    self.position_history[symbol]["last_action"] = "SELL"
                    self.position_history[symbol]["timestamp"] = datetime.now().isoformat()
                    logger.info(f"Position closed for {symbol}")
            
            # 변경된 포지션 (일부 매도 등)
            common_positions = old_positions.intersection(new_positions)
            for symbol in common_positions:
                old_quantity = self.account["positions"].get(symbol, {}).get("quantity", 0)
                new_quantity = self.account["positions"][symbol]["quantity"]
                
                # 수량이 변경된 경우
                if old_quantity != new_quantity and symbol in self.position_history:
                    self.position_history[symbol]["current_quantity"] = new_quantity
                    
                    # 포지션 상태 업데이트
                    initial_qty = self.position_history[symbol]["initial_quantity"]
                    position_ratio = new_quantity / initial_qty if initial_qty > 0 else 0
                    
                    if position_ratio <= 0:
                        status = "NONE"  # 보유 없음
                    elif position_ratio < 0.7:  # 여유 마진을 둠 (정확히 0.5가 아닐 수 있음)
                        status = "HALF"  # 절반 보유
                    else:
                        status = "FULL"  # 전량 보유
                    
                    self.position_history[symbol]["position_status"] = status
                    
                    if new_quantity < old_quantity:
                        self.position_history[symbol]["last_action"] = "SELL"
                    else:
                        self.position_history[symbol]["last_action"] = "BUY"
                    
                    self.position_history[symbol]["timestamp"] = datetime.now().isoformat()
                    logger.info(f"Position updated for {symbol}: {new_quantity}/{initial_qty} shares, status: {status}")
            
            logger.info(f"Account info updated: Cash={self.account['cash_balance']}, Positions={len(self.account['positions'])}")
            return True
        except Exception as e:
            logger.error(f"Error updating account info: {str(e)}")
            return False
    
    async def initialize_chart_data(self, symbols: List[str]):
        """모든 종목의 차트 데이터 초기화 및 캐싱"""
        # 차트 데이터 캐시가 유효하지 않은 경우에만 다시 로드
        if not self.chart_processor.is_cache_valid():
            logger.info("Initializing envelope indicators for all symbols...")
            await self.chart_processor.preload_envelope_indicators(symbols, self.kiwoom_api)
            self.cache_initialized = True
            logger.info("Envelope indicators initialized and cached")
        else:
            logger.info("Using existing envelope indicators cache")
            self.cache_initialized = True
    
    def handle_realtime_price(self, symbol: str, price: float):
        """실시간 가격 수신 처리 및 매매 신호 생성"""
        try:
            # 가격 저장
            self.realtime_prices[symbol] = price
            
            # 쿨다운 체크 (너무 자주 판단하지 않도록)
            current_time = datetime.now().timestamp()
            last_time = self.last_decision_time.get(symbol, 0)
            
            if current_time - last_time < self.decision_cooldown:
                return
            
            # 차트 데이터가 초기화되지 않았으면 처리하지 않음
            if not self.cache_initialized:
                return
                
            # 비동기로 매매 신호 처리
            asyncio.create_task(self.process_data(symbol, price))
            
        except Exception as e:
            logger.error(f"Error handling realtime price for {symbol}: {str(e)}")
    
    async def process_data(self, symbol: str, current_price: float) -> Optional[TradeDecision]:
        """차트 데이터 처리 및 거래 결정 생성"""
        # 계좌 정보가 없으면 거래 결정 생성하지 않음
        if self.account["total_asset_value"] == 0:
            logger.warning("Missing account information, cannot make trade decisions")
            return None
        
        # 현재 가격 검증
        if current_price is None or current_price <= 0:
            logger.warning(f"Invalid current price for {symbol}: {current_price}")
            return None
        
        # Envelope 지표 조회 (캐시 사용)
        indicators = self.chart_processor.get_envelope_indicators(symbol, current_price)
        
        if not indicators:
            logger.warning(f"Failed to get envelope indicators for {symbol}")
            return None
        
        # Envelope 전략 기반 거래 결정 생성
        trade_decision = self._make_envelope_decision(symbol, indicators)
        
        # 결정이 있으면 대기 목록에 추가
        if trade_decision:
            self.pending_decisions[symbol].append(trade_decision)
            self.last_decision_time[symbol] = datetime.now().timestamp()
            logger.info(f"New trade decision for {symbol}: {trade_decision.action} {trade_decision.quantity} shares at {trade_decision.price}")
        
        return trade_decision
    
    def _make_envelope_decision(self, symbol: str, indicators: Dict[str, Any]) -> Optional[TradeDecision]:
        """Envelope 전략 기반 거래 결정 생성 (포지션 관리 로직 포함)"""
        # 기본 정보 추출 - 현재 가격은 실시간으로 받아온 가격 사용
        realtime_price = indicators["currentPrice"]  # 실시간 API에서 받아온 현재 가격
        ma20 = indicators["MA20"]  # 캐싱된 20일 이동평균선
        upper_band = indicators["upperBand"]  # 캐싱된 상한선 
        lower_band = indicators["lowerBand"]  # 캐싱된 하한선
        
        # 현재 포지션 상태 확인
        position_status = "NONE"  # 기본값: 보유 없음
        if symbol in self.account["positions"]:
            # 포지션 이력이 있는 경우
            if symbol in self.position_history:
                position_status = self.position_history[symbol]["position_status"]
            else:
                # 포지션 이력이 없으면 새로 생성
                current_quantity = self.account["positions"][symbol]["quantity"]
                self.initial_position_sizes[symbol] = current_quantity
                self.position_history[symbol] = {
                    "initial_quantity": current_quantity,
                    "current_quantity": current_quantity,
                    "position_status": "FULL",  # 기본값: 전량 보유
                    "last_action": "BUY",
                    "timestamp": datetime.now().isoformat()
                }
                position_status = "FULL"
        
        # 로그 추가 - 거래 판단 기준값 확인
        logger.debug(f"{symbol} - 실시간 가격: {realtime_price}, MA20: {ma20}, 상한선: {upper_band}, 하한선: {lower_band}, 포지션: {position_status}")
        
        # 매매 신호 판단 - 실시간 가격과 캐싱된 지표 비교 (포지션 상태 고려)
        action = "HOLD"
        confidence = 0.5  # 기본 신뢰도
        sell_portion = 0.0  # 매도 비율 (0: 매도 없음, 0.5: 절반 매도, 1: 전량 매도)
        
        # 매수 신호 (하한선 아래로 실시간 가격이 떨어지면) - 보유 포지션이 없을 때만
        if realtime_price <= lower_band and position_status == "NONE":
            action = "BUY"
            # 하한선으로부터 얼마나 아래인지에 따라 신뢰도 계산
            distance = (lower_band - realtime_price) / lower_band
            confidence = min(0.9, 0.5 + distance * 2)
            logger.info(f"{symbol} - 매수 신호: 실시간 가격({realtime_price})이 하한선({lower_band}) 이하, 신뢰도: {confidence:.2f}")
        
        # 매도 신호 - 포지션 상태에 따라 다른 전략 적용
        elif realtime_price >= ma20:
            # 전량 보유 상태에서 중앙선 도달 시 절반 매도
            if position_status == "FULL" and realtime_price >= ma20:
                action = "SELL"
                sell_portion = 0.5  # 절반만 매도
                confidence = 0.6
                logger.info(f"{symbol} - 절반 매도 신호: 실시간 가격({realtime_price})이 중앙선({ma20}) 이상, 현재 포지션: {position_status}, 신뢰도: {confidence:.2f}")
            
            # 절반 보유 상태에서 상한선 도달 시 나머지 매도
            elif position_status == "HALF" and realtime_price >= upper_band:
                action = "SELL"
                sell_portion = 1.0  # 남은 것 전부 매도
                # 상한선 이상에서 매도 시 상한선으로부터 얼마나 위인지에 따라 신뢰도 계산
                distance = (realtime_price - upper_band) / upper_band
                confidence = min(0.9, 0.7 + distance * 2)
                logger.info(f"{symbol} - 잔량 매도 신호: 실시간 가격({realtime_price})이 상한선({upper_band}) 이상, 현재 포지션: {position_status}, 신뢰도: {confidence:.2f}")
            
            # 전량 보유 상태에서 상한선 도달 시 전량 매도 (특수 상황)
            elif position_status == "FULL" and realtime_price >= upper_band:
                action = "SELL"
                sell_portion = 1.0  # 전량 매도
                # 상한선 이상에서 매도 시 상한선으로부터 얼마나 위인지에 따라 신뢰도 계산
                distance = (realtime_price - upper_band) / upper_band
                confidence = min(0.9, 0.7 + distance * 2)
                logger.info(f"{symbol} - 전량 매도 신호(특수): 실시간 가격({realtime_price})이 상한선({upper_band}) 이상, 현재 포지션: {position_status}, 신뢰도: {confidence:.2f}")
        else:
            logger.debug(f"{symbol} - 관망 신호: 실시간 가격({realtime_price})이 하한선({lower_band})과 중앙선({ma20}) 사이")
        
        # HOLD 신호면 거래 결정 생성 안 함
        if action == "HOLD":
            return None
            
        # 포지션 상태를 고려한 거래 결정 검증
        return self._validate_decision(symbol, action, realtime_price, confidence, sell_portion, indicators)
    
    def _validate_decision(self, symbol: str, action: str, price: float, confidence: float, 
                          sell_portion: float, indicators: Dict[str, Any]) -> Optional[TradeDecision]:
        """포지션 상태를 고려하여 거래 결정 유효성 검증"""
        # 현재 보유 중인 종목인지 확인
        has_position = symbol in self.account["positions"]
        
        # 이미 매수한 종목에 대해 다시 매수 신호가 오면 무시
        if has_position and action == "BUY":
            return None
        
        # 보유하지 않은 종목에 대해 매도 신호가 오면 무시
        if not has_position and action == "SELL":
            return None
        
        # 최대 포지션 수 제한 (매수 시에만 적용)
        if action == "BUY" and len(self.account["positions"]) >= self.max_positions:
            logger.info(f"Maximum positions reached ({self.max_positions}), ignoring BUY for {symbol}")
            return None
        
        # 현금 부족 시 매수 무시
        if action == "BUY" and self.account["cash_balance"] < price:
            logger.info(f"Insufficient cash balance for {symbol}, ignoring BUY")
            return None
        
        # 유효한 결정일 경우 수량 계산 및 거래 결정 생성
        quantity = self._calculate_quantity(symbol, price, action, sell_portion)
        
        # 최소 거래 수량 확인
        if quantity <= 0:
            return None
        
        # 최종 거래 결정 생성
        decision = TradeDecision(
            symbol=symbol,
            action=action,
            confidence=confidence,
            price=price,
            quantity=quantity,
            timestamp=datetime.now(),
            meta={
                "ma20": indicators["MA20"],
                "upper_band": indicators["upperBand"],
                "lower_band": indicators["lowerBand"],
                "envelope_pct": self.chart_processor.envelope_percentage,
                "strategy": "envelope",
                "sell_portion": sell_portion
            }
        )
        
        return decision
    
    def _calculate_quantity(self, symbol: str, price: float, action: str, sell_portion: float = 1.0) -> int:
        """거래 수량 계산 - 포지션 관리 로직 반영"""
        # 매수인 경우: 총자산의 일정 비율로 매수 수량 계산
        if action == "BUY":
            # 총자산(현금 + 보유 주식 가치)의 5%를 투자
            total_asset = self.account["total_asset_value"]
            investment_amount = total_asset * self.position_size_pct
            
            # 현금 잔고 확인
            available_cash = self.account["cash_balance"]
            actual_investment = min(investment_amount, available_cash)
            
            # 수량 계산 (최소 1주)
            quantity = max(1, int(actual_investment / price))
            
            logger.info(f"Calculated BUY quantity for {symbol}: {quantity} shares at {price}")
            return quantity
        
        # 매도인 경우: 포지션 상태 기반 수량 계산
        elif action == "SELL" and symbol in self.account["positions"]:
            current_quantity = self.account["positions"][symbol]["quantity"]
            
            # 포지션 이력이 있으면 더 정확한 계산
            if symbol in self.position_history:
                position_status = self.position_history[symbol]["position_status"]
                initial_quantity = self.position_history[symbol]["initial_quantity"]
                
                # 상태별 매도 수량 계산
                if position_status == "FULL" and sell_portion == 0.5:
                    # 전량 보유 상태에서 절반 매도
                    sell_quantity = int(initial_quantity * 0.5)
                    logger.info(f"Selling HALF position for {symbol}: {sell_quantity}/{current_quantity} shares")
                elif position_status == "HALF" and sell_portion == 1.0:
                    # 절반 보유 상태에서 전량 매도 - 남은 수량 모두 매도
                    sell_quantity = current_quantity
                    logger.info(f"Selling remaining HALF position for {symbol}: {sell_quantity} shares")
                elif position_status == "FULL" and sell_portion == 1.0:
                    # 전량 보유 상태에서 전량 매도 - 특수 상황
                    sell_quantity = current_quantity
                    logger.info(f"Selling FULL position for {symbol}: {sell_quantity} shares (special case)")
                else:
                    # 기타 상황 - 일반적인 비율 계산
                    sell_quantity = int(current_quantity * sell_portion)
                    logger.info(f"General selling for {symbol}: {sell_quantity}/{current_quantity} shares, portion: {sell_portion}")
                
                return max(1, min(sell_quantity, current_quantity))  # 최소 1주, 최대 현재 보유량
            else:
                # 포지션 이력이 없는 경우 - 기본 계산
                sell_quantity = int(current_quantity * sell_portion)
                logger.info(f"Basic selling for {symbol}: {sell_quantity}/{current_quantity} shares, portion: {sell_portion}")
                return max(1, sell_quantity)  # 최소 1주 이상 매도
        
        # 그 외 경우
        return 0
    
    async def process_trade_result(self, symbol: str, action: str, quantity: int, price: float):
        """거래 결과 처리 및 포지션 이력 업데이트"""
        try:
            # 포지션 이력 업데이트
            if action == "BUY":
                # 매수 거래인 경우
                if symbol not in self.position_history:
                    # 새 포지션 추가
                    self.initial_position_sizes[symbol] = quantity
                    self.position_history[symbol] = {
                        "initial_quantity": quantity,
                        "current_quantity": quantity,
                        "position_status": "FULL",
                        "last_action": "BUY",
                        "timestamp": datetime.now().isoformat()
                    }
                else:
                    # 추가 매수
                    current_qty = self.position_history[symbol]["current_quantity"]
                    new_qty = current_qty + quantity
                    self.position_history[symbol]["current_quantity"] = new_qty
                    self.position_history[symbol]["last_action"] = "BUY"
                    self.position_history[symbol]["timestamp"] = datetime.now().isoformat()
                    
                    # 상태 업데이트 (추가 매수 시에는 보통 FULL로 간주)
                    self.position_history[symbol]["position_status"] = "FULL"
                
                logger.info(f"Updated position after BUY for {symbol}: {self.position_history[symbol]}")
                
            elif action == "SELL":
                # 매도 거래인 경우
                if symbol in self.position_history:
                    current_qty = self.position_history[symbol]["current_quantity"]
                    initial_qty = self.position_history[symbol]["initial_quantity"]
                    
                    # 현재 수량 업데이트
                    new_qty = max(0, current_qty - quantity)
                    self.position_history[symbol]["current_quantity"] = new_qty
                    self.position_history[symbol]["last_action"] = "SELL"
                    self.position_history[symbol]["timestamp"] = datetime.now().isoformat()
                    
                    # 포지션 상태 업데이트
                    if new_qty <= 0:
                        status = "NONE"  # 보유 없음
                    elif new_qty < initial_qty * 0.7:  # 여유 마진을 둠 (정확히 0.5가 아닐 수 있음)
                        status = "HALF"  # 절반 보유
                    else:
                        status = "FULL"  # 전량 보유
                    
                    self.position_history[symbol]["position_status"] = status
                    
                    logger.info(f"Updated position after SELL for {symbol}: {self.position_history[symbol]}")
            
            # 계좌 정보 업데이트 요청 (백엔드에서)
            await self.kiwoom_api.request_account_info()
                
            logger.info(f"Trade result processed for {symbol}: {action} {quantity} shares at {price}")
            
        except Exception as e:
            logger.error(f"Error processing trade result: {str(e)}")
    
    async def get_trade_decisions(self) -> List[TradeDecision]:
        """현재 대기 중인 모든 매매 결정 가져오기 (최소 신뢰도 이상)"""
        decisions = []
        
        # 모든 종목에 대한 매매 결정 수집
        for symbol, symbol_decisions in self.pending_decisions.items():
            if not symbol_decisions:
                continue
                
            # 신뢰도 기준 이상인 결정만 필터링
            filtered_decisions = [d for d in symbol_decisions if d.confidence >= settings.MIN_CONFIDENCE]
            
            if filtered_decisions:
                # 가장 최근(마지막) 결정 사용
                decisions.append(filtered_decisions[-1])
                
        # 대기 목록 초기화
        self.pending_decisions.clear()
        
        # 신뢰도 순으로 정렬 (내림차순)
        decisions.sort(key=lambda x: x.confidence, reverse=True)
        
        logger.info(f"Get {len(decisions)} trade decisions with confidence >= {settings.MIN_CONFIDENCE}")
        return decisions