import logging
import asyncio
from typing import Dict, List, Any, Optional
from datetime import datetime

logger = logging.getLogger(__name__)

class TradingModel:
    """Envelope 전략 기반 AI 트레이딩 모델"""
    
    def __init__(self, kiwoom_api):
        """Envelope 전략 트레이딩 모델 초기화"""
        self.kiwoom_api = kiwoom_api
        self.backend_client = None
        
        # 매매 관련 설정
        self.max_positions = 20  # 최대 보유 종목 수
        self.trade_amount_per_stock = 5000000  # 종목당 매매 금액 (100만원)
        self.min_holding_period = 1  # 최소 보유 기간 (일)
        
        # 실행 상태
        self.is_running = False
        
        # 매매 신호 저장 딕셔너리
        self.trading_signals = {}  # {symbol: {"signal": "lower", "price": price, "timestamp": datetime}}
        
        # 거래 이력 저장
        self.trade_history = {}  # {symbol: {"last_trade": datetime, "last_action": "buy"}}
        
        logger.info("AI 트레이딩 모델 초기화 완료")
    
    def set_backend_client(self, backend_client):
        """백엔드 클라이언트 설정"""
        self.backend_client = backend_client
    
    async def start(self):
        """트레이딩 모델 시작"""
        if self.is_running:
            logger.warning("트레이딩 모델이 이미 실행 중입니다.")
            return
        
        self.is_running = True
        logger.info("트레이딩 모델 시작")
        
        # 매매 신호 모니터링 시작
        asyncio.create_task(self.monitor_signals())
    
    async def stop(self):
        """트레이딩 모델 중지"""
        self.is_running = False
        logger.info("트레이딩 모델 중지")
    
    async def handle_realtime_price(self, symbol, price):
        """실시간 가격 데이터 처리"""
        if not self.is_running:
            return
        
        # Envelope 지표 가져오기 (현재가 업데이트)
        envelope = self.kiwoom_api.stock_cache.get_envelope_indicators(symbol, price)
        current_price = self.kiwoom_api.stock_cache.get_price(symbol)

        if not envelope:
            return
        
        # 밴드 정보 확인
        upper_band = envelope.get("upperBand", 0)
        middle_band = envelope.get("middleBand", 0)
        lower_band = envelope.get("lowerBand", 0)
        
        # 현재 시간
        now = datetime.now()
        
        # 상한선 (전량 매도 신호)
        if current_price >= upper_band:
            self.trading_signals[symbol] = {
                "signal": "upper",
                "price": current_price,
                "timestamp": now,
                "bands": {
                    "upper": upper_band,
                    "middle": middle_band,
                    "lower": lower_band
                }
            }
            logger.info(f"상한선 신호 발생: {symbol}, 현재가: {current_price:.2f}, 상한선: {upper_band:.2f}")
        
        # 중앙선 (절반 매도 신호) - 이동평균선
        # 중앙선보다 높고 상한선보다 낮을 때
        elif current_price >= middle_band and current_price < upper_band:
            self.trading_signals[symbol] = {
                "signal": "middle",
                "price": current_price,
                "timestamp": now,
                "bands": {
                    "upper": upper_band,
                    "middle": middle_band,
                    "lower": lower_band
                }
            }
            logger.info(f"중앙선(이동평균선) 신호 발생: {symbol}, 현재가: {current_price:.2f}, 중앙선: {middle_band:.2f}")
        
        # 하한선 (매수 신호)
        elif current_price <= lower_band:
            self.trading_signals[symbol] = {
                "signal": "lower",
                "price": current_price,
                "timestamp": now,
                "bands": {
                    "upper": upper_band,
                    "middle": middle_band,
                    "lower": lower_band
                }
            }
            logger.info(f"하한선 신호 발생: {symbol}, 현재가: {current_price:.2f}, 하한선: {lower_band:.2f}")
    
    async def monitor_signals(self):
        """매매 신호 주기적 모니터링 및 처리"""
        logger.info("매매 신호 모니터링 시작")
        
        while self.is_running:
            try:
                # 1분마다 매매 신호 확인
                await asyncio.sleep(60)
                
                # 계좌 정보 확인
                if not self.kiwoom_api.account_info:
                    logger.warning("계좌 정보가 없습니다.")
                    continue
                
                # 매매 신호 로깅
                if self.trading_signals:
                    signal_counts = {
                        "lower": sum(1 for v in self.trading_signals.values() if v["signal"] == "lower"),
                        "middle": sum(1 for v in self.trading_signals.values() if v["signal"] == "middle"),
                        "upper": sum(1 for v in self.trading_signals.values() if v["signal"] == "upper")
                    }
                    logger.info(f"현재 매매 신호: 하한선(매수)={signal_counts['lower']}개, " +
                              f"중앙선(절반매도)={signal_counts['middle']}개, " +
                              f"상한선(전량매도)={signal_counts['upper']}개")
                
            except Exception as e:
                logger.error(f"매매 신호 모니터링 중 오류: {str(e)}")
                await asyncio.sleep(30)
    
    async def get_trade_decisions(self) -> List[Dict[str, Any]]:
        """매매 의사결정 목록 반환"""
        if not self.is_running:
            logger.warning("트레이딩 모델이 실행 중이지 않습니다.")
            return []
        
        decisions = []
        try:
            # 계좌 정보 최신화 (백엔드에서 가져옴)
            if self.backend_client:
                account_info = await self.backend_client.request_account_info()
                if account_info:
                    self.kiwoom_api.update_account_info(account_info)
            
            # 계좌 정보 확인
            if not self.kiwoom_api.account_info:
                logger.warning("계좌 정보가 없습니다.")
                return []
            
            cash_balance = self.kiwoom_api.account_info.get("cash_balance", 0)
            positions = self.kiwoom_api.account_info.get("positions", {})
            
            # 현재 시간
            now = datetime.now()
            
            # 매매 신호에 따른 거래 결정 처리
            for symbol, signal_info in list(self.trading_signals.items()):
                signal = signal_info["signal"]
                price = signal_info["price"]
                timestamp = signal_info["timestamp"]
                
                # 신호 유효 시간 (10분) 체크
                if (now - timestamp).total_seconds() > 600:
                    # 오래된 신호 제거
                    del self.trading_signals[symbol]
                    continue
                
                # 1. 하한선 매수 신호 처리
                if signal == "lower":
                    # 현재 보유 종목 수 확인
                    if len(positions) >= self.max_positions:
                        continue
                    
                    # 충분한 현금 확인
                    if cash_balance < self.trade_amount_per_stock:
                        continue
                    
                    # 이미 보유 중인지 확인
                    if symbol in positions:
                        continue
                    
                    # 매수 수량 계산 (종목당 비중에 맞게)
                    current_price = self.kiwoom_api.stock_cache.get_price(symbol)
                    if not current_price or current_price <= 0:
                        continue
                    
                    quantity = int(self.trade_amount_per_stock / current_price)
                    if quantity <= 0:
                        continue
                    
                    # 매수 결정 추가
                    decision = {
                        "symbol": symbol,
                        "action": "buy",
                        "quantity": quantity,
                        "price": current_price,
                        "reason": "Envelope 하한선 터치 - 매수",
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
                    logger.info(f"매수 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}")
                
                # 2. 중앙선 절반 매도 신호 처리
                elif signal == "middle":
                    # 보유 중인 종목인지 확인
                    if symbol not in positions:
                        del self.trading_signals[symbol]
                        continue
                    
                    position = positions[symbol]
                    total_quantity = position.get("quantity", 0)
                    
                    # 절반 수량 계산 (최소 1주)
                    sell_quantity = max(1, int(total_quantity / 2))
                    
                    if sell_quantity <= 0:
                        continue
                    
                    # 매도 결정 추가
                    current_price = self.kiwoom_api.stock_cache.get_price(symbol)
                    decision = {
                        "symbol": symbol,
                        "action": "sell",
                        "quantity": sell_quantity,
                        "price": current_price,
                        "reason": "Envelope 중앙선 터치 - 절반 매도",
                        "timestamp": now.isoformat()
                    }
                    decisions.append(decision)
                    
                    # 거래 이력 업데이트
                    self.trade_history[symbol] = {
                        "last_trade": now,
                        "last_action": "sell_half"
                    }
                    
                    # 신호 제거
                    del self.trading_signals[symbol]
                    logger.info(f"절반 매도 결정: {symbol}, {sell_quantity}주, 가격: {current_price:.2f}")
                
                # 3. 상한선 전량 매도 신호 처리
                elif signal == "upper":
                    # 보유 중인 종목인지 확인
                    if symbol not in positions:
                        del self.trading_signals[symbol]
                        continue
                    
                    position = positions[symbol]
                    total_quantity = position.get("quantity", 0)
                    
                    if total_quantity <= 0:
                        continue
                    
                    # 매도 결정 추가 (전량)
                    current_price = self.kiwoom_api.stock_cache.get_price(symbol)
                    decision = {
                        "symbol": symbol,
                        "action": "sell",
                        "quantity": total_quantity,
                        "price": current_price,
                        "reason": "Envelope 상한선 터치 - 전량 매도",
                        "timestamp": now.isoformat()
                    }
                    decisions.append(decision)
                    
                    # 거래 이력 업데이트
                    self.trade_history[symbol] = {
                        "last_trade": now,
                        "last_action": "sell_all"
                    }
                    
                    # 신호 제거
                    del self.trading_signals[symbol]
                    logger.info(f"전량 매도 결정: {symbol}, {total_quantity}주, 가격: {current_price:.2f}")
            
            return decisions
        
        except Exception as e:
            logger.error(f"매매 의사결정 생성 중 오류: {str(e)}")
            return []