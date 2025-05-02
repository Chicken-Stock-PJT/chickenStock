import logging
import asyncio
from typing import Dict, List, Any, Optional
from datetime import datetime, timedelta
import random

from app.envelope import ChartDataProcessor

logger = logging.getLogger(__name__)

class TradingModel:
    """Envelope 전략 기반 AI 트레이딩 모델"""
    
    def __init__(self, kiwoom_api):
        """Envelope 전략 트레이딩 모델 초기화"""
        self.kiwoom_api = kiwoom_api
        self.backend_client = None
        
        # 차트 데이터 처리기 초기화
        self.chart_processor = ChartDataProcessor()
        
        # 매매 의사결정 캐시
        self.decision_cache = {}
        
        # 매매 관련 설정
        self.max_positions = 10  # 최대 보유 종목 수
        self.trade_amount_per_stock = 1000000  # 종목당 매매 금액 (100만원)
        self.min_holding_period = 3  # 최소 보유 기간 (일)
        
        # 실행 상태
        self.is_running = False
        
        # 매수 후보 종목 목록 (매수 신호가 발생한 종목)
        self.buy_candidates = []
        
        # 매도 후보 종목 목록 (매도 신호가 발생한 종목)
        self.sell_candidates = []
        
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
        
        # 차트 데이터 및 Envelope 지표 초기화
        filtered_symbols = list(self.kiwoom_api.stock_cache.stock_info_cache.keys())
        if filtered_symbols:
            logger.info(f"Envelope 지표 계산 시작: {len(filtered_symbols)}개 종목")
            
            # Envelope 지표 계산 (시간이 오래 걸릴 수 있음)
            success_count = await self.chart_processor.preload_envelope_indicators(filtered_symbols, self.kiwoom_api)
            
            logger.info(f"Envelope 지표 계산 완료: {success_count}/{len(filtered_symbols)}개")
        else:
            logger.warning("필터링된 종목이 없습니다.")
        
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
        envelope = self.chart_processor.get_envelope_indicators(symbol, price)
        
        if not envelope:
            return
        
        # 매수/매도 신호 확인
        current_price = envelope.get("currentPrice", 0)
        upper_band = envelope.get("upperBand", 0)
        lower_band = envelope.get("lowerBand", 0)
        ma20 = envelope.get("MA20", 0)
        
        # 신호 저장
        if current_price >= upper_band:
            # 상단 밴드 터치 - 매도 신호
            if symbol not in self.sell_candidates:
                self.sell_candidates.append(symbol)
                logger.info(f"매도 신호 발생: {symbol}, 현재가: {current_price:.2f}, 상한선: {upper_band:.2f}")
        
        elif current_price <= lower_band:
            # 하단 밴드 터치 - 매수 신호
            if symbol not in self.buy_candidates:
                self.buy_candidates.append(symbol)
                logger.info(f"매수 신호 발생: {symbol}, 현재가: {current_price:.2f}, 하한선: {lower_band:.2f}")
    
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
                
                # 매수/매도 신호 로깅
                if self.buy_candidates:
                    logger.info(f"현재 매수 후보 종목: {len(self.buy_candidates)}개")
                    sample_candidates = self.buy_candidates[:5]  # 샘플 5개만 로깅
                    logger.info(f"매수 후보 샘플: {sample_candidates}")
                
                if self.sell_candidates:
                    logger.info(f"현재 매도 후보 종목: {len(self.sell_candidates)}개")
                    sample_candidates = self.sell_candidates[:5]  # 샘플 5개만 로깅
                    logger.info(f"매도 후보 샘플: {sample_candidates}")
                
            except Exception as e:
                logger.error(f"매매 신호 모니터링 중 오류: {str(e)}")
                await asyncio.sleep(30)  # 오류 발생 시 30초 대기
    
    async def get_trade_decisions(self) -> List[Dict[str, Any]]:
        """매매 의사결정 목록 반환"""
        if not self.is_running:
            logger.warning("트레이딩 모델이 실행 중이지 않습니다.")
            return []
        
        decisions = []
        try:
            # 계좌 정보 확인
            if not self.kiwoom_api.account_info:
                logger.warning("계좌 정보가 없습니다.")
                return []
            
            cash_balance = self.kiwoom_api.account_info.get("cash_balance", 0)
            positions = self.kiwoom_api.account_info.get("positions", {})
            
            # 매수 결정 처리
            if cash_balance > self.trade_amount_per_stock and len(positions) < self.max_positions and self.buy_candidates:
                # 매수 후보에서 종목 선택 (첫 번째 종목)
                symbol = self.buy_candidates[0]
                
                # 현재가 확인
                current_price = self.kiwoom_api.stock_cache.get_price(symbol)
                if not current_price or current_price <= 0:
                    logger.warning(f"종목 {symbol}의 현재가를 조회할 수 없습니다.")
                else:
                    # 매수 수량 계산 (종목당 거래 금액 기준)
                    quantity = int(self.trade_amount_per_stock / current_price)
                    
                    if quantity > 0:
                        # 매수 결정 추가
                        decision = {
                            "symbol": symbol,
                            "action": "buy",
                            "quantity": quantity,
                            "price": current_price,
                            "reason": "Envelope 하한선 터치",
                            "timestamp": datetime.now().isoformat()
                        }
                        decisions.append(decision)
                        
                        # 매수 후보에서 제거
                        self.buy_candidates.remove(symbol)
                        
                        logger.info(f"매수 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}")
            
            # 매도 결정 처리
            for symbol, position in positions.items():
                # 매도 후보에 있는 보유 종목만 처리
                if symbol in self.sell_candidates:
                    current_price = self.kiwoom_api.stock_cache.get_price(symbol)
                    quantity = position.get("quantity", 0)
                    
                    if current_price and quantity > 0:
                        # 매도 결정 추가
                        decision = {
                            "symbol": symbol,
                            "action": "sell",
                            "quantity": quantity,
                            "price": current_price,
                            "reason": "Envelope 상한선 터치",
                            "timestamp": datetime.now().isoformat()
                        }
                        decisions.append(decision)
                        
                        # 매도 후보에서 제거
                        self.sell_candidates.remove(symbol)
                        
                        logger.info(f"매도 결정: {symbol}, {quantity}주, 가격: {current_price:.2f}")
            
            # 매매 결정이 있는 경우 로깅
            if decisions:
                logger.info(f"매매 결정 생성: {len(decisions)}개")
            
            return decisions
        
        except Exception as e:
            logger.error(f"매매 의사결정 생성 중 오류: {str(e)}")
            return []