import logging
import asyncio
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any

from app.config import settings
from app.models import TradeDecision
from app.envelope import ChartDataProcessor

logger = logging.getLogger(__name__)

class TradingModel:
    """Envelope 전략 기반 자동매매 모델"""
    
    def __init__(self, kiwoom_api):
        """모델 초기화"""
        self.kiwoom_api = kiwoom_api
        
        # Envelope 지표 처리기
        self.chart_processor = ChartDataProcessor()
        
        # 계좌 정보
        self.account = {
            "cash_balance": 0,
            "positions": {},
            "total_asset_value": 0
        }
        
        # 매매 파라미터
        self.position_size_pct = settings.POSITION_SIZE_PCT
        self.max_positions = settings.MAX_POSITIONS
        
        # 실시간 가격 데이터
        self.realtime_prices = {}  # 종목 코드 -> {price, name, timestamp}
        
        # 거래 관리
        self.pending_decisions = {}  # 종목 코드 -> 매매 결정
        self.trade_history = {}  # 종목 코드 -> 매매 이력
        
        # 모델 상태
        self.running = False
    
    async def start(self):
        """트레이딩 모델 시작"""
        self.account = self.kiwoom_api.account_info
        self.running = True
        logger.info("트레이딩 모델 시작됨")
        return True
    
    async def stop(self):
        """트레이딩 모델 정지"""
        self.running = False
        logger.info("트레이딩 모델 정지됨")
        return True
    
    async def initialize_chart_data(self, symbols: List[str]):
        """모든 종목의 Envelope 지표 계산 및 캐싱"""
        logger.info(f"{len(symbols)}개 종목에 대한 Envelope 지표 초기화 시작")
        
        # ChartDataProcessor를 통해 Envelope 지표 미리 계산
        if not self.chart_processor.is_cache_valid():
            await self.chart_processor.preload_envelope_indicators(symbols, self.kiwoom_api)
            logger.info("Envelope 지표 캐시 업데이트 완료")
        else:
            logger.info("기존 Envelope 지표 캐시 사용")
    
    def handle_realtime_price(self, code: str, price: int, stock_name: str = ""):
        """실시간 가격 데이터 처리"""
        try:
            # 가격 데이터 캐싱
            self.realtime_prices[code] = {
                "price": price,
                "name": stock_name,
                "timestamp": datetime.now()
            }
            
            # 거래 이력 확인 (동일 종목 하루 1회 제한)
            if code in self.trade_history:
                last_trade_date = self.trade_history[code].get("date")
                if last_trade_date and last_trade_date.date() == datetime.now().date():
                    return  # 오늘 이미 거래한 종목은 스킵
            
            # 지표와 현재가 비교하여 거래 결정 생성
            self._create_trade_decision(code, price)
        
        except Exception as e:
            logger.error(f"실시간 가격 처리 중 오류 ({code}): {str(e)}")
    
    def _create_trade_decision(self, code: str, price: float):
        """거래 결정 생성"""
        try:
            # Envelope 지표 조회 (ChartDataProcessor의 캐시 사용)
            indicators = self.chart_processor.get_envelope_indicators(code, price)
            if not indicators:
                return
            
            # 필요한 값 추출
            ma20 = indicators.get("MA20", 0)
            upper_band = indicators.get("upperBand", 0)
            lower_band = indicators.get("lowerBand", 0)
            
            # 현재 보유 포지션 확인
            has_position = code in self.account.get("positions", {})
            
            # 거래 결정
            action = None
            quantity = 0
            reason = ""
            
            # 매수 신호: 현재가가 하단 밴드 아래에 있고 포지션이 없는 경우
            if price <= lower_band and not has_position:
                action = "buy"
                # 자금의 설정된 비율 이내로 매수
                available_cash = self.account.get("cash_balance", 0)
                investment_amount = min(available_cash * self.position_size_pct, available_cash)
                quantity = max(1, int(investment_amount / price))  # 최소 1주
                reason = "하단 밴드 터치"
                
                logger.info(f"매수 신호 감지: {code}, 가격: {price}, 하단밴드: {lower_band}, 수량: {quantity}")
            
            # 매도 신호: 현재가가 상단 밴드 위에 있고 포지션이 있는 경우
            elif price >= upper_band and has_position:
                action = "sell"
                quantity = self.account["positions"][code].get("quantity", 0)
                reason = "상단 밴드 터치"
                
                logger.info(f"매도 신호 감지: {code}, 가격: {price}, 상단밴드: {upper_band}, 수량: {quantity}")
            
            # 거래 결정이 있으면 저장
            if action and quantity > 0:
                self.pending_decisions[code] = {
                    "symbol": code,
                    "action": action,
                    "quantity": quantity,
                    "price": price,
                    "price_type": "market",
                    "reason": reason,
                    "timestamp": datetime.now()
                }
        
        except Exception as e:
            logger.error(f"거래 결정 생성 중 오류 ({code}): {str(e)}")
    
    async def get_trade_decisions(self) -> List[Dict]:
        """현재 대기 중인 모든 매매 결정 가져오기"""
        if not self.running:
            return []
        
        decisions = []
        
        try:
            # 계좌 정보 업데이트
            self.account = self.kiwoom_api.account_info
            
            # 포지션 수 제한 확인
            current_positions = len(self.account.get("positions", {}))
            
            # 모든 대기 중인 결정을 처리
            for code, decision in list(self.pending_decisions.items()):
                # 매수인데 최대 포지션 수에 도달한 경우 스킵
                if decision["action"] == "buy" and current_positions >= self.max_positions:
                    logger.info(f"최대 포지션 수 도달: {code} 매수 결정 무시")
                    continue
                
                # 종목명 추가
                decision["name"] = self.kiwoom_api.stock_cache.get_stock_name(code)
                
                # 결정 추가
                decisions.append(decision)
                
                # 거래 이력 업데이트 (당일 중복 거래 방지)
                self.trade_history[code] = {
                    "action": decision["action"],
                    "price": decision["price"],
                    "quantity": decision["quantity"],
                    "date": datetime.now()
                }
            
            # 대기 목록 초기화
            self.pending_decisions.clear()
            
            logger.info(f"{len(decisions)}개 매매 결정 반환")
            return decisions
        
        except Exception as e:
            logger.error(f"매매 결정 처리 중 오류: {str(e)}")
            return []
    
    async def process_trade_result(self, symbol: str, action: str, quantity: int, price: float):
        """거래 결과 처리"""
        try:
            # 거래 이력 업데이트
            self.trade_history[symbol] = {
                "action": action.lower(),
                "price": price,
                "quantity": quantity,
                "date": datetime.now()
            }
            
            logger.info(f"거래 결과 처리: {symbol} {action} {quantity}주 @ {price}")
            
            # 계좌 정보 업데이트 요청
            await self.kiwoom_api.request_account_info()
        
        except Exception as e:
            logger.error(f"거래 결과 처리 중 오류: {str(e)}")