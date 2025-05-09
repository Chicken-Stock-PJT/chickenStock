import logging
import json
import os
from typing import Dict, Optional
from enum import Enum

from app.config import settings

logger = logging.getLogger(__name__)

class TradingStrategy(str, Enum):
    ENVELOPE = "envelope"
    BOLLINGER = "bollinger"

class AccountStrategyManager:
    """사용자 계정별 전략을 관리하는 클래스"""
    
    def __init__(self):
        """계정별 전략 매핑 초기화"""
        # 계정 이메일과 전략 간의 매핑
        self.account_strategy_map = {}
        
        # 설정 파일에서 계정별 전략 로드
        self._load_from_settings()
        
        # 기본 전략 설정
        self.default_strategy = TradingStrategy(settings.DEFAULT_STRATEGY)
        
        logger.info(f"계정별 전략 매핑 관리자 초기화 완료 ({len(self.account_strategy_map)}개 계정 로드됨)")
    
    def _load_from_settings(self):
        """설정에서 계정별 전략 설정 로드"""
        try:
            # settings.ACCOUNT_STRATEGIES에서 계정별 전략 로드
            for email, strategy_str in settings.ACCOUNT_STRATEGIES.items():
                try:
                    # 문자열을 TradingStrategy Enum으로 변환
                    strategy = TradingStrategy(strategy_str.lower())
                    self.account_strategy_map[email] = strategy
                except ValueError:
                    logger.warning(f"잘못된 전략 유형: {strategy_str} (계정: {email})")
        except Exception as e:
            logger.error(f"설정에서 계정별 전략 로드 중 오류: {str(e)}")
    
    def _save_to_file(self):
        """계정별 전략 설정을 파일로 저장"""
        try:
            # Enum을 문자열로 변환하여 저장
            serializable_map = {
                email: strategy.value 
                for email, strategy in self.account_strategy_map.items()
            }
            
            # 설정 파일 경로 가져오기
            config_file = os.getenv('ACCOUNT_STRATEGIES_FILE', 'account_strategies.json')
            
            # JSON 형식으로 파일에 저장
            with open(config_file, 'w') as f:
                json.dump(serializable_map, f, indent=2)
                
            logger.info(f"계정별 전략 설정 저장 완료: {len(serializable_map)}개 계정")
            return True
        except Exception as e:
            logger.error(f"계정별 전략 설정 저장 중 오류: {str(e)}")
            return False
    
    def get_strategy_for_account(self, email: str) -> TradingStrategy:
        """특정 계정에 할당된 전략 조회"""
        # 계정별 전략 매핑에서 찾기 (없으면 기본 전략 반환)
        return self.account_strategy_map.get(email, self.default_strategy)
    
    def register_account_strategy(self, email: str, strategy: TradingStrategy) -> bool:
        """새로운 계정과 전략 등록"""
        try:
            self.account_strategy_map[email] = strategy
            logger.info(f"새로운 계정 전략 등록: {email} -> {strategy}")
            
            # 파일에 저장
            self._save_to_file()
            return True
        except Exception as e:
            logger.error(f"계정 전략 등록 중 오류: {str(e)}")
            return False
    
    def update_account_strategy(self, email: str, strategy: TradingStrategy) -> bool:
        """기존 계정의 전략 업데이트"""
        try:
            if email in self.account_strategy_map:
                old_strategy = self.account_strategy_map[email]
                self.account_strategy_map[email] = strategy
                logger.info(f"계정 전략 업데이트: {email} ({old_strategy} -> {strategy})")
            else:
                # 계정이 없으면 새로 등록
                logger.info(f"새로운 계정 전략 등록: {email} -> {strategy}")
                self.account_strategy_map[email] = strategy
            
            # 파일에 저장
            self._save_to_file()
            return True
        except Exception as e:
            logger.error(f"계정 전략 업데이트 중 오류: {str(e)}")
            return False
    
    def get_all_account_strategies(self) -> Dict[str, TradingStrategy]:
        """모든 계정과 전략 매핑 조회"""
        return self.account_strategy_map
    
    def remove_account_strategy(self, email: str) -> bool:
        """계정별 전략 설정 제거"""
        try:
            if email in self.account_strategy_map:
                del self.account_strategy_map[email]
                logger.info(f"계정 전략 제거: {email}")
                
                # 파일에 저장
                self._save_to_file()
                return True
            else:
                logger.warning(f"제거할 계정을 찾을 수 없음: {email}")
                return False
        except Exception as e:
            logger.error(f"계정 전략 제거 중 오류: {str(e)}")
            return False