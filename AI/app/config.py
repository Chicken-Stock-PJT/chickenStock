import os
from typing import Dict
from pydantic_settings import BaseSettings

class Settings(BaseSettings):
    # API 설정
    API_BASE_URL: str = "https://api.kiwoom.com"  # 키움증권 API 기본 URL
    BACKEND_API_URL: str = "https://k12a106.p.ssafy.io"  # 백엔드 API URL
    WEBSOCKET_API_URL: str = "wss://api.kiwoom.com:10000"
    
    # 인증 설정
    KIWOOM_API_KEY: str = os.getenv('KIWOOM_API_KEY', '')  # appkey
    KIWOOM_API_SECRET: str = os.getenv('KIWOOM_API_SECRET', '')  # secretkey
    
    # 토큰 관리 설정
    TOKEN_REFRESH_MARGIN: int = 300  # 토큰 만료 5분(300초) 전에 갱신
    TOKEN_REFRESH_INTERVAL: int = 3600  # 토큰 갱신 검사 간격(최대 1시간)

    # 계정별 기본 전략 설정 - 설정 파일이나 환경 변수로 관리
    # 환경 변수 예시: ACCOUNT_STRATEGIES='{"user1@example.com":"envelope","user2@example.com":"bollinger"}'
    ACCOUNT_STRATEGIES: Dict[str, str] = {}
    
    # 기본 계정 정보 추가
    DEFAULT_EMAIL: str = ""  # 기본 계정 이메일 입력
    DEFAULT_PASSWORD: str = ""  # 기본 계정 비밀번호 입력
    
    # 기본 전략 설정
    DEFAULT_STRATEGY: str = "ENVELOPE"  # 'ENVELOPE' 또는 'BOLLINGER'
    
    # Envelope 전략 설정
    ENVELOPE_MAX_POSITIONS: int = 20  # 최대 보유 종목 수
    ENVELOPE_TRADE_AMOUNT: int = 5000000  # 종목당 매매 금액 (500만원)
    ENVELOPE_MIN_HOLDING_PERIOD: int = 1  # 최소 보유 기간 (일)
    ENVELOPE_PERCENTAGE: float = 0.2  # Envelope 밴드 비율 (20%)
    
    # 볼린저 밴드 전략 설정
    BOLLINGER_MAX_POSITIONS: int = 15  # 최대 보유 종목 수
    BOLLINGER_TRADE_AMOUNT: int = 6000000  # 종목당 매매 금액 (600만원)
    BOLLINGER_MIN_HOLDING_PERIOD: int = 1  # 최소 보유 기간 (일)
    BOLLINGER_PERIOD: int = 20  # 볼린저 밴드 기간
    BOLLINGER_STD_DEV: float = 2.0  # 볼린저 밴드 표준편차 승수

    # 주식 거래 설정
    MIN_CONFIDENCE: float = 0.6  # 최소 신뢰도 (60% 이상일 때만 거래)
    MAX_POSITIONS: int = 20  # 최대 보유 종목 수
    POSITION_SIZE_PCT: float = 0.05  # 총 자산의 5%를 투자
    
    # 로깅 설정
    LOG_LEVEL: str = "INFO"
    
    # HTTP 요청 설정
    REQUEST_TIMEOUT: int = 30  # HTTP 요청 타임아웃(초)
    MAX_RETRIES: int = 3  # 최대 재시도 횟수
    
    # 디버그 모드 (장 시간 외에도 동작)
    DEBUG_MODE: bool = False
    
    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"
        extra = 'ignore'

settings = Settings()

# 설정 파일이나 환경 변수에서 계정별 전략 설정 로드
def load_account_strategies():
    """계정별 전략 설정 로드 (환경 변수 또는 설정 파일에서)"""
    import json
    import os
    
    # 환경 변수에서 계정별 전략 설정 가져오기
    account_strategies_env = os.getenv('ACCOUNT_STRATEGIES')
    if account_strategies_env:
        try:
            # JSON 문자열을 딕셔너리로 변환
            strategies = json.loads(account_strategies_env)
            settings.ACCOUNT_STRATEGIES.update(strategies)
        except json.JSONDecodeError:
            print("ACCOUNT_STRATEGIES 환경 변수 형식이 잘못되었습니다. JSON 형식이어야 합니다.")
    
    # 설정 파일에서 계정별 전략 설정 가져오기 (파일이 존재하는 경우)
    config_file = os.getenv('ACCOUNT_STRATEGIES_FILE', 'account_strategies.json')
    if os.path.exists(config_file):
        try:
            with open(config_file, 'r') as f:
                strategies = json.load(f)
                settings.ACCOUNT_STRATEGIES.update(strategies)
        except (json.JSONDecodeError, IOError) as e:
            print(f"계정별 전략 설정 파일 로드 중 오류: {str(e)}")

# 설정 로드 실행
load_account_strategies()