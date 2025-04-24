import os
import sys
import logging
import asyncio
from typing import Dict, List, Set, Callable, Optional, Any
from datetime import datetime
from PyQt5.QAxContainer import QAxWidget
from PyQt5.QtCore import QEventLoop, QObject, pyqtSignal, QTimer

from .models import TradeDecision
from .config import settings

logger = logging.getLogger(__name__)

class KiwoomAPI(QObject):
    # 이벤트 시그널 정의
    on_event_connect = pyqtSignal(int)
    on_receive_tr_data = pyqtSignal(str, str, str, str, str, int, str, str, str)
    on_receive_real_data = pyqtSignal(str, str, str)
    on_receive_chejan_data = pyqtSignal(str, int, str)
    on_receive_msg = pyqtSignal(str, str, str, str)
    on_receive_condition_ver = pyqtSignal(int, str)
    on_receive_real_condition = pyqtSignal(str, str, str, str)
    on_receive_tr_condition = pyqtSignal(str, str, str, int, int)

    def __init__(self):
        """키움 OPEN API 연결 및 초기화"""
        super().__init__()
        
        # 키움 OPEN API 컨트롤 생성
        self.kiwoom = QAxWidget("KHOPENAPI.KHOpenAPICtrl.1")
        
        # 이벤트 핸들러 연결
        self.kiwoom.OnEventConnect.connect(self._event_connect)
        self.kiwoom.OnReceiveTrData.connect(self._receive_tr_data)
        self.kiwoom.OnReceiveRealData.connect(self._receive_real_data)
        self.kiwoom.OnReceiveChejanData.connect(self._receive_chejan_data)
        self.kiwoom.OnReceiveMsg.connect(self._receive_msg)
        self.kiwoom.OnReceiveConditionVer.connect(self._receive_condition_ver)
        self.kiwoom.OnReceiveRealCondition.connect(self._receive_real_condition)
        self.kiwoom.OnReceiveTrCondition.connect(self._receive_tr_condition)
        
        # 이벤트 루프 설정
        self.loop = QEventLoop()
        
        # 로그인 상태
        self.connected = False
        
        # 계좌 정보
        self.account_number = ""
        self.account_count = 0
        self.account_list = []
        
        # 종목 관련 데이터
        self.kospi_symbols = []
        self.kosdaq_symbols = []
        self.realtime_prices = {}
        self.subscribed_symbols = set()
        
        # 실시간 시세 콜백 함수
        self.real_data_callback = None
        
        # 매매 관련 정보
        self.account_info = {
            "cash_balance": 0,
            "positions": {},
            "total_asset_value": 0
        }
    
    async def connect(self) -> bool:
        """API 연결 및 로그인"""
        # 키움 로그인 창 실행
        self.on_event_connect.connect(self._handle_login)
        login_result = self.kiwoom.dynamicCall("CommConnect()")
        
        # 로그인 대기
        self.loop.exec_()
        
        if not self.connected:
            logger.error("로그인 실패")
            return False
        
        # 계좌 정보 가져오기
        self._get_account_info()
        
        # 시장 데이터 초기화
        self._init_market_data()
        
        logger.info("키움 API 연결 완료")
        return True
    
    def _handle_login(self, err_code):
        """로그인 이벤트 처리"""
        if err_code == 0:
            self.connected = True
            logger.info("로그인 성공")
        else:
            self.connected = False
            logger.error(f"로그인 실패: 에러 코드 {err_code}")
        
        self.on_event_connect.disconnect(self._handle_login)
        self.loop.exit()
    
    def _get_account_info(self):
        """계좌 정보 가져오기"""
        self.account_count = int(self.kiwoom.dynamicCall("GetLoginInfo(QString)", ["ACCOUNT_CNT"]))
        accounts = self.kiwoom.dynamicCall("GetLoginInfo(QString)", ["ACCLIST"]).split(';')
        self.account_list = [acc for acc in accounts if acc]
        
        if self.account_list:
            self.account_number = self.account_list[0]  # 첫 번째 계좌 선택
            logger.info(f"계좌번호: {self.account_number}")
    
    def _init_market_data(self):
        """장 시작 시 필요한 데이터 초기화"""
        try:
            # 코스피, 코스닥 종목 리스트 가져오기
            self._get_stock_list()
            
        except Exception as e:
            logger.error(f"시장 데이터 초기화 중 오류: {str(e)}")
    
    def _get_stock_list(self):
        """종목 리스트 가져오기"""
        # 코스피 종목 리스트
        kospi_codes = self.kiwoom.dynamicCall("GetCodeListByMarket(QString)", ["0"])
        self.kospi_symbols = kospi_codes.split(';')
        
        # 코스닥 종목 리스트
        kosdaq_codes = self.kiwoom.dynamicCall("GetCodeListByMarket(QString)", ["10"])
        self.kosdaq_symbols = kosdaq_codes.split(';')
        
        # 빈 문자열 제거
        self.kospi_symbols = [code for code in self.kospi_symbols if code]
        self.kosdaq_symbols = [code for code in self.kosdaq_symbols if code]
        
        # 시가총액 상위 종목만 필터링 (별도 함수로 구현 필요)
        self._filter_top_stocks()
        
        logger.info(f"코스피 종목 수: {len(self.kospi_symbols)}, 코스닥 종목 수: {len(self.kosdaq_symbols)}")
    
    def _filter_top_stocks(self):
        """시가총액 상위 종목 필터링"""
        # 코스피 시가총액 상위 450개, 코스닥 시가총액 상위 150개 필터링
        # 시가총액 정보를 가져오기 위해 TR 요청 필요
        if settings.DEBUG_MODE:
            # 디버그 모드에서는 종목 수 제한
            self.kospi_symbols = self.kospi_symbols[:30]
            self.kosdaq_symbols = self.kosdaq_symbols[:10]
            return
        
        # 실제 구현에서는 TR 요청으로 시가총액 정보 조회 후 정렬
        # 임시로 현재 리스트에서 450개, 150개만 사용
        if len(self.kospi_symbols) > 450:
            self.kospi_symbols = self.kospi_symbols[:450]
        
        if len(self.kosdaq_symbols) > 150:
            self.kosdaq_symbols = self.kosdaq_symbols[:150]
    
    def get_stock_name(self, code):
        """종목 코드로 종목명 조회"""
        return self.kiwoom.dynamicCall("GetMasterCodeName(QString)", [code])
    
    def subscribe_realtime_data(self, codes: List[str], callback: Callable = None):
        """실시간 데이터 구독"""
        # 이미 구독 중인 종목은 제외
        new_codes = [code for code in codes if code not in self.subscribed_symbols]
        
        if not new_codes:
            return
        
        # 콜백 함수 설정
        if callback:
            self.real_data_callback = callback
        
        # 실시간 데이터 등록
        codes_str = ";".join(new_codes)
        self.kiwoom.dynamicCall("SetRealReg(QString, QString, QString, QString)", 
                               ["0101", codes_str, "10", "0"])
        
        # 구독 종목 추가
        self.subscribed_symbols.update(new_codes)
        logger.info(f"{len(new_codes)}개 종목 실시간 시세 구독 추가, 총 {len(self.subscribed_symbols)}개 구독 중")
    
    def unsubscribe_realtime_data(self, codes: List[str] = None):
        """실시간 데이터 구독 해제"""
        if codes is None:
            # 전체 구독 해제
            self.kiwoom.dynamicCall("SetRealRemove(QString, QString)", ["ALL", "ALL"])
            self.subscribed_symbols.clear()
            logger.info("모든 종목 실시간 시세 구독 해제")
        else:
            # 특정 종목만 구독 해제
            for code in codes:
                if code in self.subscribed_symbols:
                    self.kiwoom.dynamicCall("SetRealRemove(QString, QString)", ["0101", code])
                    self.subscribed_symbols.remove(code)
            logger.info(f"{len(codes)}개 종목 실시간 시세 구독 해제")
    
    def _receive_real_data(self, code, real_type, real_data):
        """실시간 데이터 수신 이벤트 처리"""
        try:
            # 주식 시세 데이터인 경우
            if real_type == "주식시세":
                # 현재가 조회
                price = abs(int(self.kiwoom.dynamicCall("GetCommRealData(QString, int)", [code, 10])))
                
                # 실시간 가격 데이터 업데이트
                if code in self.realtime_prices:
                    self.realtime_prices[code]["price"] = price
                else:
                    self.realtime_prices[code] = {
                        "symbol": code,
                        "name": self.get_stock_name(code),
                        "price": price,
                        "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                    }
                
                # 콜백 함수 호출
                if self.real_data_callback:
                    self.real_data_callback(code, price)
            
            # 이벤트 시그널 발생
            self.on_receive_real_data.emit(code, real_type, real_data)
            
        except Exception as e:
            logger.error(f"실시간 데이터 처리 중 오류: {str(e)}")
    
    def _receive_tr_data(self, screen_no, rqname, trcode, record_name, next, unused1, unused2, unused3, unused4):
        """TR 데이터 수신 이벤트 처리"""
        try:
            # TR별 처리 함수 호출
            if rqname == "계좌평가현황요청":
                self._handle_account_evaluation(trcode, rqname, next)
            elif rqname == "주식기본정보요청":
                self._handle_stock_basic_info(trcode, rqname)
            
            # 이벤트 시그널 발생
            self.on_receive_tr_data.emit(screen_no, rqname, trcode, record_name, next, unused1, unused2, unused3, unused4)
            
        except Exception as e:
            logger.error(f"TR 데이터 처리 중 오류: {str(e)}")
        finally:
            self.loop.exit()
    
    def _receive_chejan_data(self, gubun, item_cnt, fid_list):
        """체결 데이터 수신 이벤트 처리"""
        try:
            # 주문/체결 정보 처리
            if gubun == "0":  # 주문/체결
                order_number = self.kiwoom.dynamicCall("GetChejanData(int)", [9203])  # 주문번호
                code = self.kiwoom.dynamicCall("GetChejanData(int)", [9001])  # 종목코드
                code = code.strip()[1:]  # 'A' 제거
                name = self.kiwoom.dynamicCall("GetChejanData(int)", [302])  # 종목명
                order_status = self.kiwoom.dynamicCall("GetChejanData(int)", [913])  # 주문상태
                quantity = int(self.kiwoom.dynamicCall("GetChejanData(int)", [900]))  # 주문수량
                price = abs(int(self.kiwoom.dynamicCall("GetChejanData(int)", [901])))  # 주문가격
                
                logger.info(f"체결 알림: {name}({code}) {order_status} - {quantity}주 {price}원")
                
                # 계좌 정보 갱신
                self.request_account_info()
            
            # 이벤트 시그널 발생
            self.on_receive_chejan_data.emit(gubun, item_cnt, fid_list)
            
        except Exception as e:
            logger.error(f"체결 데이터 처리 중 오류: {str(e)}")
    
    def _receive_msg(self, screen_no, rqname, trcode, msg):
        """메시지 수신 이벤트 처리"""
        logger.info(f"메시지 수신: [{screen_no}] {rqname} - {msg}")
        self.on_receive_msg.emit(screen_no, rqname, trcode, msg)
    
    def _receive_condition_ver(self, ret, msg):
        """조건검색 조건식 수신 이벤트 처리"""
        self.on_receive_condition_ver.emit(ret, msg)
        self.loop.exit()
    
    def _receive_real_condition(self, code, type_name, condition_name, condition_index):
        """조건검색 실시간 편입/이탈 이벤트 처리"""
        self.on_receive_real_condition.emit(code, type_name, condition_name, condition_index)
    
    def _receive_tr_condition(self, screen_no, code_list, condition_name, condition_index, next):
        """조건검색 TR 수신 이벤트 처리"""
        self.on_receive_tr_condition.emit(screen_no, code_list, condition_name, condition_index, next)
        self.loop.exit()
    
    def _event_connect(self, err_code):
        """통신 연결 이벤트 처리"""
        self.on_event_connect.emit(err_code)
    
    async def request_account_info(self):
        """백엔드 서버에서 계좌 정보 요청"""
        from .backend_client import BackendClient
        
        try:
            # BackendClient 임시 인스턴스 생성
            client = BackendClient(None)
            await client.start()
            
            # 백엔드 서버에서 계좌 정보 요청
            account_data = await client.fetch_account_info()
            
            if account_data:
                # 계좌 정보 업데이트
                self.account_info = account_data
                logger.info(f"계좌정보 업데이트(백엔드): 예수금={account_data['cash_balance']}, 종목수={len(account_data['positions'])}")
                await client.stop()
                return True
            else:
                logger.error("백엔드에서 계좌 정보 조회 실패")
                await client.stop()
                return False
                
        except Exception as e:
            logger.error(f"계좌 정보 요청 중 오류: {str(e)}")
            return False
    
    def send_order(self, rqname, screen_no, acc_no, order_type, code, quantity, price, hoga_gb, org_order_no):
        """주식 주문 요청
        order_type: 1:신규매수, 2:신규매도, 3:매수취소, 4:매도취소, 5:매수정정, 6:매도정정
        """
        return self.kiwoom.dynamicCall("SendOrder(QString, QString, QString, int, QString, int, int, QString, QString)",
                                      [rqname, screen_no, acc_no, order_type, code, quantity, price, hoga_gb, org_order_no])
    
    def buy_stock(self, code, quantity, price):
        """주식 매수"""
        # 시장가 주문
        if price == 0:
            hoga_gb = "03"
        else:
            hoga_gb = "00"  # 지정가
        
        # 주문 요청
        order_result = self.send_order("매수주문", "0101", self.account_number, 1, code, quantity, price, hoga_gb, "")
        
        if order_result == 0:
            logger.info(f"매수 주문 전송 성공: {code} {quantity}주 {price}원")
            return True
        else:
            logger.error(f"매수 주문 전송 실패: {code} {order_result}")
            return False
    
    def sell_stock(self, code, quantity, price):
        """주식 매도"""
        # 시장가 주문
        if price == 0:
            hoga_gb = "03"
        else:
            hoga_gb = "00"  # 지정가
        
        # 주문 요청
        order_result = self.send_order("매도주문", "0101", self.account_number, 2, code, quantity, price, hoga_gb, "")
        
        if order_result == 0:
            logger.info(f"매도 주문 전송 성공: {code} {quantity}주 {price}원")
            return True
        else:
            logger.error(f"매도 주문 전송 실패: {code} {order_result}")
            return False
    
    def execute_trade_decision(self, decision: TradeDecision) -> bool:
        """거래 결정 실행"""
        try:
            symbol = decision.symbol
            action = decision.action
            price = decision.price
            quantity = decision.quantity
            
            # 매수 주문
            if action == "BUY":
                return self.buy_stock(symbol, quantity, price)
            
            # 매도 주문
            elif action == "SELL":
                return self.sell_stock(symbol, quantity, price)
                
            return False
            
        except Exception as e:
            logger.error(f"거래 실행 중 오류: {str(e)}")
            return False
    
    def get_master_code_name(self, code):
        """종목코드로 종목명 얻기"""
        return self.kiwoom.dynamicCall("GetMasterCodeName(QString)", [code])
    
    def get_all_symbols(self):
        """모든 관심 종목 반환"""
        return self.kospi_symbols + self.kosdaq_symbols
    
    def get_kospi_symbols(self):
        """코스피 종목 반환"""
        return self.kospi_symbols
    
    def get_kosdaq_symbols(self):
        """코스닥 종목 반환"""
        return self.kosdaq_symbols
    
    def get_account_info(self):
        """계좌 정보 반환"""
        return self.account_info