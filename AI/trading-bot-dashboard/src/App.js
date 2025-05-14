import React, { useState, useEffect } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import './App.css';
import AccountInfo from './components/AccountInfo';
import Positions from './components/Positions';
import Alert from './components/Alert';
import PriceMonitor from './components/PriceMonitor';

function App() {
  const [statusInfo, setStatusInfo] = useState({
    is_running: false,
    connected_to_api: false,
    subscribed_symbols: 0,
    start_time: null,
    account_info: null
  });
  
  const [accountInfo, setAccountInfo] = useState({
    cash_balance: 0,
    total_asset_value: 0,
    positions: []
  });
  
  const [alert, setAlert] = useState(null);
  const [isLoading, setIsLoading] = useState(true);
  const [showPriceMonitor, setShowPriceMonitor] = useState(false);
  const [activeBots, setActiveBots] = useState([]);
  
  // API URL 기본 경로
  const API_BASE_URL = 'http://localhost:8000';
  
  // API 요청 헤더
  const getHeaders = () => {
    return {
      'Content-Type': 'application/json'
    };
  };
  
  // 알림 메시지 표시 함수
  const showAlert = (type, message) => {
    setAlert({ type, message });
    
    // 5초 후 알림 닫기
    setTimeout(() => {
      setAlert(null);
    }, 5000);
  };
  
  // 활성화된 봇 목록 로드
  const loadActiveBots = async () => {
    try {
      const response = await fetch(`${API_BASE_URL}/bots/active`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        setActiveBots(data);
        
        // 봇이 하나 이상 실행 중이면 상태를 실행 중으로 표시
        if (data.length > 0) {
          setStatusInfo(prevState => ({
            ...prevState,
            is_running: true,
            start_time: data[0].start_time
          }));
        }
      } else {
        console.error('Failed to load active bots:', response.statusText);
      }
    } catch (error) {
      console.error('Error loading active bots:', error);
      showAlert('danger', '봇 정보를 불러오는 중 오류가 발생했습니다.');
    }
  };
  
  // 계좌 정보 로드 함수
  const loadAccountInfo = async () => {
    try {
      const response = await fetch(`${API_BASE_URL}/account`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        setAccountInfo(data);
      }
    } catch (error) {
      console.error('Error loading account info:', error);
    }
  };
  
  // 서버 상태 정보 로드 함수
  const loadStatusInfo = async () => {
    try {
      const response = await fetch(`${API_BASE_URL}/status`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        setStatusInfo(prevState => ({
          ...prevState,
          connected_to_api: data.connected_to_api || false,
          subscribed_symbols: data.subscribed_symbols || 0,
          account_info: data.account_info || null
        }));
        
        // 계좌 정보가 포함되어 있으면 업데이트
        if (data.account_info) {
          setAccountInfo({
            cash_balance: data.account_info.cash_balance || 0,
            total_asset_value: data.account_info.total_asset_value || 0,
            positions: data.account_info.positions || []
          });
        }
      }
    } catch (error) {
      console.error('Error loading status info:', error);
    }
  };
  
  // 데이터 새로고침 함수
  const refreshData = async () => {
    setIsLoading(true);
    try {
      await loadActiveBots();
      await loadStatusInfo();
      await loadAccountInfo();
      showAlert('success', '데이터가 새로고침되었습니다.');
    } catch (error) {
      console.error('Error refreshing data:', error);
      showAlert('danger', '데이터 새로고침 중 오류가 발생했습니다.');
    } finally {
      setIsLoading(false);
    }
  };
  
  // 가격 모니터 토글 함수
  const togglePriceMonitor = () => {
    setShowPriceMonitor(!showPriceMonitor);
  };
  
  // 초기 데이터 로드
  useEffect(() => {
    const loadInitialData = async () => {
      setIsLoading(true);
      try {
        await loadActiveBots();
        await loadStatusInfo();
        await loadAccountInfo();
      } catch (error) {
        console.error('Error loading initial data:', error);
        showAlert('danger', '초기 데이터를 불러오는 중 오류가 발생했습니다.');
      } finally {
        setIsLoading(false);
      }
    };
    
    loadInitialData();
  }, []);
  
  // 주기적인 데이터 업데이트
  useEffect(() => {
    // 30초마다 데이터 새로고침
    const interval = setInterval(() => {
      loadActiveBots();
      loadStatusInfo();
      loadAccountInfo();
    }, 30000);
    
    // 컴포넌트 언마운트 시 인터벌 정리
    return () => clearInterval(interval);
  }, []);
  
  // 로딩 중 표시
  if (isLoading && activeBots.length === 0) {
    return (
      <div className="d-flex justify-content-center align-items-center vh-100">
        <div className="spinner-border text-primary" role="status">
          <span className="visually-hidden">로딩 중...</span>
        </div>
      </div>
    );
  }
  
  return (
    <div className="container-fluid">
      {alert && <Alert type={alert.type} message={alert.message} onClose={() => setAlert(null)} />}
      
      <div className="row mt-3">
        <div className="col-12">
          <div className="card">
            <div className="card-header bg-primary text-white d-flex justify-content-between align-items-center">
              <h5 className="mb-0">자동매매 봇 모니터링</h5>
              <div>
                <button 
                  className="btn btn-light btn-sm me-2"
                  onClick={refreshData}
                  disabled={isLoading}
                >
                  {isLoading ? (
                    <span className="spinner-border spinner-border-sm me-1" role="status" aria-hidden="true"></span>
                  ) : null}
                  새로고침
                </button>
                <button 
                  className="btn btn-light btn-sm"
                  onClick={togglePriceMonitor}
                >
                  {showPriceMonitor ? '실시간 시세 숨기기' : '실시간 시세 보기'}
                </button>
              </div>
            </div>
            <div className="card-body">
              <div className="mb-3">
                <h6>시스템 상태</h6>
                <div className="d-flex flex-wrap">
                  <div className="me-4">
                    <strong>상태:</strong>{' '}
                    <span className={`badge ${statusInfo.is_running ? 'bg-success' : 'bg-danger'}`}>
                      {statusInfo.is_running ? '실행 중' : '중지됨'}
                    </span>
                  </div>
                  <div className="me-4">
                    <strong>API 연결:</strong>{' '}
                    <span className={`badge ${statusInfo.connected_to_api ? 'bg-success' : 'bg-danger'}`}>
                      {statusInfo.connected_to_api ? '연결됨' : '연결 안됨'}
                    </span>
                  </div>
                  <div className="me-4">
                    <strong>구독 종목 수:</strong>{' '}
                    <span className="badge bg-info">{statusInfo.subscribed_symbols}</span>
                  </div>
                  {statusInfo.start_time && (
                    <div>
                      <strong>시작 시간:</strong>{' '}
                      {new Date(statusInfo.start_time).toLocaleString()}
                    </div>
                  )}
                </div>
              </div>
              
              <div className="mb-3">
                <h6>활성 봇 목록 ({activeBots.length})</h6>
                {activeBots.length === 0 ? (
                  <div className="alert alert-warning">실행 중인 봇이 없습니다.</div>
                ) : (
                  <div className="table-responsive">
                    <table className="table table-striped table-bordered">
                      <thead>
                        <tr>
                          <th>봇 ID</th>
                          <th>전략</th>
                          <th>시작 시간</th>
                        </tr>
                      </thead>
                      <tbody>
                        {activeBots.map((bot) => (
                          <tr key={bot.email || bot.id}>
                            <td>{bot.email || bot.id}</td>
                            <td>{bot.strategy}</td>
                            <td>{bot.start_time ? new Date(bot.start_time).toLocaleString() : '-'}</td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                )}
              </div>
            </div>
          </div>
        </div>
      </div>
      
      {/* 실시간 가격 정보 컴포넌트 */}
      {showPriceMonitor && (
        <div className="row mt-3">
          <div className="col-12">
            <PriceMonitor apiBaseUrl={API_BASE_URL} />
          </div>
        </div>
      )}
      
      <div className="row mt-3">
        <div className="col-md-12">
          <AccountInfo accountInfo={accountInfo} />
        </div>
      </div>
      
      <div className="row mt-3">
        <div className="col-12">
          <Positions positions={accountInfo.positions || []} />
        </div>
      </div>
    </div>
  );
}

export default App;