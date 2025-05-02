import React, { useState, useEffect } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import './App.css';
import Sidebar from './components/Sidebar';
import AccountInfo from './components/AccountInfo';
import RecentTrades from './components/RecentTrades';
import Positions from './components/Positions';
import Alert from './components/Alert';
import TokenManager from './components/TokenManager';
import PriceMonitor from './components/PriceMonitor'; // 새로 추가한 컴포넌트
import Login from './components/login'; // 로그인 컴포넌트 가져오기
import EnvelopeIndicators from './components/EnvelopeIndicators';

function App() {
  const [statusInfo, setStatusInfo] = useState({
    is_running: false,
    connected_to_api: false,
    subscribed_symbols: 0,
    start_time: null,
    account_info: null,
    auth_status: {
      is_authenticated: false,
      access_token_expires_at: null
    }
  });
  
  const [accountInfo, setAccountInfo] = useState({
    cash_balance: 0,
    total_asset_value: 0,
    positions: []
  });
  
  const [trades, setTrades] = useState([]);
  const [alert, setAlert] = useState(null);
  const [isLoading, setIsLoading] = useState(false);
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  const [authLoading, setAuthLoading] = useState(true);
  const [showPriceMonitor, setShowPriceMonitor] = useState(false); // 가격 모니터 표시 여부
  const [showEnvelopeIndicators, setShowEnvelopeIndicators] = useState(false); // Envelope 지표 표시 여부
  
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
  
  // 로그인 처리 함수
  const handleLogin = async (email, password) => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/auth/login`, {
        method: 'POST',
        headers: getHeaders(),
        body: JSON.stringify({
          email,
          password,
          platform: 'mobile'
        })
      });
      
      const data = await response.json();
      
      if (response.ok && data.success) {
        setIsAuthenticated(true);
        showAlert('success', '로그인 성공!');
        await loadAuthStatus();
        await loadDashboardData();
      } else {
        showAlert('danger', '로그인 실패: ' + (data.detail || data.message || '알 수 없는 오류'));
      }
    } catch (error) {
      console.error('Login error:', error);
      showAlert('danger', '로그인 실패: 서버 연결 오류');
    } finally {
      setIsLoading(false);
    }
  };
  
  // 로그아웃 처리 함수
  const handleLogout = async () => {
    try {
      setIsAuthenticated(false);
      showAlert('info', '로그아웃 되었습니다.');
      // 필요한 경우 서버 측 로그아웃 엔드포인트 호출
    } catch (error) {
      console.error('Logout error:', error);
    }
  };
  
  // 인증 상태 로드 함수
  const loadAuthStatus = async () => {
    try {
      const response = await fetch(`${API_BASE_URL}/auth/status`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        setIsAuthenticated(data.is_authenticated);
        
        return data;
      }
      
      return { is_authenticated: false };
    } catch (error) {
      console.error('Error loading auth status:', error);
      return { is_authenticated: false };
    }
  };
  
  // 토큰 갱신 함수
  const refreshToken = async () => {
    try {
      const response = await fetch(`${API_BASE_URL}/auth/refresh`, {
        method: 'POST',
        headers: getHeaders()
      });
      
      const data = await response.json();
      
      if (response.ok && data.success) {
        return true;
      } else {
        return false;
      }
    } catch (error) {
      console.error('Token refresh error:', error);
      return false;
    }
  };
  
  // 서비스 시작 함수
  const startService = async () => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/service/start`, {
        method: 'POST',
        headers: getHeaders()
      });
      
      const data = await response.json();
      
      if (response.ok) {
        showAlert('success', '서비스 시작 요청이 성공적으로 처리되었습니다.');
        // 1초 후 데이터 새로고침
        setTimeout(loadDashboardData, 1000);
      } else {
        showAlert('danger', `서비스 시작 실패: ${data.detail || '알 수 없는 오류'}`);
      }
    } catch (error) {
      console.error('Error starting service:', error);
      showAlert('danger', '서비스 시작 중 오류가 발생했습니다.');
    } finally {
      setIsLoading(false);
    }
  };
  
  // 서비스 중지 함수
  const stopService = async () => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/service/stop`, {
        method: 'POST',
        headers: getHeaders()
      });
      
      const data = await response.json();
      
      if (response.ok) {
        showAlert('success', '서비스가 중지되었습니다.');
        // 1초 후 데이터 새로고침
        setTimeout(loadDashboardData, 1000);
      } else {
        showAlert('danger', `서비스 중지 실패: ${data.detail || '알 수 없는 오류'}`);
      }
    } catch (error) {
      console.error('Error stopping service:', error);
      showAlert('danger', '서비스 중지 중 오류가 발생했습니다.');
    } finally {
      setIsLoading(false);
    }
  };
  
  // 상태 정보 로드 함수
  const loadStatusInfo = async () => {
    try {
      const response = await fetch(`${API_BASE_URL}/status`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        setStatusInfo(data);
        
        // 인증 상태 업데이트
        if (data.auth_status) {
          setIsAuthenticated(data.auth_status.is_authenticated);
        }
        
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
  
  // 계좌 정보 로드 함수
  const loadAccountInfo = async () => {
    try {
      // 서비스가 실행 중이 아니면 요청 건너뛰기
      if (!statusInfo.is_running) {
        return;
      }
      
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
  
  // 거래 이력 로드 함수
  const loadTradeHistory = async () => {
    try {
      // 서비스가 실행 중이 아니면 요청 건너뛰기
      if (!statusInfo.is_running) {
        return;
      }
      
      const response = await fetch(`${API_BASE_URL}/trades`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        setTrades(data);
      }
    } catch (error) {
      console.error('Error loading trade history:', error);
    }
  };
  
  // 대시보드 데이터 로드 함수
  const loadDashboardData = async () => {
    if (!isAuthenticated) return;
    
    setIsLoading(true);
    try {
      await loadStatusInfo();
      // 상태 정보에 따라 다른 데이터 로드
      if (statusInfo.is_running) {
        await loadAccountInfo();
        await loadTradeHistory();
      }
    } catch (error) {
      console.error('Error loading dashboard data:', error);
      showAlert('danger', '데이터 로드 중 오류가 발생했습니다.');
    } finally {
      setIsLoading(false);
    }
  };
  
  // 토큰 갱신 검사 함수
  const checkAndRefreshToken = async () => {
    if (!isAuthenticated) return;
    
    // 토큰 만료 시간 확인
    if (statusInfo.auth_status && statusInfo.auth_status.access_token_expires_at) {
      const expiresAt = new Date(statusInfo.auth_status.access_token_expires_at);
      const now = new Date();
      const timeToExpiry = expiresAt - now;
      
      // 만료 10분 전에 갱신
      if (timeToExpiry > 0 && timeToExpiry < 10 * 60 * 1000) {
        try {
          const refreshed = await refreshToken();
          if (!refreshed) {
            // 토큰 갱신 실패 시 재로그인 필요
            setIsAuthenticated(false);
            showAlert('warning', '인증이 만료되었습니다. 다시 로그인해주세요.');
          } else {
            // 갱신 성공 시 상태 다시 로드
            await loadAuthStatus();
          }
        } catch (error) {
          console.error('Token refresh check error:', error);
        }
      }
    }
  };
  
  // 가격 모니터 토글 함수
  const togglePriceMonitor = () => {
    setShowPriceMonitor(!showPriceMonitor);
  };
  
  // 초기 인증 상태 확인
  useEffect(() => {
    const checkAuth = async () => {
      setAuthLoading(true);
      try {
        const authStatus = await loadAuthStatus();
        setIsAuthenticated(authStatus.is_authenticated);
        
        if (authStatus.is_authenticated) {
          await loadDashboardData();
        }
      } catch (error) {
        console.error('Initial auth check error:', error);
        setIsAuthenticated(false);
      } finally {
        setAuthLoading(false);
      }
    };
    
    checkAuth();
  }, []);

  // 3. Envelope 지표 토글 함수 추가
  const toggleEnvelopeIndicators = () => {
    setShowEnvelopeIndicators(!showEnvelopeIndicators);
  };
  
  // 데이터 로드 및 토큰 갱신 인터벌 설정
  useEffect(() => {
    if (!isAuthenticated) return;
    
    // 30초마다 데이터 새로고침
    const dataInterval = setInterval(loadDashboardData, 30000);
    
    // 1분마다 토큰 갱신 검사
    const tokenInterval = setInterval(checkAndRefreshToken, 60000);
    
    // 컴포넌트 언마운트 시 인터벌 정리
    return () => {
      clearInterval(dataInterval);
      clearInterval(tokenInterval);
    };
  }, [isAuthenticated, statusInfo.is_running]);
  
  // 로딩 중 표시
  if (authLoading) {
    return (
      <div className="d-flex justify-content-center align-items-center vh-100">
        <div className="spinner-border text-primary" role="status">
          <span className="visually-hidden">로딩 중...</span>
        </div>
      </div>
    );
  }
  
  // 로그인되지 않은 경우 로그인 화면 표시
  if (!isAuthenticated) {
    return (
      <div className="container">
        <div className="row justify-content-center mt-5">
          <div className="col-md-6 col-lg-4">
            {alert && <Alert type={alert.type} message={alert.message} onClose={() => setAlert(null)} />}
            <div className="text-center mb-4">
              <h2>주식 자동매매 봇</h2>
              <p className="text-muted">AI 트레이딩 시스템에 로그인하세요</p>
            </div>
            <Login onLogin={handleLogin} isLoading={isLoading} />
          </div>
        </div>
      </div>
    );
  }
  
  // 로그인된 경우 대시보드 표시
  return (
    <div className="container-fluid">
      {alert && <Alert type={alert.type} message={alert.message} onClose={() => setAlert(null)} />}
      <div className="row">
        <Sidebar 
          statusInfo={statusInfo}
          startService={startService}
          stopService={stopService}
          refreshData={loadDashboardData}
          isLoading={isLoading}
          onLogout={handleLogout}
          isAuthenticated={isAuthenticated}
          togglePriceMonitor={togglePriceMonitor}
          showPriceMonitor={showPriceMonitor}
          toggleEnvelopeIndicators={toggleEnvelopeIndicators}
          showEnvelopeIndicators={showEnvelopeIndicators}
        />
        
        <div className="col-md-10 main-content">
          <div className="row mt-3">
            <div className="col-md-6">
              <TokenManager onRefresh={loadDashboardData} />
            </div>
            <div className="col-md-6 text-end">
              <button 
                className={`btn ${showPriceMonitor ? 'btn-secondary' : 'btn-primary'} mb-3`}
                onClick={togglePriceMonitor}
              >
                {showPriceMonitor ? '실시간 시세 숨기기' : '실시간 시세 보기'}
              </button>
            </div>
          </div>
          {/* 실시간 가격 정보 컴포넌트 */}
          {showPriceMonitor && (
            <div className="row mt-3">
              <div className="col-md-12">
                <PriceMonitor apiBaseUrl={API_BASE_URL} />
              </div>
            </div>
          )}          
          <div className="row mt-3">
            <div className="col-md-4">
              <AccountInfo accountInfo={accountInfo} />
            </div>
            <div className="col-md-8">
              <RecentTrades trades={trades} />
            </div>
          </div>
          <div className="row mt-3">
            <div className="col-md-12">
              <Positions positions={accountInfo.positions || []} />
            </div>
          </div>
          {/* Envelope 지표 컴포넌트 */}
          {showEnvelopeIndicators && (
            <div className="row mt-3">
              <div className="col-md-12">
                <EnvelopeIndicators apiBaseUrl={API_BASE_URL} />
              </div>
            </div>
          )}
          <div className="col-md-6 text-end">
            <button 
              className={`btn ${showPriceMonitor ? 'btn-secondary' : 'btn-primary'} mb-3 me-2`}
              onClick={togglePriceMonitor}
            >
              {showPriceMonitor ? '실시간 시세 숨기기' : '실시간 시세 보기'}
            </button>
            <button 
              className={`btn ${showEnvelopeIndicators ? 'btn-secondary' : 'btn-primary'} mb-3`}
              onClick={toggleEnvelopeIndicators}
            >
              {showEnvelopeIndicators ? 'Envelope 지표 숨기기' : 'Envelope 지표 보기'}
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;