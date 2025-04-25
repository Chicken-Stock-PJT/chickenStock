import React, { useState, useEffect } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import './App.css';
import Sidebar from './components/Sidebar';
import AccountInfo from './components/AccountInfo';
import RecentTrades from './components/RecentTrades';
import Positions from './components/Positions';
import Alert from './components/Alert';

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
  
  const [trades, setTrades] = useState([]);
  const [alert, setAlert] = useState(null);
  const [isLoading, setIsLoading] = useState(false);
  const [token, setToken] = useState('');
  
  // API URL 기본 경로
  const API_BASE_URL = 'http://localhost:8000';
  
  // 토큰 가져오기
  const getToken = async () => {
    try {
      const response = await fetch(`${API_BASE_URL}/auth/token`, {
        method: 'POST'
      });
      
      if (response.ok) {
        const data = await response.json();
        setToken(data.access_token);
        return data.access_token;
      } else {
        console.error('토큰 발급 실패');
        return null;
      }
    } catch (error) {
      console.error('토큰 발급 중 오류:', error);
      return null;
    }
  };
  
  // API 요청 헤더 생성
  const getHeaders = () => {
    return {
      'Authorization': `Bearer ${token}`,
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
  
  // 서비스 시작 함수
  const startService = async () => {
    try {
      setIsLoading(true);
      
      // 토큰이 없으면 가져오기
      const currentToken = token || await getToken();
      if (!currentToken) {
        showAlert('danger', '인증 토큰을 가져올 수 없습니다.');
        setIsLoading(false);
        return;
      }
      
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
      
      // 토큰이 없으면 가져오기
      const currentToken = token || await getToken();
      if (!currentToken) {
        showAlert('danger', '인증 토큰을 가져올 수 없습니다.');
        setIsLoading(false);
        return;
      }
      
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
      // 토큰이 없으면 가져오기
      const currentToken = token || await getToken();
      if (!currentToken) {
        console.error('인증 토큰을 가져올 수 없습니다.');
        return;
      }
      
      const response = await fetch(`${API_BASE_URL}/status`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        setStatusInfo(data);
        
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
      
      // 토큰이 없으면 가져오기
      const currentToken = token || await getToken();
      if (!currentToken) {
        console.error('인증 토큰을 가져올 수 없습니다.');
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
      
      // 토큰이 없으면 가져오기
      const currentToken = token || await getToken();
      if (!currentToken) {
        console.error('인증 토큰을 가져올 수 없습니다.');
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
  
  // 초기 데이터 로드 및 인터벌 설정
  useEffect(() => {
    // 초기 토큰 발급
    getToken().then(() => {
      // 토큰 발급 후 데이터 로드
      loadDashboardData();
    });
    
    // 30초마다 데이터 새로고침
    const interval = setInterval(loadDashboardData, 30000);
    
    // 컴포넌트 언마운트 시 인터벌 정리
    return () => clearInterval(interval);
  }, []);
  
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
        />
        
        <div className="col-md-10 main-content">
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
        </div>
      </div>
    </div>
  );
}

export default App;