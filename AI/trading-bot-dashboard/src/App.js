import React, { useState, useEffect } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import './App.css';
import Sidebar from './components/Sidebar';
import AccountInfo from './components/AccountInfo';
import RecentTrades from './components/RecentTrades';
import Positions from './components/Positions';
import Alert from './components/Alert';
import TokenManager from './components/TokenManager';
import PriceMonitor from './components/PriceMonitor';
import Login from './components/login';
import EnvelopeIndicators from './components/EnvelopeIndicators';
import BollingerIndicators from './components/BollingerIndicators'; // 추가 필요한 컴포넌트

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
  const [showPriceMonitor, setShowPriceMonitor] = useState(false);
  const [showEnvelopeIndicators, setShowEnvelopeIndicators] = useState(false);
  const [showBollingerIndicators, setShowBollingerIndicators] = useState(false); // 볼린저 밴드 지표 표시 여부
  const [showBotManager, setShowBotManager] = useState(false); // 봇 관리자 표시 여부
  const [botsList, setBotsList] = useState([]); // 봇 목록
  const [currentBot, setCurrentBot] = useState(null); // 현재 선택된 봇
  
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
      
      // 먼저 기존의 로그인 엔드포인트 시도
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
        await loadBots(); // 봇 목록 로드
        await loadDashboardData();
      } 
      // 여기서 대체 로그인 로직 추가: 만약 /auth/login이 없다면 봇 생성 API를 통해 로그인
      else if (response.status === 404) {
        // 새 봇 생성 API를 사용하여 로그인 (기본 전략으로 Envelope 사용)
        const createBotResponse = await fetch(`${API_BASE_URL}/bots/create`, {
          method: 'POST',
          headers: getHeaders(),
          body: JSON.stringify({
            email,
            password,
            strategy: "envelope" // 기본 전략
          })
        });
        
        if (createBotResponse.ok) {
          setIsAuthenticated(true);
          showAlert('success', '봇 생성 및 로그인 성공!');
          await loadBots(); // 봇 목록 로드
          await loadDashboardData();
        } else {
          // 이미 존재하는 봇인 경우도 성공으로 처리 (중복 봇 생성 시도)
          if (createBotResponse.status === 400) {
            setIsAuthenticated(true);
            showAlert('success', '기존 봇으로 로그인 성공!');
            await loadBots(); // 봇 목록 로드
            await loadDashboardData();
          } else {
            const errorData = await createBotResponse.json();
            showAlert('danger', '로그인 실패: ' + (errorData.detail || '알 수 없는 오류'));
          }
        }
      } else {
        showAlert('danger', '로그인 실패: ' + (data.detail || data.message || '알 수 없는 오류'));
      }
    } catch (error) {
      console.error('Login error:', error);
      
      // 서버가 응답하지 않는 경우 대체 메시지
      if (error.name === 'TypeError' && error.message.includes('Failed to fetch')) {
        showAlert('danger', '서버에 연결할 수 없습니다. 서버가 실행 중인지 확인하세요.');
      } else {
        showAlert('danger', '로그인 실패: 서버 연결 오류');
      }
    } finally {
      setIsLoading(false);
    }
  };
  
  // 로그아웃 처리 함수
  const handleLogout = async () => {
    try {
      setIsAuthenticated(false);
      setCurrentBot(null);
      setBotsList([]);
      showAlert('info', '로그아웃 되었습니다.');
      // 필요한 경우 서버 측 로그아웃 엔드포인트 호출
    } catch (error) {
      console.error('Logout error:', error);
    }
  };
  
  // 인증 상태 로드 함수도 수정 - 봇 상태를 통해 인증 상태 확인
  const loadAuthStatus = async () => {
    try {
      // 먼저 /auth/status 엔드포인트 시도
      const response = await fetch(`${API_BASE_URL}/auth/status`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        setIsAuthenticated(data.is_authenticated);
        return data;
      }
      
      // /auth/status가 없으면 봇 목록을 통해 인증 상태 확인
      const botsResponse = await fetch(`${API_BASE_URL}/bots`, {
        headers: getHeaders()
      });
      
      if (botsResponse.ok) {
        const botsData = await botsResponse.json();
        // 봇 목록을 가져올 수 있으면 인증된 것으로 간주
        setIsAuthenticated(true);
        return { is_authenticated: true };
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
  
  // 봇 목록 로드 함수
  const loadBots = async () => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/bots`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        setBotsList(data);
        
        // 실행 중인 봇이 있으면 첫 번째 봇을 현재 봇으로 설정
        const runningBots = data.filter(bot => bot.is_running);
        if (runningBots.length > 0) {
          setCurrentBot(runningBots[0]);
        } else if (data.length > 0) {
          setCurrentBot(data[0]);
        }
      } else {
        console.error('Failed to load bots:', response.statusText);
      }
    } catch (error) {
      console.error('Error loading bots:', error);
      showAlert('danger', '봇 목록 로드 중 오류가 발생했습니다.');
    } finally {
      setIsLoading(false);
    }
  };
  
  // 봇 생성 함수
  const createBot = async (email, password, strategy) => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/bots/create`, {
        method: 'POST',
        headers: getHeaders(),
        body: JSON.stringify({
          email,
          password,
          strategy
        })
      });
      
      if (response.ok) {
        const data = await response.json();
        showAlert('success', `${email} 봇이 성공적으로 생성되었습니다.`);
        await loadBots(); // 봇 목록 새로고침
        return data;
      } else {
        const errorData = await response.json();
        showAlert('danger', `봇 생성 실패: ${errorData.detail || '알 수 없는 오류'}`);
        return null;
      }
    } catch (error) {
      console.error('Error creating bot:', error);
      showAlert('danger', '봇 생성 중 오류가 발생했습니다.');
      return null;
    } finally {
      setIsLoading(false);
    }
  };
  
  // 두 개의 봇 동시 생성 함수
  const createBothBots = async (envelopeEmail, bollingerEmail, password) => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/bots/create-and-start-both`, {
        method: 'POST',
        headers: getHeaders(),
        body: JSON.stringify({
          envelope_email: envelopeEmail,
          bollinger_email: bollingerEmail,
          password
        })
      });
      
      if (response.ok) {
        const data = await response.json();
        showAlert('success', '두 개의 봇이 성공적으로 생성되고 시작되었습니다.');
        await loadBots(); // 봇 목록 새로고침
        return data;
      } else {
        const errorData = await response.json();
        showAlert('danger', `봇 생성 실패: ${errorData.detail || '알 수 없는 오류'}`);
        return null;
      }
    } catch (error) {
      console.error('Error creating both bots:', error);
      showAlert('danger', '봇 생성 중 오류가 발생했습니다.');
      return null;
    } finally {
      setIsLoading(false);
    }
  };
  
  // 봇 시작 함수
  const startBot = async (email) => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/bots/${email}/start`, {
        method: 'POST',
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        showAlert('success', `${email} 봇이 시작되었습니다.`);
        await loadBots(); // 봇 목록 새로고침
        return data;
      } else {
        const errorData = await response.json();
        showAlert('danger', `봇 시작 실패: ${errorData.detail || '알 수 없는 오류'}`);
        return null;
      }
    } catch (error) {
      console.error('Error starting bot:', error);
      showAlert('danger', '봇 시작 중 오류가 발생했습니다.');
      return null;
    } finally {
      setIsLoading(false);
    }
  };
  
  // 봇 중지 함수
  const stopBot = async (email) => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/bots/${email}/stop`, {
        method: 'POST',
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        showAlert('success', `${email} 봇이 중지되었습니다.`);
        await loadBots(); // 봇 목록 새로고침
        return data;
      } else {
        const errorData = await response.json();
        showAlert('danger', `봇 중지 실패: ${errorData.detail || '알 수 없는 오류'}`);
        return null;
      }
    } catch (error) {
      console.error('Error stopping bot:', error);
      showAlert('danger', '봇 중지 중 오류가 발생했습니다.');
      return null;
    } finally {
      setIsLoading(false);
    }
  };
  
  // 봇 삭제 함수
  const deleteBot = async (email) => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/bots/${email}`, {
        method: 'DELETE',
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        showAlert('success', `${email} 봇이 삭제되었습니다.`);
        
        // 현재 선택된 봇이 삭제되었으면 선택 해제
        if (currentBot && currentBot.email === email) {
          setCurrentBot(null);
        }
        
        await loadBots(); // 봇 목록 새로고침
        return data;
      } else {
        const errorData = await response.json();
        showAlert('danger', `봇 삭제 실패: ${errorData.detail || '알 수 없는 오류'}`);
        return null;
      }
    } catch (error) {
      console.error('Error deleting bot:', error);
      showAlert('danger', '봇 삭제 중 오류가 발생했습니다.');
      return null;
    } finally {
      setIsLoading(false);
    }
  };
  
  // 모든 봇 시작 함수
  const startAllBots = async () => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/bots/start-all`, {
        method: 'POST',
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        showAlert('success', `모든 봇 시작 요청이 처리되었습니다: ${data.message}`);
        await loadBots(); // 봇 목록 새로고침
        return data;
      } else {
        const errorData = await response.json();
        showAlert('danger', `봇 시작 실패: ${errorData.detail || '알 수 없는 오류'}`);
        return null;
      }
    } catch (error) {
      console.error('Error starting all bots:', error);
      showAlert('danger', '봇 시작 중 오류가 발생했습니다.');
      return null;
    } finally {
      setIsLoading(false);
    }
  };
  
  // 모든 봇 중지 함수
  const stopAllBots = async () => {
    try {
      setIsLoading(true);
      
      const response = await fetch(`${API_BASE_URL}/bots/stop-all`, {
        method: 'POST',
        headers: getHeaders()
      });
      
      if (response.ok) {
        const data = await response.json();
        showAlert('success', `모든 봇 중지 요청이 처리되었습니다: ${data.message}`);
        await loadBots(); // 봇 목록 새로고침
        return data;
      } else {
        const errorData = await response.json();
        showAlert('danger', `봇 중지 실패: ${errorData.detail || '알 수 없는 오류'}`);
        return null;
      }
    } catch (error) {
      console.error('Error stopping all bots:', error);
      showAlert('danger', '봇 중지 중 오류가 발생했습니다.');
      return null;
    } finally {
      setIsLoading(false);
    }
  };
  
  // 봇 상태 정보 로드 함수
  const loadBotStatus = async (email) => {
    if (!email) return null;
    
    try {
      const response = await fetch(`${API_BASE_URL}/bots/${email}/status`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        return await response.json();
      } else {
        console.error('Failed to load bot status:', response.statusText);
        return null;
      }
    } catch (error) {
      console.error(`Error loading status for bot ${email}:`, error);
      return null;
    }
  };
  
  // 봇별 지표 로드 함수
  const loadBotIndicators = async (email) => {
    if (!email) return null;
    
    try {
      const response = await fetch(`${API_BASE_URL}/indicators/${email}`, {
        headers: getHeaders()
      });
      
      if (response.ok) {
        return await response.json();
      } else {
        console.error('Failed to load bot indicators:', response.statusText);
        return null;
      }
    } catch (error) {
      console.error(`Error loading indicators for bot ${email}:`, error);
      return null;
    }
  };
  
  // 기존 서비스 시작 함수 - 레거시 지원용
  const startService = async () => {
    // 현재 선택된 봇이 있으면 해당 봇 시작
    if (currentBot) {
      await startBot(currentBot.email);
    } else {
      showAlert('warning', '시작할 봇을 선택해주세요.');
    }
  };
  
  // 기존 서비스 중지 함수 - 레거시 지원용
  const stopService = async () => {
    // 현재 선택된 봇이 있으면 해당 봇 중지
    if (currentBot) {
      await stopBot(currentBot.email);
    } else {
      showAlert('warning', '중지할 봇을 선택해주세요.');
    }
  };
  
  // 상태 정보 로드 함수 - 레거시 지원용
  const loadStatusInfo = async () => {
    // 현재 선택된 봇이 있으면 해당 봇의 상태 로드
    if (currentBot) {
      const botStatus = await loadBotStatus(currentBot.email);
      if (botStatus) {
        setStatusInfo({
          is_running: botStatus.is_running,
          connected_to_api: true, // 봇 상태에서는 이 정보를 제공하지 않으므로 기본값 설정
          subscribed_symbols: 0, // 봇 상태에서는 이 정보를 제공하지 않으므로 기본값 설정
          start_time: botStatus.start_time,
          account_info: botStatus.account_info,
          auth_status: {
            is_authenticated: true, // 봇이 있다면 인증된 상태로 간주
            access_token_expires_at: null // 봇 상태에서는 이 정보를 제공하지 않음
          }
        });
        
        // 계좌 정보가 포함되어 있으면 업데이트
        if (botStatus.account_info) {
          setAccountInfo({
            cash_balance: botStatus.account_info.cash_balance || 0,
            total_asset_value: botStatus.account_info.total_asset_value || 0,
            positions: botStatus.account_info.positions || []
          });
        }
      }
    } 
    // 선택된 봇이 없거나 기존 API를 사용하는 경우 기존 로직 유지
    else {
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
    }
  };
  
  // 계좌 정보 로드 함수
  const loadAccountInfo = async () => {
    // 현재 선택된 봇이 있고 실행 중이면 해당 봇의 상태에서 계좌 정보 가져오기
    if (currentBot && currentBot.is_running) {
      const botStatus = await loadBotStatus(currentBot.email);
      if (botStatus && botStatus.account_info) {
        setAccountInfo({
          cash_balance: botStatus.account_info.cash_balance || 0,
          total_asset_value: botStatus.account_info.total_asset_value || 0,
          positions: botStatus.account_info.positions || []
        });
        return;
      }
    }
    
    // 선택된 봇이 없거나 기존 API를 사용하는 경우 기존 로직 유지
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
        // API가 반환하는 형식이 tradeHistories 객체를 포함하는 형식이므로 이를 처리
        if (data.tradeHistories) {
          setTrades(data.tradeHistories);
        } else {
          // API가 배열을 직접 반환하는 경우 (이전 버전 호환성)
          setTrades(data);
        }
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
      await loadBots(); // 봇 목록 갱신
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
  
  // Envelope 지표 토글 함수
  const toggleEnvelopeIndicators = () => {
    setShowEnvelopeIndicators(!showEnvelopeIndicators);
    if (!showEnvelopeIndicators) {
      setShowBollingerIndicators(false); // 하나만 보이도록
    }
  };
  
  // 볼린저 밴드 지표 토글 함수
  const toggleBollingerIndicators = () => {
    setShowBollingerIndicators(!showBollingerIndicators);
    if (!showBollingerIndicators) {
      setShowEnvelopeIndicators(false); // 하나만 보이도록
    }
  };
  
  // 봇 관리자 토글 함수
  const toggleBotManager = () => {
    setShowBotManager(!showBotManager);
  };
  
  // 현재 봇 변경 함수
  const changeCurrentBot = (bot) => {
    setCurrentBot(bot);
    // 봇 변경 시 데이터 새로고침
    loadStatusInfo();
  };
  
  // 초기 인증 상태 확인
  useEffect(() => {
    const checkAuth = async () => {
      setAuthLoading(true);
      try {
        const authStatus = await loadAuthStatus();
        setIsAuthenticated(authStatus.is_authenticated);
        
        if (authStatus.is_authenticated) {
          await loadBots(); // 봇 목록 로드
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
          toggleBollingerIndicators={toggleBollingerIndicators}
          showBollingerIndicators={showBollingerIndicators}
          toggleBotManager={toggleBotManager}
          showBotManager={showBotManager}
          botsList={botsList}
          currentBot={currentBot}
          onChangeBot={changeCurrentBot}
        />
        
        <div className="col-md-10 main-content">
          <div className="row mt-3">
            <div className="col-md-6">
              <TokenManager onRefresh={loadDashboardData} />
            </div>
            <div className="col-md-6 text-end">
              <button 
                className="btn btn-primary mb-3 me-2"
                onClick={toggleBotManager}
              >
                {showBotManager ? '봇 관리 숨기기' : '봇 관리 보기'}
              </button>
              <button 
                className={`btn ${showPriceMonitor ? 'btn-secondary' : 'btn-primary'} mb-3`}
                onClick={togglePriceMonitor}
              >
                {showPriceMonitor ? '실시간 시세 숨기기' : '실시간 시세 보기'}
              </button>
            </div>
          </div>
          
          {/* 봇 관리자 컴포넌트 */}
          {showBotManager && (
            <div className="row mt-3 mb-4">
              <div className="col-md-12">
                <div className="card">
                  <div className="card-header bg-primary text-white d-flex justify-content-between align-items-center">
                    <h5 className="mb-0">봇 관리</h5>
                    <div>
                      <button
                        className="btn btn-sm btn-light me-2"
                        onClick={startAllBots}
                        disabled={isLoading}
                      >
                        모든 봇 시작
                      </button>
                      <button
                        className="btn btn-sm btn-light"
                        onClick={stopAllBots}
                        disabled={isLoading}
                      >
                        모든 봇 중지
                      </button>
                    </div>
                  </div>
                  <div className="card-body">
                    <div className="row mb-3">
                      <div className="col-md-12">
                        <h6>두 전략 봇 동시 생성</h6>
                        <div className="d-flex">
                          <input
                            type="text"
                            className="form-control me-2"
                            placeholder="Envelope 봇 이메일"
                            id="envelopeEmail"
                          />
                          <input
                            type="text"
                            className="form-control me-2"
                            placeholder="Bollinger 봇 이메일"
                            id="bollingerEmail"
                          />
                          <input
                            type="password"
                            className="form-control me-2"
                            placeholder="비밀번호"
                            id="bothPassword"
                          />
                          <button
                            className="btn btn-success"
                            onClick={() => {
                              const envelopeEmail = document.getElementById('envelopeEmail').value;
                              const bollingerEmail = document.getElementById('bollingerEmail').value;
                              const password = document.getElementById('bothPassword').value;
                              createBothBots(envelopeEmail, bollingerEmail, password);
                            }}
                            disabled={isLoading}
                          >
                            생성 및 시작
                          </button>
                        </div>
                      </div>
                    </div>
                    
                    <div className="row mb-3">
                      <div className="col-md-12">
                        <h6>개별 봇 생성</h6>
                        <div className="d-flex">
                          <input
                            type="text"
                            className="form-control me-2"
                            placeholder="이메일"
                            id="singleEmail"
                          />
                          <input
                            type="password"
                            className="form-control me-2"
                            placeholder="비밀번호"
                            id="singlePassword"
                          />
                          <select className="form-select me-2" id="strategy">
                            <option value="envelope">Envelope 전략</option>
                            <option value="bollinger">볼린저 밴드 전략</option>
                          </select>
                          <button
                            className="btn btn-primary"
                            onClick={() => {
                              const email = document.getElementById('singleEmail').value;
                              const password = document.getElementById('singlePassword').value;
                              const strategy = document.getElementById('strategy').value;
                              createBot(email, password, strategy);
                            }}
                            disabled={isLoading}
                          >
                            생성
                          </button>
                        </div>
                      </div>
                    </div>
                    
                    <h6>봇 목록 {botsList.length > 0 ? `(${botsList.length})` : ''}</h6>
                    <div className="table-responsive">
                      <table className="table table-striped table-bordered">
                        <thead>
                          <tr>
                            <th>계정</th>
                            <th>전략</th>
                            <th>상태</th>
                            <th>시작 시간</th>
                            <th>작업</th>
                          </tr>
                        </thead>
                        <tbody>
                          {botsList.length === 0 ? (
                            <tr>
                              <td colSpan="5" className="text-center">봇이 없습니다.</td>
                            </tr>
                          ) : (
                            botsList.map((bot) => (
                              <tr key={bot.email} className={currentBot?.email === bot.email ? 'table-active' : ''}>
                                <td>{bot.email}</td>
                                <td>{bot.strategy}</td>
                                <td>
                                  <span className={`badge ${bot.is_running ? 'bg-success' : 'bg-danger'}`}>
                                    {bot.is_running ? '실행 중' : '중지됨'}
                                  </span>
                                </td>
                                <td>{bot.start_time ? new Date(bot.start_time).toLocaleString() : '-'}</td>
                                <td>
                                  <button
                                    className="btn btn-sm btn-primary me-1"
                                    onClick={() => changeCurrentBot(bot)}
                                  >
                                    선택
                                  </button>
                                  {!bot.is_running ? (
                                    <button
                                      className="btn btn-sm btn-success me-1"
                                      onClick={() => startBot(bot.email)}
                                      disabled={isLoading}
                                    >
                                      시작
                                    </button>
                                  ) : (
                                    <button
                                      className="btn btn-sm btn-warning me-1"
                                      onClick={() => stopBot(bot.email)}
                                      disabled={isLoading}
                                    >
                                      중지
                                    </button>
                                  )}
                                  <button
                                    className="btn btn-sm btn-danger"
                                    onClick={() => {
                                      if (window.confirm(`${bot.email} 봇을 삭제하시겠습니까?`)) {
                                        deleteBot(bot.email);
                                      }
                                    }}
                                    disabled={isLoading || bot.is_running}
                                  >
                                    삭제
                                  </button>
                                </td>
                              </tr>
                            ))
                          )}
                        </tbody>
                      </table>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          )}
          
          {/* 현재 선택된 봇 정보 표시 */}
          {currentBot && (
            <div className="alert alert-info">
              <strong>현재 선택된 봇:</strong> {currentBot.email} ({currentBot.strategy} 전략)
              <span className={`badge ms-2 ${currentBot.is_running ? 'bg-success' : 'bg-danger'}`}>
                {currentBot.is_running ? '실행 중' : '중지됨'}
              </span>
            </div>
          )}
          
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
                {currentBot && currentBot.is_running ? (
                  <EnvelopeIndicators 
                    apiBaseUrl={API_BASE_URL} 
                    botEmail={currentBot.email}
                    useBotEndpoint={true}
                  />
                ) : (
                  <EnvelopeIndicators apiBaseUrl={API_BASE_URL} />
                )}
              </div>
            </div>
          )}
          
          {/* 볼린저 밴드 지표 컴포넌트 */}
          {showBollingerIndicators && (
            <div className="row mt-3">
              <div className="col-md-12">
                {currentBot && currentBot.is_running && currentBot.strategy === 'bollinger' ? (
                  <BollingerIndicators 
                    apiBaseUrl={API_BASE_URL} 
                    botEmail={currentBot.email}
                    useBotEndpoint={true}
                  />
                ) : (
                  <div className="alert alert-warning">
                    볼린저 밴드 지표를 확인하려면 볼린저 밴드 전략을 사용하는 봇을 선택하세요.
                  </div>
                )}
              </div>
            </div>
          )}
          
          <div className="row mt-3">
            <div className="col-md-12 text-end">
              <button 
                className={`btn ${showEnvelopeIndicators ? 'btn-secondary' : 'btn-primary'} mb-3 me-2`}
                onClick={toggleEnvelopeIndicators}
              >
                {showEnvelopeIndicators ? 'Envelope 지표 숨기기' : 'Envelope 지표 보기'}
              </button>
              <button 
                className={`btn ${showBollingerIndicators ? 'btn-secondary' : 'btn-primary'} mb-3`}
                onClick={toggleBollingerIndicators}
              >
                {showBollingerIndicators ? '볼린저 밴드 지표 숨기기' : '볼린저 밴드 지표 보기'}
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;