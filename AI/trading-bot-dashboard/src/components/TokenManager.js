import React, { useState, useEffect } from 'react';

function TokenManager({ onRefresh }) {
  const [tokenStatus, setTokenStatus] = useState({
    has_token: false,
    is_valid: false,
    is_expired: false,
    expires_at: null
  });
  const [isLoading, setIsLoading] = useState(false);
  const [message, setMessage] = useState(null);

  const API_BASE_URL = 'http://localhost:8000';

  // 토큰 상태 조회
  const checkTokenStatus = async () => {
    try {
      const response = await fetch(`${API_BASE_URL}/auth/token-status`);
      if (response.ok) {
        const data = await response.json();
        setTokenStatus(data);
      }
    } catch (error) {
      console.error('토큰 상태 확인 중 오류:', error);
    }
  };

  // 토큰 발급 요청
  const requestToken = async () => {
    setIsLoading(true);
    setMessage(null);
    
    try {
      const response = await fetch(`${API_BASE_URL}/auth/request-token`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        }
      });
      
      const data = await response.json();
      
      if (response.ok && data.success) {
        setMessage({ type: 'success', text: '토큰이 성공적으로 발급되었습니다.' });
        // 토큰 상태 새로고침
        await checkTokenStatus();
        // 부모 컴포넌트에 알림
        if (onRefresh) onRefresh();
      } else {
        setMessage({ type: 'danger', text: `토큰 발급 실패: ${data.message || '알 수 없는 오류'}` });
      }
    } catch (error) {
      console.error('토큰 발급 요청 중 오류:', error);
      setMessage({ type: 'danger', text: '토큰 발급 요청 중 오류가 발생했습니다.' });
    } finally {
      setIsLoading(false);
    }
  };

  // 초기 로드 시 토큰 상태 확인
  useEffect(() => {
    checkTokenStatus();
    // 60초마다 토큰 상태 확인
    const interval = setInterval(checkTokenStatus, 60000);
    return () => clearInterval(interval);
  }, []);

  // 만료 시간 포맷팅
  const formatExpiresAt = (dateString) => {
    if (!dateString) return '정보 없음';
    const date = new Date(dateString);
    return `${date.toLocaleDateString()} ${date.toLocaleTimeString()}`;
  };

  return (
    <div className="card mb-4">
      <div className="card-header">
        <h5 className="card-title">키움증권 API 토큰 관리</h5>
      </div>
      <div className="card-body">
        <div className="mb-3">
          <h6>토큰 상태</h6>
          <ul className="list-group">
            <li className="list-group-item d-flex justify-content-between align-items-center">
              토큰 존재
              {tokenStatus.has_token ? 
                <span className="badge bg-success">Yes</span> : 
                <span className="badge bg-danger">No</span>}
            </li>
            <li className="list-group-item d-flex justify-content-between align-items-center">
              토큰 유효성
              {tokenStatus.is_valid ? 
                <span className="badge bg-success">유효함</span> : 
                <span className="badge bg-danger">유효하지 않음</span>}
            </li>
            <li className="list-group-item d-flex justify-content-between align-items-center">
              만료 시간
              <span>{formatExpiresAt(tokenStatus.expires_at)}</span>
            </li>
          </ul>
        </div>
        
        {message && (
          <div className={`alert alert-${message.type} alert-dismissible fade show`} role="alert">
            {message.text}
            <button type="button" className="btn-close" onClick={() => setMessage(null)} aria-label="Close"></button>
          </div>
        )}
        
        <button 
          className="btn btn-primary" 
          onClick={requestToken} 
          disabled={isLoading}
        >
          {isLoading ? '처리 중...' : '토큰 발급 요청'}
        </button>
      </div>
    </div>
  );
}

export default TokenManager;