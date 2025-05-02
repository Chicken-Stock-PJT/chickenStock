import React from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';

const Sidebar = ({ 
  statusInfo, 
  startService, 
  stopService, 
  refreshData, 
  isLoading, 
  onLogout, 
  isAuthenticated,
  togglePriceMonitor,
  showPriceMonitor,
  toggleEnvelopeIndicators,
  showEnvelopeIndicators
}) => {
  
  return (
    <div className="col-md-2 sidebar bg-light">
      <div className="d-flex flex-column p-3 h-100">
        <h3 className="mb-4">AI 자동매매봇</h3>
        
        <div className="mb-4">
          <h6>시스템 상태</h6>
          <div className="mb-2">
            <span className="badge bg-secondary">상태</span>
            <span className={`badge ms-2 ${statusInfo.is_running ? 'bg-success' : 'bg-danger'}`}>
              {statusInfo.is_running ? '실행 중' : '중지됨'}
            </span>
          </div>
          <div className="mb-2">
            <span className="badge bg-secondary">API 연결</span>
            <span className={`badge ms-2 ${statusInfo.connected_to_api ? 'bg-success' : 'bg-danger'}`}>
              {statusInfo.connected_to_api ? '연결됨' : '연결 안됨'}
            </span>
          </div>
          <div className="mb-2">
            <span className="badge bg-secondary">인증 상태</span>
            <span className={`badge ms-2 ${statusInfo.auth_status?.is_authenticated ? 'bg-success' : 'bg-danger'}`}>
              {statusInfo.auth_status?.is_authenticated ? '인증됨' : '인증 안됨'}
            </span>
          </div>
          {statusInfo.start_time && (
            <div className="mb-2 small text-muted">
              시작 시간: {new Date(statusInfo.start_time).toLocaleString()}
            </div>
          )}
        </div>
        
        <div className="mb-4">
          <h6>서비스 제어</h6>
          <div className="d-grid gap-2">
            <button 
              className="btn btn-success btn-sm" 
              onClick={startService}
              disabled={statusInfo.is_running || isLoading || !isAuthenticated}
            >
              서비스 시작
            </button>
            <button 
              className="btn btn-danger btn-sm" 
              onClick={stopService}
              disabled={!statusInfo.is_running || isLoading || !isAuthenticated}
            >
              서비스 중지
            </button>
            <button 
              className="btn btn-primary btn-sm" 
              onClick={refreshData}
              disabled={isLoading || !isAuthenticated}
            >
              상태 새로고침
            </button>
          </div>
        </div>
        
        <div className="mb-4">
          <h6>모니터링</h6>
          <div className="d-grid gap-2">
            <button 
              className={`btn btn-sm ${showPriceMonitor ? 'btn-secondary' : 'btn-outline-primary'}`}
              onClick={togglePriceMonitor}
              disabled={!statusInfo.is_running}
            >
              {showPriceMonitor ? '실시간 시세 숨기기' : '실시간 시세 보기'}
            </button>
            <button 
              className={`btn btn-sm ${showEnvelopeIndicators ? 'btn-secondary' : 'btn-outline-primary'}`}
              onClick={toggleEnvelopeIndicators}
              disabled={!statusInfo.is_running}
            >
              {showEnvelopeIndicators ? 'Envelope 지표 숨기기' : 'Envelope 지표 보기'}
            </button>
          </div>
        </div>
        
        <div className="mt-auto">
          {isAuthenticated && (
            <button className="btn btn-sm btn-outline-secondary w-100" onClick={onLogout}>
              로그아웃
            </button>
          )}
          <div className="small text-muted text-center mt-2">
            v1.0.0
          </div>
        </div>
      </div>
    </div>
  );
};

export default Sidebar;