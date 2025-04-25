import React from 'react';

function Sidebar({ statusInfo, startService, stopService, refreshData, isLoading }) {
  return (
    <div className="col-md-2 sidebar">
      <h3 className="mt-3 mb-4 text-center">
        <i className="fas fa-robot me-2"></i>
        트레이딩봇
      </h3>
      <div className="d-flex flex-column">
        <button 
          id="startBtn" 
          className="btn btn-success mb-2"
          onClick={startService}
          disabled={isLoading}
        >
          {isLoading ? (
            <><i className="fas fa-spinner fa-spin me-2"></i>시작 중...</>
          ) : (
            <><i className="fas fa-play me-2"></i>서비스 시작</>
          )}
        </button>
        <button 
          id="stopBtn" 
          className="btn btn-danger mb-4"
          onClick={stopService}
          disabled={isLoading}
        >
          {isLoading ? (
            <><i className="fas fa-spinner fa-spin me-2"></i>중지 중...</>
          ) : (
            <><i className="fas fa-stop me-2"></i>서비스 중지</>
          )}
        </button>
      </div>
      <div className="status-container p-3 mb-4">
        <h5>시스템 상태</h5>
        <div className="status-item">
          <span>서비스 상태:</span>
          <span id="serviceStatus" className={`badge bg-${statusInfo.is_running ? 'success' : 'danger'}`}>
            {statusInfo.is_running ? '실행 중' : '중지됨'}
          </span>
        </div>
        <div className="status-item">
          <span>키움 API 연결:</span>
          <span id="apiStatus" className={`badge bg-${statusInfo.connected_to_kiwoom ? 'success' : 'danger'}`}>
            {statusInfo.connected_to_kiwoom ? '연결됨' : '연결 안됨'}
          </span>
        </div>
        <div className="status-item">
          <span>구독 종목 수:</span>
          <span id="symbolCount">{statusInfo.subscribed_symbols || '-'}</span>
        </div>
        <div className="status-item">
          <span>시작 시간:</span>
          <span id="startTime">
            {statusInfo.start_time 
              ? new Date(statusInfo.start_time).toLocaleString() 
              : '-'}
          </span>
        </div>
      </div>
      <div className="text-center mt-4">
        <button 
          id="refreshBtn" 
          className="btn btn-info"
          onClick={refreshData}
          disabled={isLoading}
        >
          {isLoading ? (
            <><i className="fas fa-spinner fa-spin me-2"></i>로드 중...</>
          ) : (
            <><i className="fas fa-sync-alt me-2"></i>새로고침</>
          )}
        </button>
      </div>
    </div>
  );
}

export default Sidebar;