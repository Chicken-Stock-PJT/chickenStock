import React, { useState, useEffect } from 'react';

function BollingerIndicators({ apiBaseUrl, botEmail, useBotEndpoint = false }) {
  const [indicators, setIndicators] = useState({});
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [search, setSearch] = useState('');
  const [symbolFilter, setSymbolFilter] = useState('');
  const [sortField, setSortField] = useState('percentB');
  const [sortDirection, setSortDirection] = useState('asc');
  const [filterSignal, setFilterSignal] = useState('all');

  useEffect(() => {
    const fetchData = async () => {
      try {
        setLoading(true);

        // 봇 엔드포인트를 사용하는 경우와 기존 엔드포인트를 사용하는 경우 구분
        let endpoint = `${apiBaseUrl}/indicators`;
        if (useBotEndpoint && botEmail) {
          endpoint = `${apiBaseUrl}/indicators/${botEmail}`;
        }

        const response = await fetch(endpoint);
        if (!response.ok) {
          throw new Error('데이터를 가져오는데 실패했습니다.');
        }

        const data = await response.json();
        
        // 응답 형식에 따라 데이터 추출
        let indicatorsData = {};
        if (data.indicators) {
          indicatorsData = data.indicators;
        } else if (Object.keys(data).length > 0 && typeof data.total_count === 'undefined') {
          indicatorsData = data;
        }

        setIndicators(indicatorsData);
        setError(null);
      } catch (err) {
        setError(err.message);
        console.error('데이터 가져오기 오류:', err);
      } finally {
        setLoading(false);
      }
    };

    fetchData();
    
    // 1분마다 데이터 자동 갱신
    const intervalId = setInterval(fetchData, 60000);
    
    return () => clearInterval(intervalId);
  }, [apiBaseUrl, botEmail, useBotEndpoint]);

  // 검색어 변경 핸들러
  const handleSearchChange = (e) => {
    setSearch(e.target.value);
  };

  // 종목코드 필터 변경 핸들러
  const handleSymbolFilterChange = (e) => {
    setSymbolFilter(e.target.value);
  };

  // 정렬 필드 변경 핸들러
  const handleSortChange = (field) => {
    if (sortField === field) {
      // 같은 필드를 다시 클릭하면 정렬 방향 변경
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      // 다른 필드를 클릭하면 해당 필드로 정렬 필드 변경 (기본은 오름차순)
      setSortField(field);
      setSortDirection('asc');
    }
  };

  // 신호 필터 변경 핸들러
  const handleSignalFilterChange = (e) => {
    setFilterSignal(e.target.value);
  };

  // 지표 데이터 필터링 및 정렬
  const filteredAndSortedIndicators = Object.entries(indicators)
    .filter(([symbol, data]) => {
      // 종목코드 또는 종목명으로 검색
      const searchMatch = 
        symbol.toLowerCase().includes(search.toLowerCase()) || 
        (data.stockName && data.stockName.toLowerCase().includes(search.toLowerCase()));
      
      // 종목코드 필터 (특정 종목코드만 보기)
      const symbolMatch = 
        symbolFilter === '' || symbol === symbolFilter;
      
      // 신호 필터 (매수, 매도, 중립)
      const signalMatch = 
        filterSignal === 'all' || 
        (filterSignal === 'buy' && data.signal === '매수') ||
        (filterSignal === 'sell' && data.signal === '매도') ||
        (filterSignal === 'neutral' && data.signal === '중립');
      
      return searchMatch && symbolMatch && signalMatch;
    })
    .sort(([symbolA, dataA], [symbolB, dataB]) => {
      // 정렬 로직
      let comparison = 0;
      
      if (sortField === 'symbol') {
        comparison = symbolA.localeCompare(symbolB);
      } else if (sortField === 'stockName') {
        comparison = (dataA.stockName || '').localeCompare(dataB.stockName || '');
      } else if (sortField === 'percentB') {
        comparison = (dataA.percentB || 0) - (dataB.percentB || 0);
      } else if (sortField === 'bandwidth') {
        comparison = (dataA.bandwidth || 0) - (dataB.bandwidth || 0);
      } else if (sortField === 'currentPrice') {
        comparison = (dataA.currentPrice || 0) - (dataB.currentPrice || 0);
      } else if (sortField === 'signal') {
        comparison = (dataA.signal || '').localeCompare(dataB.signal || '');
      }
      
      // 정렬 방향 적용
      return sortDirection === 'asc' ? comparison : -comparison;
    });

  // 저장할 총 데이터 개수
  const totalCount = filteredAndSortedIndicators.length;

  // 볼린저 밴드 %B에 따른 행 색상 클래스 지정
  const getRowColorClass = (percentB) => {
    if (percentB <= 0.05) return 'table-danger'; // 매수 신호 (강함)
    if (percentB <= 0.2) return 'table-warning'; // 매수 신호 (약함)
    if (percentB >= 0.95) return 'table-danger'; // 매도 신호 (강함)
    if (percentB >= 0.8) return 'table-warning'; // 매도 신호 (약함)
    return '';
  };

  // 신호에 따른 뱃지 색상 클래스 지정
  const getSignalBadgeClass = (signal) => {
    if (signal === '매수') return 'bg-success';
    if (signal === '매도') return 'bg-danger';
    return 'bg-secondary';
  };

  // %B 값을 백분율로 표시
  const formatPercentB = (value) => {
    return (value * 100).toFixed(2) + '%';
  };

  // 정렬 화살표 표시
  const renderSortArrow = (field) => {
    if (sortField !== field) return null;
    return sortDirection === 'asc' ? '↑' : '↓';
  };

  if (loading) {
    return (
      <div className="card">
        <div className="card-header bg-primary text-white">
          <h5 className="mb-0">볼린저 밴드 지표</h5>
        </div>
        <div className="card-body text-center py-5">
          <div className="spinner-border text-primary" role="status">
            <span className="visually-hidden">로딩 중...</span>
          </div>
          <p className="mt-3">볼린저 밴드 지표 데이터를 불러오는 중입니다...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="card">
        <div className="card-header bg-primary text-white">
          <h5 className="mb-0">볼린저 밴드 지표</h5>
        </div>
        <div className="card-body text-center py-5">
          <div className="alert alert-danger">
            <i className="bi bi-exclamation-triangle-fill me-2"></i>
            {error}
          </div>
          <p>볼린저 밴드 지표를 가져오는 중 오류가 발생했습니다. 나중에 다시 시도해주세요.</p>
        </div>
      </div>
    );
  }

  return (
    <div className="card">
      <div className="card-header bg-primary text-white d-flex justify-content-between align-items-center">
        <h5 className="mb-0">볼린저 밴드 지표</h5>
        <div>
          {botEmail && <span className="badge bg-light text-dark me-2">{botEmail}</span>}
          <span className="badge bg-light text-dark">{totalCount}개 종목</span>
        </div>
      </div>
      <div className="card-body">
        <div className="row mb-3">
          <div className="col-md-4">
            <input
              type="text"
              className="form-control"
              placeholder="종목코드 또는 종목명 검색..."
              value={search}
              onChange={handleSearchChange}
            />
          </div>
          <div className="col-md-3">
            <input
              type="text"
              className="form-control"
              placeholder="특정 종목코드 입력"
              value={symbolFilter}
              onChange={handleSymbolFilterChange}
            />
          </div>
          <div className="col-md-3">
            <select
              className="form-select"
              value={filterSignal}
              onChange={handleSignalFilterChange}
            >
              <option value="all">모든 신호</option>
              <option value="buy">매수 신호</option>
              <option value="sell">매도 신호</option>
              <option value="neutral">중립 신호</option>
            </select>
          </div>
          <div className="col-md-2">
            <button 
              className="btn btn-outline-secondary w-100"
              onClick={() => {
                setSearch('');
                setSymbolFilter('');
                setFilterSignal('all');
              }}
            >
              필터 초기화
            </button>
          </div>
        </div>
        
        <div className="table-responsive">
          <table className="table table-striped table-bordered table-hover">
            <thead className="table-light">
              <tr>
                <th onClick={() => handleSortChange('symbol')} style={{ cursor: 'pointer' }}>
                  종목코드 {renderSortArrow('symbol')}
                </th>
                <th onClick={() => handleSortChange('stockName')} style={{ cursor: 'pointer' }}>
                  종목명 {renderSortArrow('stockName')}
                </th>
                <th onClick={() => handleSortChange('currentPrice')} style={{ cursor: 'pointer' }}>
                  현재가 {renderSortArrow('currentPrice')}
                </th>
                <th onClick={() => handleSortChange('percentB')} style={{ cursor: 'pointer' }}>
                  %B {renderSortArrow('percentB')}
                </th>
                <th onClick={() => handleSortChange('bandwidth')} style={{ cursor: 'pointer' }}>
                  밴드폭 {renderSortArrow('bandwidth')}
                </th>
                <th>밴드 정보</th>
                <th onClick={() => handleSortChange('signal')} style={{ cursor: 'pointer' }}>
                  신호 {renderSortArrow('signal')}
                </th>
              </tr>
            </thead>
            <tbody>
              {filteredAndSortedIndicators.length === 0 ? (
                <tr>
                  <td colSpan="7" className="text-center py-3">
                    표시할 데이터가 없거나 필터 조건에 맞는 데이터가 없습니다.
                  </td>
                </tr>
              ) : (
                filteredAndSortedIndicators.map(([symbol, data]) => (
                  <tr key={symbol} className={getRowColorClass(data.percentB || 0.5)}>
                    <td>{symbol}</td>
                    <td>{data.stockName || '-'}</td>
                    <td className="text-end">{data.currentPrice?.toLocaleString() || '-'}</td>
                    <td className="text-end">{formatPercentB(data.percentB || 0)}</td>
                    <td className="text-end">{(data.bandwidth || 0).toFixed(4)}</td>
                    <td className="text-end">
                      <small>
                        상한: {data.upperBand?.toLocaleString() || '-'}<br />
                        중앙: {data.middleBand?.toLocaleString() || '-'}<br />
                        하한: {data.lowerBand?.toLocaleString() || '-'}
                      </small>
                    </td>
                    <td>
                      <span className={`badge ${getSignalBadgeClass(data.signal)}`}>
                        {data.signal || '중립'}
                      </span>
                    </td>
                  </tr>
                ))
              )}
            </tbody>
          </table>
        </div>
        
        <div className="mt-3">
          <h6>볼린저 밴드 지표 설명:</h6>
          <p className="small">
            <strong>%B:</strong> 가격이 밴드 내에서 위치한 상대적 위치를 나타냅니다. 0은 하한선, 1은 상한선을 의미합니다.<br />
            <strong>밴드폭:</strong> 변동성을 나타내는 지표로, 상한선과 하한선의 차이를 중앙선으로 나눈 값입니다.<br />
            <strong>신호:</strong> %B 값에 따라 다음과 같이 매매 신호가 생성됩니다:<br />
            - <span className="badge bg-success">매수</span>: %B가 0.1 이하일 때 (과매도 상태)<br />
            - <span className="badge bg-danger">매도</span>: %B가 0.9 이상일 때 (과매수 상태)<br />
            - <span className="badge bg-secondary">중립</span>: 그 외 정상 범위 내에 있을 때
          </p>
        </div>
      </div>
      <div className="card-footer text-muted text-end">
        마지막 업데이트: {new Date().toLocaleString()}
      </div>
    </div>
  );
}

export default BollingerIndicators;