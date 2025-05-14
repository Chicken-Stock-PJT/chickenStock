import React, { useState, useEffect } from 'react';

const PriceMonitor = ({ apiBaseUrl }) => {
  const [prices, setPrices] = useState({});
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [searchTerm, setSearchTerm] = useState('');

  useEffect(() => {
    let isMounted = true;
    const fetchPrices = async () => {
      try {
        setLoading(true);
        
        const response = await fetch(`${apiBaseUrl}/prices`, {
          headers: {
            'Content-Type': 'application/json'
          }
        });
        
        if (!response.ok) {
          throw new Error('서버에서 데이터를 가져오는데 실패했습니다.');
        }
        
        const data = await response.json();
        
        if (isMounted) {
          setPrices(data);
          setError(null);
        }
      } catch (err) {
        if (isMounted) {
          setError('실시간 시세 데이터를 불러오는데 실패했습니다.');
          console.error('Error fetching prices:', err);
        }
      } finally {
        if (isMounted) {
          setLoading(false);
        }
      }
    };

    fetchPrices();
    
    // 3초마다 데이터 갱신
    const interval = setInterval(fetchPrices, 3000);
    
    return () => {
      isMounted = false;
      clearInterval(interval);
    };
  }, [apiBaseUrl]);

  // 검색어에 따라 종목 필터링
  const filteredSymbols = Object.keys(prices).filter(symbol => 
    symbol.includes(searchTerm.toUpperCase()) || 
    (prices[symbol].name && prices[symbol].name.includes(searchTerm))
  );

  return (
    <div className="card mb-4">
      <div className="card-header bg-primary text-white">
        <h5 className="mb-0">실시간 시세 모니터</h5>
      </div>
      <div className="card-body">
        <div className="mb-3">
          <input
            type="text"
            className="form-control"
            placeholder="종목코드 또는 이름으로 검색"
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
          />
        </div>
        
        {loading && filteredSymbols.length === 0 ? (
          <div className="text-center my-3">
            <div className="spinner-border text-primary" role="status">
              <span className="visually-hidden">로딩 중...</span>
            </div>
          </div>
        ) : error ? (
          <div className="alert alert-danger">{error}</div>
        ) : filteredSymbols.length === 0 ? (
          <div className="alert alert-info">표시할 종목이 없습니다.</div>
        ) : (
          <div className="table-responsive">
            <table className="table table-striped table-bordered">
              <thead className="table-light">
                <tr>
                  <th>종목코드</th>
                  <th>종목명</th>
                  <th>현재가</th>
                  <th>전일대비</th>
                  <th>등락률</th>
                </tr>
              </thead>
              <tbody>
                {filteredSymbols.map(symbol => {
                  const item = prices[symbol];
                  const priceChange = item.change || 0;
                  const changePercent = item.changePercent || 0;
                  
                  return (
                    <tr key={symbol}>
                      <td>{symbol}</td>
                      <td>{item.name || '-'}</td>
                      <td className="text-end">{item.price?.toLocaleString() || '-'}</td>
                      <td className={`text-end ${priceChange > 0 ? 'text-danger' : priceChange < 0 ? 'text-primary' : ''}`}>
                        {priceChange > 0 ? '+' : ''}{priceChange.toLocaleString()}
                      </td>
                      <td className={`text-end ${changePercent > 0 ? 'text-danger' : changePercent < 0 ? 'text-primary' : ''}`}>
                        {changePercent > 0 ? '+' : ''}{changePercent.toFixed(2)}%
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        )}
      </div>
    </div>
  );
};

export default PriceMonitor;