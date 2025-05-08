import React, { useState, useEffect } from 'react';
import { Table, Form, Button, Card, Spinner, Alert, Pagination } from 'react-bootstrap';

// Envelope 지표 모니터링 컴포넌트
const EnvelopeIndicators = ({ apiBaseUrl }) => {
  const [indicators, setIndicators] = useState({});
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [itemsPerPage] = useState(15); // 페이지당 항목 수
  const [sortBy, setSortBy] = useState('symbol');
  const [sortDirection, setSortDirection] = useState('asc');
  const [selectedSymbols, setSelectedSymbols] = useState([]);
  const [stockInfoMap, setStockInfoMap] = useState({});  // 종목코드 -> 종목명 매핑

  // 종목 정보 로드
  const loadStockInfo = async () => {
    try {
      const response = await fetch(`${apiBaseUrl}/symbols`);
      
      if (!response.ok) {
        throw new Error(`종목 정보 API 요청 실패: ${response.status}`);
      }
      
      const data = await response.json();
      
      // 종목 정보를 매핑 형태로 저장
      const stockMap = {};
      
      // KOSPI 종목 처리
      if (data.kospi) {
        data.kospi.forEach(symbol => {
          if (symbol.code && symbol.name) {
            stockMap[symbol.code] = symbol.name;
          }
        });
      }
      
      // KOSDAQ 종목 처리
      if (data.kosdaq) {
        data.kosdaq.forEach(symbol => {
          if (symbol.code && symbol.name) {
            stockMap[symbol.code] = symbol.name;
          }
        });
      }
      
      setStockInfoMap(stockMap);
    } catch (err) {
      console.error('종목 정보 로드 오류:', err);
      // 오류가 발생해도 진행 (종목명이 없어도 코드로 표시 가능)
    }
  };

  // 지표 데이터 로드
  const loadIndicators = async () => {
    try {
      setIsLoading(true);
      setError(null);

      let url = `${apiBaseUrl}/indicators`;
      
      // 특정 종목코드가 선택된 경우
      if (selectedSymbols.length > 0) {
        url += `?symbols=${selectedSymbols.join(',')}`;
      }

      const response = await fetch(url);
      
      if (!response.ok) {
        throw new Error(`API 요청 실패: ${response.status}`);
      }
      
      const data = await response.json();
      
      // 응답 형식에 따라 적절히 처리
      if (data.indicators) {
        setIndicators(data.indicators);
      } else {
        setIndicators(data);
      }
      
      console.log('Loaded indicators data:', data); // 디버깅용 로그 추가
    } catch (err) {
      setError(err.message);
      console.error('지표 데이터 로드 오류:', err);
    } finally {
      setIsLoading(false);
    }
  };

  // 컴포넌트 마운트 시 종목 정보와 지표 데이터 로드
  useEffect(() => {
    loadStockInfo();
    loadIndicators();
    
    // 30초마다 데이터 새로고침
    const interval = setInterval(() => {
      loadIndicators();
    }, 30000);
    
    return () => clearInterval(interval);
  }, [apiBaseUrl, selectedSymbols]);

  // 데이터를 표시 가능한 배열로 변환 (수정된 부분)
  const getIndicatorArray = () => {
    return Object.entries(indicators).map(([symbol, data]) => ({
      symbol,
      shortName: data.stockName || stockInfoMap[symbol] || '(이름 없음)', // API에서 제공하는 stockName 우선 사용
      ...data
    }));
  };

  // 검색 필터링 (종목코드 또는 종목명으로 검색 가능)
  const filteredData = getIndicatorArray().filter(item => 
    item.symbol.toLowerCase().includes(searchTerm.toLowerCase()) ||
    item.shortName.toLowerCase().includes(searchTerm.toLowerCase())
  );

  // 정렬 함수
  const sortedData = [...filteredData].sort((a, b) => {
    let comparison = 0;
    
    switch (sortBy) {
      case 'symbol':
        comparison = a.symbol.localeCompare(b.symbol);
        break;
      case 'shortName':
        comparison = a.shortName.localeCompare(b.shortName);
        break;
      case 'MA20':
      case 'upperBand':
      case 'lowerBand':
      case 'currentPrice':
        comparison = a[sortBy] - b[sortBy];
        break;
      default:
        comparison = 0;
    }
    
    return sortDirection === 'asc' ? comparison : -comparison;
  });

  // 페이지네이션
  const indexOfLastItem = currentPage * itemsPerPage;
  const indexOfFirstItem = indexOfLastItem - itemsPerPage;
  const currentItems = sortedData.slice(indexOfFirstItem, indexOfLastItem);
  const totalPages = Math.ceil(sortedData.length / itemsPerPage);

  // 정렬 변경 핸들러
  const handleSort = (column) => {
    if (sortBy === column) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortBy(column);
      setSortDirection('asc');
    }
  };

  // 종목 검색 및 추가 핸들러
  const handleAddSymbol = (e) => {
    e.preventDefault();
    if (searchTerm && !selectedSymbols.includes(searchTerm)) {
      setSelectedSymbols([...selectedSymbols, searchTerm]);
      setSearchTerm('');
    }
  };

  // 선택된 종목 제거 핸들러
  const handleRemoveSymbol = (symbol) => {
    setSelectedSymbols(selectedSymbols.filter(s => s !== symbol));
  };

  // 모든 종목 표시로 전환
  const handleShowAll = () => {
    setSelectedSymbols([]);
  };

  // 매수/매도 신호 확인 (현재가와 Envelope 밴드 비교)
  const getSignal = (item) => {
    const currentPrice = item.currentPrice || 0;
    const upperBand = item.upperBand || 0;
    const lowerBand = item.lowerBand || 0;
    
    if (currentPrice >= upperBand) {
      return { type: 'sell', text: '매도', className: 'text-danger' };
    } else if (currentPrice <= lowerBand) {
      return { type: 'buy', text: '매수', className: 'text-success' };
    }
    return { type: 'neutral', text: '중립', className: 'text-secondary' };
  };

  // Envelope 지표 진행률 표시
  const getPercentInEnvelope = (item) => {
    const currentPrice = item.currentPrice || 0;
    const upperBand = item.upperBand || 0;
    const lowerBand = item.lowerBand || 0;
    
    if (currentPrice <= lowerBand) return 0;
    if (currentPrice >= upperBand) return 100;
    
    return ((currentPrice - lowerBand) / (upperBand - lowerBand)) * 100 || 0;
  };

  // 페이지 변경 핸들러
  const handlePageChange = (pageNumber) => {
    setCurrentPage(pageNumber);
  };

  // 페이지네이션 컴포넌트
  const renderPagination = () => {
    if (totalPages <= 1) return null;
    
    const pageItems = [];
    
    // 처음 페이지로
    pageItems.push(
      <Pagination.First 
        key="first" 
        onClick={() => handlePageChange(1)} 
        disabled={currentPage === 1} 
      />
    );
    
    // 이전 페이지로
    pageItems.push(
      <Pagination.Prev 
        key="prev" 
        onClick={() => handlePageChange(currentPage - 1)} 
        disabled={currentPage === 1} 
      />
    );
    
    // 페이지 번호
    for (let number = Math.max(1, currentPage - 2); number <= Math.min(totalPages, currentPage + 2); number++) {
      pageItems.push(
        <Pagination.Item 
          key={number} 
          active={number === currentPage}
          onClick={() => handlePageChange(number)}
        >
          {number}
        </Pagination.Item>
      );
    }
    
    // 다음 페이지로
    pageItems.push(
      <Pagination.Next 
        key="next" 
        onClick={() => handlePageChange(currentPage + 1)} 
        disabled={currentPage === totalPages} 
      />
    );
    
    // 마지막 페이지로
    pageItems.push(
      <Pagination.Last 
        key="last" 
        onClick={() => handlePageChange(totalPages)} 
        disabled={currentPage === totalPages} 
      />
    );
    
    return <Pagination>{pageItems}</Pagination>;
  };

  return (
    <Card className="mb-4">
      <Card.Header className="d-flex justify-content-between align-items-center">
        <h5 className="mb-0">Envelope 지표 모니터링</h5>
        <div>
          <Button 
            variant="outline-primary" 
            size="sm"
            onClick={loadIndicators}
            disabled={isLoading}
            className="me-2"
          >
            {isLoading ? (
              <>
                <Spinner as="span" animation="border" size="sm" role="status" aria-hidden="true" />
                <span className="ms-1">로딩 중...</span>
              </>
            ) : '새로고침'}
          </Button>
          <Button
            variant="outline-secondary"
            size="sm"
            onClick={handleShowAll}
            disabled={selectedSymbols.length === 0}
          >
            전체 종목 보기
          </Button>
        </div>
      </Card.Header>
      <Card.Body>
        {error && (
          <Alert variant="danger" onClose={() => setError(null)} dismissible>
            {error}
          </Alert>
        )}
        
        {/* 종목 검색 및 추가 */}
        <Form onSubmit={handleAddSymbol} className="mb-3">
          <div className="d-flex">
            <Form.Control
              type="text"
              placeholder="종목코드 또는 종목명 검색..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="me-2"
            />
            <Button type="submit" variant="primary" disabled={!searchTerm}>
              종목 추가
            </Button>
          </div>
        </Form>
        
        {/* 선택된 종목 태그 */}
        {selectedSymbols.length > 0 && (
          <div className="mb-3">
            <strong>선택된 종목:</strong>
            <div className="d-flex flex-wrap mt-2">
              {selectedSymbols.map(symbol => (
                <span 
                  key={symbol} 
                  className="badge bg-primary me-2 mb-2 d-flex align-items-center"
                >
                  {symbol} {stockInfoMap[symbol] ? `(${stockInfoMap[symbol]})` : ''}
                  <button 
                    type="button" 
                    className="btn-close btn-close-white ms-2" 
                    style={{ fontSize: '0.5rem' }}
                    onClick={() => handleRemoveSymbol(symbol)}
                    aria-label="Remove"
                  ></button>
                </span>
              ))}
            </div>
          </div>
        )}
        
        {/* 지표 테이블 */}
        {isLoading && Object.keys(indicators).length === 0 ? (
          <div className="text-center my-5">
            <Spinner animation="border" variant="primary" />
            <p className="mt-2">지표 데이터를 로드하는 중...</p>
          </div>
        ) : (
          <>
            {currentItems.length === 0 ? (
              <div className="text-center my-3">
                <p>표시할 지표 데이터가 없습니다.</p>
              </div>
            ) : (
              <>
                <div className="table-responsive">
                  <Table striped bordered hover>
                    <thead>
                      <tr>
                        <th 
                          onClick={() => handleSort('symbol')}
                          className="sortable-header"
                        >
                          종목코드
                          {sortBy === 'symbol' && (
                            <span className="ms-1">
                              {sortDirection === 'asc' ? '▲' : '▼'}
                            </span>
                          )}
                        </th>
                        <th 
                          onClick={() => handleSort('name')}
                          className="sortable-header"
                        >
                          종목명
                          {sortBy === 'name' && (
                            <span className="ms-1">
                              {sortDirection === 'asc' ? '▲' : '▼'}
                            </span>
                          )}
                        </th>
                        <th 
                          onClick={() => handleSort('MA20')}
                          className="sortable-header"
                        >
                          MA20
                          {sortBy === 'MA20' && (
                            <span className="ms-1">
                              {sortDirection === 'asc' ? '▲' : '▼'}
                            </span>
                          )}
                        </th>
                        <th 
                          onClick={() => handleSort('lowerBand')}
                          className="sortable-header"
                        >
                          하한선
                          {sortBy === 'lowerBand' && (
                            <span className="ms-1">
                              {sortDirection === 'asc' ? '▲' : '▼'}
                            </span>
                          )}
                        </th>
                        <th 
                          onClick={() => handleSort('upperBand')}
                          className="sortable-header"
                        >
                          상한선
                          {sortBy === 'upperBand' && (
                            <span className="ms-1">
                              {sortDirection === 'asc' ? '▲' : '▼'}
                            </span>
                          )}
                        </th>
                        <th 
                          onClick={() => handleSort('currentPrice')}
                          className="sortable-header"
                        >
                          현재가
                          {sortBy === 'currentPrice' && (
                            <span className="ms-1">
                              {sortDirection === 'asc' ? '▲' : '▼'}
                            </span>
                          )}
                        </th>
                        <th>현재 위치</th>
                        <th>신호</th>
                      </tr>
                    </thead>
                    <tbody>
                      {currentItems.map(item => {
                        const signal = getSignal(item);
                        const percentInEnvelope = getPercentInEnvelope(item);
                        
                        return (
                          <tr key={item.symbol}>
                            <td>{item.symbol}</td>
                            <td>{item.shortName}</td>
                            <td>{item.MA20?.toLocaleString(undefined, { maximumFractionDigits: 2 })}</td>
                            <td>{item.lowerBand?.toLocaleString(undefined, { maximumFractionDigits: 2 })}</td>
                            <td>{item.upperBand?.toLocaleString(undefined, { maximumFractionDigits: 2 })}</td>
                            <td className={
                              item.currentPrice > item.MA20 ? 'text-success' : 
                              item.currentPrice < item.MA20 ? 'text-danger' : ''
                            }>
                              {item.currentPrice?.toLocaleString(undefined, { maximumFractionDigits: 2 })}
                            </td>
                            <td>
                              <div className="progress" style={{ height: '20px' }}>
                                <div 
                                  className={`progress-bar ${
                                    percentInEnvelope < 50 ? 'bg-danger' : 
                                    percentInEnvelope > 50 ? 'bg-success' : 'bg-warning'
                                  }`}
                                  role="progressbar" 
                                  style={{ width: `${percentInEnvelope}%` }}
                                  aria-valuenow={percentInEnvelope} 
                                  aria-valuemin="0" 
                                  aria-valuemax="100"
                                >
                                  {percentInEnvelope.toFixed(0)}%
                                </div>
                              </div>
                            </td>
                            <td>
                              <span className={`badge ${
                                signal.type === 'buy' ? 'bg-success' : 
                                signal.type === 'sell' ? 'bg-danger' : 'bg-secondary'
                              }`}>
                                {signal.text}
                              </span>
                            </td>
                          </tr>
                        );
                      })}
                    </tbody>
                  </Table>
                </div>
                
                {/* 페이지네이션 */}
                <div className="d-flex justify-content-between align-items-center">
                  <div>
                    전체 {sortedData.length}개 중 {indexOfFirstItem + 1}-{Math.min(indexOfLastItem, sortedData.length)}
                  </div>
                  {renderPagination()}
                </div>
              </>
            )}
          </>
        )}
      </Card.Body>
    </Card>
  );
};

export default EnvelopeIndicators;