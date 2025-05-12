import React, { useState, useEffect } from 'react';
import { Table, Form, InputGroup, Button, Card, Spinner, Badge } from 'react-bootstrap';

const PriceMonitor = ({ apiBaseUrl }) => {
  const [prices, setPrices] = useState({});
  const [filteredPrices, setFilteredPrices] = useState({});
  const [isLoading, setIsLoading] = useState(false);
  const [searchTerm, setSearchTerm] = useState('');
  const [priceCount, setPriceCount] = useState(0);
  const [lastUpdated, setLastUpdated] = useState(null);
  const [autoRefresh, setAutoRefresh] = useState(true);
  const [refreshInterval, setRefreshInterval] = useState(5); // 5초 기본값
  const [updatedItems, setUpdatedItems] = useState([]); // 최근 업데이트된 종목 추적

  // 가격 정보 로드 함수
  const loadPrices = async () => {
    setIsLoading(true);
    try {
      const response = await fetch(`${apiBaseUrl}/prices`);
      
      if (response.ok) {
        const data = await response.json();
        
        // 이전 가격과 새 가격을 비교하여 업데이트된 항목 식별
        const updatedCodes = [];
        Object.entries(data).forEach(([code, price]) => {
          const prevPrice = prices[code];
          if (prevPrice !== price) {
            // 새로 업데이트된 종목이면 목록에 추가
            updatedCodes.push({
              code,
              price,
              updatedAt: new Date()
            });
          }
        });
        
        // 최근 업데이트된 종목 목록 업데이트 (최대 100개 유지)
        setUpdatedItems(prevItems => {
          // 새 항목 추가
          const newItems = [...updatedCodes, ...prevItems];
          // 중복 제거 (같은 코드는 최신 것만 유지)
          const uniqueItems = newItems.reduce((acc, item) => {
            const existing = acc.find(i => i.code === item.code);
            if (!existing) {
              acc.push(item);
            } else if (existing.updatedAt < item.updatedAt) {
              // 기존 항목보다 새로운 항목이면 업데이트
              const index = acc.indexOf(existing);
              acc[index] = item;
            }
            return acc;
          }, []);
          
          // 최대 100개까지만 유지하고 업데이트 시간 기준 내림차순 정렬
          return uniqueItems
            .sort((a, b) => b.updatedAt - a.updatedAt)
            .slice(0, 100);
        });
        
        setPrices(data);
        setPriceCount(Object.keys(data).length);
        setLastUpdated(new Date());
        filterPrices(data, searchTerm);
      }
    } catch (error) {
      console.error('Error loading prices:', error);
    } finally {
      setIsLoading(false);
    }
  };

  // 검색어에 따라 가격 필터링
  const filterPrices = (priceData, term) => {
    if (!term.trim()) {
      // 검색어가 없으면 최근 업데이트된 종목 표시
      const recentPrices = {};
      updatedItems.forEach(item => {
        recentPrices[item.code] = priceData[item.code] || item.price;
      });
      setFilteredPrices(recentPrices);
    } else {
      // 검색어로 필터링
      const filtered = {};
      Object.keys(priceData).forEach(code => {
        if (code.includes(term.toUpperCase())) {
          filtered[code] = priceData[code];
        }
      });
      setFilteredPrices(filtered);
    }
  };

  // 검색어 변경 핸들러
  const handleSearchChange = (e) => {
    const newTerm = e.target.value;
    setSearchTerm(newTerm);
    filterPrices(prices, newTerm);
  };

  // 초기 데이터 로드
  useEffect(() => {
    loadPrices();
  }, [apiBaseUrl]);

  // 최근 업데이트된 종목 변경 시 필터링 다시 적용
  useEffect(() => {
    filterPrices(prices, searchTerm);
  }, [updatedItems]);

  // 자동 새로고침 설정
  useEffect(() => {
    let intervalId = null;
    
    if (autoRefresh) {
      intervalId = setInterval(loadPrices, refreshInterval * 1000);
    }
    
    return () => {
      if (intervalId) {
        clearInterval(intervalId);
      }
    };
  }, [autoRefresh, refreshInterval, apiBaseUrl]);

  // 새로고침 간격 변경 핸들러
  const handleRefreshIntervalChange = (e) => {
    const newInterval = parseInt(e.target.value, 10);
    if (!isNaN(newInterval) && newInterval > 0) {
      setRefreshInterval(newInterval);
    }
  };

  // 자동 새로고침 토글 핸들러
  const toggleAutoRefresh = () => {
    setAutoRefresh(!autoRefresh);
  };

  // 가격 정보 수동 새로고침
  const handleManualRefresh = () => {
    loadPrices();
  };

  // 가격을 콤마가 포함된 형식으로 변환
  const formatPrice = (price) => {
    return price ? price.toLocaleString() : '0';
  };

  // 업데이트 시간 형식화
  const formatUpdateTime = (date) => {
    if (!date) return '';
    
    const hours = date.getHours().toString().padStart(2, '0');
    const minutes = date.getMinutes().toString().padStart(2, '0');
    const seconds = date.getSeconds().toString().padStart(2, '0');
    
    return `${hours}:${minutes}:${seconds}`;
  };

  return (
    <Card>
      <Card.Header className="d-flex justify-content-between align-items-center">
        <div>
          <h5 className="mb-0">실시간 시세 모니터</h5>
          <small className="text-muted">
            {lastUpdated 
              ? `마지막 업데이트: ${lastUpdated.toLocaleTimeString()}` 
              : '데이터 로딩 중...'
            }
          </small>
        </div>
        <div>
          <Badge bg="info" className="me-2">
            총 {priceCount}개 종목
          </Badge>
          {isLoading && (
            <Spinner animation="border" size="sm" role="status">
              <span className="visually-hidden">로딩 중...</span>
            </Spinner>
          )}
        </div>
      </Card.Header>
      <Card.Body>
        <div className="mb-3">
          <Form>
            <InputGroup className="mb-3">
              <Form.Control
                placeholder="종목 코드 검색..."
                value={searchTerm}
                onChange={handleSearchChange}
              />
              <Button variant="outline-secondary" onClick={handleManualRefresh}>
                새로고침
              </Button>
            </InputGroup>
            <div className="d-flex align-items-center mb-3">
              <Form.Check
                type="switch"
                id="autoRefresh"
                label="자동 새로고침"
                checked={autoRefresh}
                onChange={toggleAutoRefresh}
                className="me-3"
              />
              {autoRefresh && (
                <Form.Group className="d-flex align-items-center">
                  <Form.Label className="me-2 mb-0">갱신 간격(초):</Form.Label>
                  <Form.Control
                    type="number"
                    min="1"
                    value={refreshInterval}
                    onChange={handleRefreshIntervalChange}
                    style={{ width: '70px' }}
                  />
                </Form.Group>
              )}
            </div>
          </Form>
        </div>
        
        <div style={{ maxHeight: '500px', overflowY: 'auto' }}>
          <Table striped bordered hover size="sm">
            <thead>
              <tr>
                <th>종목코드</th>
                <th>현재가</th>
                <th>최근 업데이트</th>
              </tr>
            </thead>
            <tbody>
              {Object.keys(filteredPrices).length > 0 ? (
                searchTerm ? (
                  // 검색 모드일 때
                  Object.entries(filteredPrices).map(([code, price]) => (
                    <tr key={code}>
                      <td>{code}</td>
                      <td className="text-end">{formatPrice(price)}</td>
                      <td className="text-center">
                        {formatUpdateTime(updatedItems.find(item => item.code === code)?.updatedAt)}
                      </td>
                    </tr>
                  ))
                ) : (
                  // 최근 업데이트 모드일 때 (검색어 없음)
                  updatedItems.map(item => (
                    <tr key={item.code}>
                      <td>{item.code}</td>
                      <td className="text-end">{formatPrice(prices[item.code] || item.price)}</td>
                      <td className="text-center">{formatUpdateTime(item.updatedAt)}</td>
                    </tr>
                  ))
                )
              ) : (
                <tr>
                  <td colSpan="3" className="text-center">
                    {isLoading ? '데이터 로딩 중...' : '데이터가 없습니다.'}
                  </td>
                </tr>
              )}
            </tbody>
          </Table>
        </div>
      </Card.Body>
      <Card.Footer className="text-muted">
        {searchTerm ? `검색 결과: ${Object.keys(filteredPrices).length}개 종목` : `최근 업데이트: ${updatedItems.length}개 종목`}
      </Card.Footer>
    </Card>
  );
};

export default PriceMonitor;