import React from 'react';

const RecentTrades = ({ trades }) => {
  // API 응답이 없거나 배열이 아닌 경우 빈 배열로 처리
  const tradeItems = Array.isArray(trades) ? trades : [];
  
  // 날짜 형식 변환 함수
  const formatDate = (dateString) => {
    if (!dateString) return '';
    const date = new Date(dateString);
    return date.toLocaleString('ko-KR');
  };

  return (
    <div className="card h-100">
      <div className="card-header">
        <h5 className="card-title mb-0">최근 거래 내역</h5>
      </div>
      <div className="card-body">
        {tradeItems.length === 0 ? (
          <p className="text-center text-muted">거래 내역이 없습니다.</p>
        ) : (
          <div className="table-responsive">
            <table className="table table-sm table-striped">
              <thead>
                <tr>
                  <th>종목명</th>
                  <th>거래 유형</th>
                  <th>수량</th>
                  <th>단가</th>
                  <th>거래 시간</th>
                </tr>
              </thead>
              <tbody>
                {tradeItems.map((trade, index) => (
                  <tr key={`${trade.createdAt}-${index}`}>
                    <td>{trade.stockName}</td>
                    <td>
                      <span className={trade.tradeType === 'BUY' ? 'text-danger' : 'text-primary'}>
                        {trade.tradeType === 'BUY' ? '매수' : '매도'}
                      </span>
                    </td>
                    <td>{trade.quantity}</td>
                    <td>{trade.unitPrice.toLocaleString()}원</td>
                    <td>{formatDate(trade.tradedAt)}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>
    </div>
  );
};

export default RecentTrades;