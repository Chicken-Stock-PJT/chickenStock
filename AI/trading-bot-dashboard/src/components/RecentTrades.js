import React from 'react';
import { formatCurrency, formatNumber } from '../utils/formatters';

function RecentTrades({ trades }) {
  // 최신 거래 순으로 정렬
  const sortedTrades = [...(trades || [])].sort((a, b) => {
    return new Date(b.timestamp) - new Date(a.timestamp);
  });
  
  // 최대 10개만 표시
  const recentTrades = sortedTrades.slice(0, 10);
  
  return (
    <div className="card recent-trades-card">
      <div className="card-header bg-success text-white">
        <h5 className="mb-0">
          <i className="fas fa-exchange-alt me-2"></i>최근 거래
        </h5>
      </div>
      <div className="card-body p-0">
        <div className="table-responsive">
          <table className="table table-striped mb-0">
            <thead>
              <tr>
                <th>시간</th>
                <th>종목</th>
                <th>거래</th>
                <th>가격</th>
                <th>수량</th>
                <th>신뢰도</th>
              </tr>
            </thead>
            <tbody>
              {recentTrades.length > 0 ? (
                recentTrades.map((trade, index) => {
                  const tradeTime = new Date(trade.timestamp).toLocaleTimeString();
                  const actionClass = trade.action === 'BUY' ? 'trade-buy' : 'trade-sell';
                  const actionText = trade.action === 'BUY' ? '매수' : '매도';
                  
                  return (
                    <tr key={index}>
                      <td>{tradeTime}</td>
                      <td>{trade.name || trade.symbol}</td>
                      <td className={actionClass}>{actionText}</td>
                      <td className="text-end">{formatCurrency(trade.price)}</td>
                      <td className="text-end">{formatNumber(trade.quantity)}</td>
                      <td className="text-end">{(trade.confidence * 100).toFixed(0)}%</td>
                    </tr>
                  );
                })
              ) : (
                <tr>
                  <td colSpan="6" className="text-center">거래 내역이 없습니다.</td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}

export default RecentTrades;