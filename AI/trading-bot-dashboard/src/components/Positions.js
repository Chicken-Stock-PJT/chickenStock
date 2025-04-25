import React from 'react';
import { formatCurrency, formatNumber } from '../utils/formatters';

function Positions({ positions }) {
  // 객체를 배열로 변환
  const positionsArray = Object.entries(positions || {}).map(([code, data]) => ({
    code,
    ...data
  }));
  
  // 평가손익 기준으로 내림차순 정렬
  positionsArray.sort((a, b) => b.eval_profit_loss - a.eval_profit_loss);
  
  return (
    <div className="card positions-card">
      <div className="card-header bg-info text-white">
        <h5 className="mb-0">
          <i className="fas fa-chart-line me-2"></i>보유 종목
        </h5>
      </div>
      <div className="card-body p-0">
        <div className="table-responsive">
          <table className="table table-striped mb-0">
            <thead>
              <tr>
                <th>종목코드</th>
                <th>종목명</th>
                <th>보유수량</th>
                <th>매입가</th>
                <th>현재가</th>
                <th>평가손익</th>
                <th>수익률</th>
              </tr>
            </thead>
            <tbody>
              {positionsArray.length > 0 ? (
                positionsArray.map((position, index) => {
                  const isProfitable = position.earning_rate >= 0;
                  
                  return (
                    <tr key={position.code}>
                      <td>{position.code}</td>
                      <td>{position.name}</td>
                      <td className="text-end">{formatNumber(position.quantity)}</td>
                      <td className="text-end">{formatCurrency(position.purchase_price)}</td>
                      <td className="text-end">{formatCurrency(position.current_price)}</td>
                      <td className={`text-end ${isProfitable ? 'profit-positive' : 'profit-negative'}`}>
                        {formatCurrency(position.eval_profit_loss)}
                      </td>
                      <td className={`text-end ${isProfitable ? 'profit-positive' : 'profit-negative'}`}>
                        {position.earning_rate.toFixed(2)}%
                      </td>
                    </tr>
                  );
                })
              ) : (
                <tr>
                  <td colSpan="7" className="text-center">보유 중인 종목이 없습니다.</td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}

export default Positions;