import React from 'react';

const Positions = ({ positions }) => {
  return (
    <div className="card mb-4">
      <div className="card-header bg-primary text-white">
        <h5 className="mb-0">현재 포지션</h5>
      </div>
      <div className="card-body">
        {positions && positions.length > 0 ? (
          <div className="table-responsive">
            <table className="table table-striped table-bordered">
              <thead className="table-light">
                <tr>
                  <th>종목코드</th>
                  <th>종목명</th>
                  <th>수량</th>
                  <th>매입단가</th>
                  <th>현재가</th>
                  <th>손익</th>
                  <th>손익률</th>
                </tr>
              </thead>
              <tbody>
                {positions.map((position, index) => {
                  const profit = (position.current_price - position.average_price) * position.quantity;
                  const profitPercent = ((position.current_price / position.average_price) - 1) * 100;
                  
                  return (
                    <tr key={index}>
                      <td>{position.symbol}</td>
                      <td>{position.name || '-'}</td>
                      <td className="text-end">{position.quantity.toLocaleString()}</td>
                      <td className="text-end">{position.average_price?.toLocaleString()}</td>
                      <td className="text-end">{position.current_price?.toLocaleString()}</td>
                      <td className={`text-end ${profit > 0 ? 'text-success' : profit < 0 ? 'text-danger' : ''}`}>
                        {profit.toLocaleString()}
                      </td>
                      <td className={`text-end ${profitPercent > 0 ? 'text-success' : profitPercent < 0 ? 'text-danger' : ''}`}>
                        {profitPercent.toFixed(2)}%
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        ) : (
          <div className="alert alert-info">현재 보유 중인 포지션이 없습니다.</div>
        )}
      </div>
    </div>
  );
};

export default Positions;