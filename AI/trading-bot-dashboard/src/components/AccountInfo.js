import React from 'react';

const AccountInfo = ({ accountInfo }) => {
  return (
    <div className="card mb-4">
      <div className="card-header bg-primary text-white">
        <h5 className="mb-0">계좌 정보</h5>
      </div>
      <div className="card-body">
        <div className="row">
          <div className="col-md-6">
            <p><strong>계좌 잔고:</strong> {accountInfo.cash_balance?.toLocaleString()} 원</p>
          </div>
          <div className="col-md-6">
            <p><strong>총 자산 가치:</strong> {accountInfo.total_asset_value?.toLocaleString()} 원</p>
          </div>
        </div>
        <hr />
        <div className="row">
          <div className="col-md-12">
            <p><strong>수익률:</strong> {
              accountInfo.total_asset_value && accountInfo.initial_balance ? 
              ((accountInfo.total_asset_value / accountInfo.initial_balance - 1) * 100).toFixed(2) + '%' : 
              '계산 불가'
            }</p>
          </div>
        </div>
      </div>
    </div>
  );
};

export default AccountInfo;