import React from 'react';
import { formatCurrency } from '../utils/formatters';

function AccountInfo({ accountInfo }) {
  return (
    <div className="card account-card">
      <div className="card-header bg-primary text-white">
        <h5 className="mb-0">
          <i className="fas fa-wallet me-2"></i>계좌 정보
        </h5>
      </div>
      <div className="card-body">
        <div className="account-info">
          <div className="info-item">
            <span>예수금:</span>
            <span id="cashBalance" className="fw-bold">
              {formatCurrency(accountInfo.cash_balance)}
            </span>
          </div>
          <div className="info-item">
            <span>총 자산가치:</span>
            <span id="totalAssetValue" className="fw-bold">
              {formatCurrency(accountInfo.total_asset_value)}
            </span>
          </div>
          <div className="info-item">
            <span>보유 종목 수:</span>
            <span id="positionsCount" className="fw-bold">
              {accountInfo.positions ? Object.keys(accountInfo.positions).length : '-'}
            </span>
          </div>
        </div>
      </div>
    </div>
  );
}

export default AccountInfo;