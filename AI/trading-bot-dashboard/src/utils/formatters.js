// 통화 형식 포맷팅 함수
export const formatCurrency = (value) => {
  if (value === undefined || value === null) return '-';
  
  return new Intl.NumberFormat('ko-KR', {
    style: 'decimal',
    maximumFractionDigits: 0
  }).format(value) + '원';
};

// 숫자 형식 포맷팅 함수
export const formatNumber = (value) => {
  if (value === undefined || value === null) return '-';
  
  return new Intl.NumberFormat('ko-KR').format(value);
};