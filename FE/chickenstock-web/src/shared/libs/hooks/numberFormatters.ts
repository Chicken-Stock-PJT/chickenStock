/**
 * 거래량을 가독성 있게 포맷팅 (예: 1.2M, 3.5K)
 */
export function formatVolume(value: number): string {
  if (value >= 1000000) {
    return (value / 1000000).toFixed(1) + "M";
  } else if (value >= 1000) {
    return (value / 1000).toFixed(1) + "K";
  }
  return value.toLocaleString();
}
