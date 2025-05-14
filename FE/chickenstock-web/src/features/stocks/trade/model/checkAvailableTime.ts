const checkAvailableTime = (isNxt: boolean, isLimitOrder: boolean) => {
  const now = new Date();
  const hour = now.getHours();
  const minute = now.getMinutes();
  const second = now.getSeconds();

  console.log(hour, minute, second);

  // 지정가 거래만 가능한 시간대
  // 1. 오전 8시 50분 ~ 9시
  if (hour === 8 && minute >= 50) {
    return { available: isLimitOrder, message: "시가 단일가 거래 시간입니다." };
  }
  // 2. 오후 3시 20분 ~ 3시 30분
  if (hour === 15 && minute >= 20 && minute < 30) {
    return { available: isLimitOrder, message: "종가 단일가 거래 시간입니다." };
  }

  // 오전 9시 00분 30초~오후 3시 20분: KRX, NXT 모두 거래 가능
  if (
    (hour === 9 && (minute > 0 || (minute === 0 && second >= 30))) ||
    (hour > 9 && hour < 15) ||
    (hour === 15 && minute <= 20)
  ) {
    return { available: true, message: "정규 거래 시간입니다." };
  }

  if (isNxt) {
    // NXT 종목 거래 가능 시간
    // 1. 오전 8시~8시50분
    if (hour === 8 && minute < 50) {
      return { available: true, message: "NXT 프리마켓 시간입니다." };
    }
    // 2. 오후 3시 30분~오후 8시
    if ((hour === 15 && minute >= 30) || (hour > 15 && hour < 20)) {
      return { available: true, message: "NXT 애프터마켓 시간입니다." };
    }
    return { available: false, message: "정규 거래가 마감되었습니다." };
  } else {
    if (hour === 8 && minute < 50 && minute >= 30) {
      return { available: isLimitOrder, message: "KRX 시가 단일가 거래 시간입니다." };
    }
    // 일반 종목 거래 가능 시간: 오전 9시~오후 3시 30분
    if (hour < 9 || hour > 15 || (hour === 15 && minute >= 30)) {
      return { available: false, message: "정규 거래가 마감되었습니다." };
    }
    return { available: true, message: "정규 거래 시간입니다." };
  }
};

export default checkAvailableTime;
